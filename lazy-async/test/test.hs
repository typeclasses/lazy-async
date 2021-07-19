module Main (main) where

import LazyAsync

import Control.Concurrent (threadDelay)
import Data.Bool          (not)
import Data.Eq            (Eq)
import Data.Foldable      (traverse_)
import Data.Function      (($), (.))
import Data.Maybe         (Maybe (Just), maybe)
import Numeric.Natural    (Natural)
import Prelude            (Integer, ($!), (+))
import System.Exit        (exitFailure)
import System.IO          (IO)
import Text.Show          (Show)

import Control.Exception (ArithException (DivideByZero),
                          Exception (fromException), SomeException, throw)

import Control.Applicative         (liftA2)
import Control.Monad               (Monad (return, (>>=)), when)
import Control.Monad.IO.Class      (MonadIO (..))
import Control.Monad.Trans.Class   (MonadTrans (lift))
import Control.Monad.Trans.Cont    (ContT (ContT), evalContT)
import Control.Monad.Trans.Control (MonadBaseControl (restoreM))

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar,
                               readTVarIO, writeTVar)

import Hedgehog (Group, MonadTest, Property, PropertyT, annotate, checkParallel,
                 discover, failure, property, withTests, (===))

import Optics.AffineFold (An_AffineFold, preview)
import Optics.Optic      (Is, Optic')
import Optics.TH         (makePrisms)

$(makePrisms ''Status)
$(makePrisms ''Outcome)

main :: IO ()
main = checkParallel group >>= \ok -> when (not ok) exitFailure

group :: Group
group = $$(discover)

pause :: MonadIO m => m ()
pause = liftIO $ threadDelay 1000000

prop_noAutoStart :: Property
prop_noAutoStart = example $ evalContT $ do
    annotate "'LazyAsync' does not start automatically"
    tick <- expectTicks 0
    la <- lazyAsyncCont tick
    pause
    poll la >>= focus _Incomplete

prop_start :: Property
prop_start = example $ evalContT $ do
    annotate "'start' prompts a 'LazyAsync' to run"
    tick <- expectTicks 1
    la <- lazyAsyncCont tick
    start la
    pause

prop_startWait :: Property
prop_startWait = example $ evalContT $ do
    annotate "'startWait' prompts a 'LazyAsync' to run"
    tick <- expectTicks 1
    la <- lazyAsyncCont tick
    _ <- startWait la
    return ()

prop_start_idemotent :: Property
prop_start_idemotent = example $ evalContT $ do
    annotate "'start' is idempotent"
    tick <- expectTicks 1
    la <- lazyAsyncCont tick
    start la
    start la
    pause

prop_startWait_idemotent :: Property
prop_startWait_idemotent = example $ evalContT $ do
    annotate "'startWait' is idemponent"
    tick <- expectTicks 1
    la <- lazyAsyncCont tick
    lift $ startWait la >>= restoreM >>= (=== 1)
    lift $ startWait la >>= restoreM >>= (=== 1)
    return ()

prop_startWaitCatch :: Property
prop_startWaitCatch = example $ evalContT $ do
    annotate "'startWaitCatch' catches exceptions"
    la <- lazyAsyncCont (throw' DivideByZero)
    startWaitCatch la >>= focus _Failure >>= exceptionIs DivideByZero

prop_startWaitCatch_idempotent :: Property
prop_startWaitCatch_idempotent = example $ evalContT $ do
    annotate "'startWaitCatch' is idempotent"
    tick <- expectTicks 1
    la <- lazyAsyncCont tick
    _ <- startWaitCatch la
    _ <- startWaitCatch la
    return ()

prop_startWait_both :: Property
prop_startWait_both = example $ evalContT $ do
    annotate "'startWait' on a complex runs both actions"
    tick <- expectTicks 2
    la1 <- lazyAsyncCont tick
    la2 <- lazyAsyncCont tick
    _ <- startWaitCatch (liftA2 (,) la1 la2)
    return ()

prop_complexOnce :: Property
prop_complexOnce = example $ evalContT $ do
    annotate "actions included in multiple complexes still can only run once"
    tick <- expectTicks 3

    la1 <- lazyAsyncCont tick
    la2 <- lazyAsyncCont tick
    la3 <- lazyAsyncCont tick

    let a = liftA2 (,) la1 la2
        b = liftA2 (,) la3 la2
        c = liftA2 (,) la1 la3

    traverse_ start [a, b, c]
    traverse_ wait [a, b, c]

throw' :: Exception e => e -> m Integer
throw' = throw

example :: PropertyT IO () -> Property
example = withTests 1 . property

type Counter = TVar Natural

newCounter :: MonadIO m => m Counter
newCounter = liftIO $ newTVarIO 0

tickCounter :: MonadIO m => Counter -> m Natural
tickCounter counter = liftIO $ atomically $
  do
    x <- readTVar counter
    writeTVar counter $! x + 1
    return $ x + 1

assertCount :: (MonadIO m, MonadTest m) => Counter -> Natural -> m ()
assertCount counter expected = liftIO (readTVarIO counter) >>= (=== expected)

type Tick m = m Natural

expectTicks :: (MonadIO m, MonadTest m) =>
    Natural -- ^ Expected number of times the 'Tick' action runs
    -> ContT r m (Tick m)
expectTicks n = ContT (\run ->
  do
    counter <- newCounter
    x <- run (tickCounter counter)
    assertCount counter n
    return x
  )

focus :: (MonadTest m, Is k An_AffineFold) => Optic' k is s a -> s -> m a
focus o = maybe failure return . preview o

exceptionIs :: MonadTest m => (Exception e, Eq e, Show e) => e -> SomeException -> m ()
exceptionIs a b = Just a === fromException b
