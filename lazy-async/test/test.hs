module Main (main) where

import LazyAsync (Outcome, Status, poll, start, startWait, startWaitCatch, wait,
                  withLazyAsync)

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
import Control.Monad.Trans.Control (MonadBaseControl (restoreM))

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar,
                               readTVarIO, writeTVar)

import Hedgehog (Group (Group), MonadTest, Property, PropertyName, PropertyT,
                 checkParallel, failure, property, withTests, (===))

import Optics.AffineFold (An_AffineFold, preview)
import Optics.Optic      (Is, Optic')
import Optics.TH         (makePrisms)

$(makePrisms ''Status)
$(makePrisms ''Outcome)

main :: IO ()
main = checkParallel group >>= \ok -> when (not ok) exitFailure

group :: Group
group = Group "LazyAsync" properties

pause :: MonadIO m => m ()
pause = liftIO $ threadDelay 1000000

properties :: [(PropertyName, Property)]
properties =

  [ (,) "'LazyAsync' does not start automatically" $ example $
      expectTicks 0 $ \tick -> withLazyAsync tick $ \la ->
        do
          pause
          poll la >>= focus _Incomplete

  , (,) "'start' prompts a 'LazyAsync' to run" $ example $
      expectTicks 1 $ \tick -> withLazyAsync tick $ \la ->
        do
          start la
          pause

  , (,) "'startWait' prompts a 'LazyAsync' to run" $ example $
      expectTicks 1 $ \tick -> withLazyAsync tick $ \la ->
        do
          _ <- startWait la
          return ()

  , (,) "'start' is idempotent" $ example $
      expectTicks 1 $ \tick -> withLazyAsync tick $ \la ->
        do
          start la
          start la
          pause

  , (,) "'startWait' is idemponent" $ example $
      expectTicks 1 $ \tick -> withLazyAsync tick $ \la ->
        do
          startWait la >>= restoreM >>= (=== 1)
          startWait la >>= restoreM >>= (=== 1)

  , (,) "'startWaitCatch' catches exceptions" $ example $
      withLazyAsync (throw DivideByZero :: PropertyT IO Integer) $ \la ->
          startWaitCatch la >>= focus _Failure >>= exceptionIs DivideByZero

  , (,) "'startWaitCatch' is idempotent" $ example $
      expectTicks 1 $ \tick -> withLazyAsync tick $ \la ->
        do
          _ <- startWaitCatch la
          _ <- startWaitCatch la
          return ()

  , (,) "'startWait' on an applicative complex runs both actions" $ example $
      expectTicks 2 $ \tick ->
        withLazyAsync tick $ \la1 ->
        withLazyAsync tick $ \la2 ->
          do
            _ <- startWaitCatch (liftA2 (,) la1 la2)
            return ()

  , (,) "actions included in more than one applicative context still can only run once" $ example $
      expectTicks 3 $ \tick ->
        withLazyAsync tick $ \la1 ->
        withLazyAsync tick $ \la2 ->
        withLazyAsync tick $ \la3 ->
          do
            let a = liftA2 (,) la1 la2
                b = liftA2 (,) la3 la2
                c = liftA2 (,) la1 la3
            traverse_ start [a, b, c]
            traverse_ wait [a, b, c]

  ]

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
    -> (Tick m -> m b) -> m b
expectTicks n run =
  do
    counter <- newCounter
    x <- run (tickCounter counter)
    assertCount counter n
    return x

focus :: (MonadTest m, Is k An_AffineFold) => Optic' k is s a -> s -> m a
focus o = maybe failure return . preview o

exceptionIs :: MonadTest m => (Exception e, Eq e, Show e) => e -> SomeException -> m ()
exceptionIs a b = Just a === fromException b
