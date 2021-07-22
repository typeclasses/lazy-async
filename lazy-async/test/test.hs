module Main (main) where

import LazyAsync

import Test.Counter
import Test.Exceptions
import Test.Optics

import Control.Concurrent (threadDelay)
import Data.Bool          (not)
import Data.Foldable      (traverse_)
import Data.Function      ((.))
import Prelude            ((+))
import System.Exit        (exitFailure)
import System.IO          (IO)

import Control.Exception (ArithException (DivideByZero))

import Control.Applicative       (liftA2)
import Control.Monad             (Monad ((>>=)), replicateM_, when)
import Control.Monad.Trans.Class (MonadTrans (lift))

import Hedgehog (Group, Property, PropertyT, annotate, checkParallel, discover,
                 property, withTests, (===))


main :: IO ()
main = checkParallel group >>= \ok -> when (not ok) exitFailure

group :: Group
group = $$(discover)

example :: PropertyT IO () -> Property
example = withTests 1 . property

contExample :: ContT () (PropertyT IO) () -> Property
contExample = example . evalContT

contIO :: MonadIO m => ContT a IO a -> ContT () m a
contIO = liftIO . evalContT

pause :: MonadBase IO m => m ()
pause = liftBase (threadDelay 1000000)

prop_noAutoStart :: Property
prop_noAutoStart = contExample do
    annotate "'LazyAsync' does not start automatically"
    tick <- expectTicks 0
    la <- lazyAsync tick
    pause
    lift (poll la) >>= focus _Incomplete

prop_memoize_noAutoStart :: Property
prop_memoize_noAutoStart = contExample do
    annotate "'memoize' does not start the action"
    tick <- expectTicks 0
    _ <- memoize tick
    pause

prop_start :: Property
prop_start = contExample do
    annotate "'start' prompts a 'LazyAsync' to run"
    tick <- expectTicks 1
    la <- lazyAsync tick
    start la
    pause

prop_startWait :: Property
prop_startWait = contExample do
    annotate "'startWait' prompts a 'LazyAsync' to run"
    tick <- expectTicks 1
    la <- lazyAsync tick
    lift( startWait la >>= (=== 1))

prop_start_idempotent :: Property
prop_start_idempotent = contExample do
    annotate "'start' is idempotent"
    tick <- expectTicks 1
    la <- lazyAsync tick
    replicateM_ 2 (start la)
    pause

prop_startWait_idempotent :: Property
prop_startWait_idempotent = contExample do
    annotate "'startWait' is idemponent"
    tick <- expectTicks 1
    la <- lazyAsync tick
    lift (replicateM_ 2 (startWait la >>= (=== 1)))

prop_memoize_idempotent :: Property
prop_memoize_idempotent = contExample do
    annotate "The action returned by 'memoize' is idempotent"
    tick <- expectTicks 1
    tick' <- memoize tick
    lift (replicateM_ 2 (tick' >>= (=== 1)))

prop_startWaitCatch :: Property
prop_startWaitCatch = contExample do
    annotate "'startWaitCatch' catches exceptions"
    la <- lazyAsync (throw' DivideByZero)
    lift (startWaitCatch la) >>= focus _Failure >>= exceptionIs DivideByZero

prop_startWaitCatch_idempotent :: Property
prop_startWaitCatch_idempotent = contExample do
    annotate "'startWaitCatch' is idempotent"
    tick <- expectTicks 1
    la <- lazyAsync tick
    replicateM_ 2 (lift (startWaitCatch la))

prop_startWait_both :: Property
prop_startWait_both = contExample do
    annotate "'startWait' on a complex runs both actions"
    tick <- expectTicks 2

    outcome <- contIO do
        la1 <- lazyAsync tick
        la2 <- lazyAsync tick
        let complex = liftA2 (+) la1 la2
        lift (startWaitCatch complex)

    lift (focus _Success outcome >>= (=== 3))

prop_complexOnce :: Property
prop_complexOnce = contExample do
    annotate "actions included in multiple complexes still can only run once"
    tick <- expectTicks 3

    contIO do
        la1 <- lazyAsync tick
        la2 <- lazyAsync tick
        la3 <- lazyAsync tick

        let complexes = [ liftA2 (,) la1 la2
                        , liftA2 (,) la3 la2
                        , liftA2 (,) la1 la3 ]

        traverse_ start complexes
        lift (traverse_ wait complexes)
