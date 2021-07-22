module Main (main) where

import LazyAsync

import Test.Counter
import Test.Exceptions
import Test.Optics

import Control.Concurrent   (threadDelay)
import Data.Bool            (not)
import Data.Foldable        (traverse_)
import Data.Function        (($), (.))
import Data.Functor.Compose
import Numeric.Natural
import System.Exit          (exitFailure)
import System.IO            (IO)

import Control.Exception (ArithException (DivideByZero))

import Control.Applicative         (liftA2)
import Control.Monad               (Monad (return, (>>=)), replicateM_, when)
import Control.Monad.Base          (MonadBase, liftBase)
import Control.Monad.Trans.Class   (MonadTrans (lift))
import Control.Monad.Trans.Control

import Hedgehog (Group, Property, PropertyT, annotate, checkParallel, discover,
                 property, withTests, (===))


main :: IO ()
main = checkParallel group >>= \ok -> when (not ok) exitFailure

group :: Group
group = $$(discover)

example :: PropertyT IO () -> Property
example = withTests 1 . property

pause :: MonadBase IO m => m ()
pause = liftBase $ threadDelay 1000000

prop_noAutoStart :: Property
prop_noAutoStart = example $ evalContT $ do
    annotate "'LazyAsync' does not start automatically"
    tick <- expectTicks 0
    la <- lazyAsync tick
    pause
    lift (poll la) >>= focus _Incomplete

prop_memoize_noAutoStart :: Property
prop_memoize_noAutoStart = example $ evalContT $ do
    annotate "'memoize' does not start the action"
    tick <- expectTicks 0
    _ <- memoize tick
    pause

prop_start :: Property
prop_start = example $ evalContT $ do
    annotate "'start' prompts a 'LazyAsync' to run"
    tick <- expectTicks 1
    la <- lazyAsync tick
    start la
    pause

prop_startWait :: Property
prop_startWait = example $ evalContT $ do
    annotate "'startWait' prompts a 'LazyAsync' to run"
    tick <- expectTicks 1
    la <- lazyAsync tick
    lift $ startWait la >>= (=== 1)

prop_start_idempotent :: Property
prop_start_idempotent = example $ evalContT $ do
    annotate "'start' is idempotent"
    tick <- expectTicks 1
    la <- lazyAsync tick
    replicateM_ 2 $ start la
    pause

prop_startWait_idempotent :: Property
prop_startWait_idempotent = example $ evalContT $ do
    annotate "'startWait' is idemponent"
    tick <- expectTicks 1
    la <- lazyAsync tick
    lift $ replicateM_ 2 $ startWait la >>= (=== 1)

prop_memoize_idempotent :: Property
prop_memoize_idempotent = example $ evalContT $ do
    annotate "The action returned by 'memoize' is idempotent"
    tick <- expectTicks 1
    tick' <- memoize tick
    lift $ replicateM_ 2 $ tick' >>= (=== 1)

prop_startWaitCatch :: Property
prop_startWaitCatch = example $ evalContT $ do
    annotate "'startWaitCatch' catches exceptions"
    la <- lazyAsync (throw' DivideByZero)
    lift (startWaitCatch la) >>= focus _Failure >>= exceptionIs DivideByZero

prop_startWaitCatch_idempotent :: Property
prop_startWaitCatch_idempotent = example $ evalContT $ do
    annotate "'startWaitCatch' is idempotent"
    tick <- expectTicks 1
    la <- lazyAsync tick
    replicateM_ 2 $ lift (startWaitCatch la)

{-

prop_startWait_both :: Property
prop_startWait_both = example $ evalContT $ do
    annotate "'startWait' on a complex runs both actions"
    tick <- expectTicks 2
    la1 <- lazyAsync tick
    la2 <- lazyAsync tick
    _ <- startWaitCatch (getCompose (liftA2 (,) (Compose la1) (Compose la2)))
    return ()

prop_complexOnce :: Property
prop_complexOnce = example $ evalContT $ do
    annotate "actions included in multiple complexes still can only run once"
    tick <- expectTicks 3

    la1 <- lazyAsync tick
    la2 <- lazyAsync tick
    la3 <- lazyAsync tick

    let a = liftA2 (,) la1 la2
        b = liftA2 (,) la3 la2
        c = liftA2 (,) la1 la3

    traverse_ start [a, b, c]
    lift (traverse_ wait [a, b, c])

-}
