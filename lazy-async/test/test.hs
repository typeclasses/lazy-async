module Main (main) where

import LazyAsync

import Test.Counter
import Test.Exceptions
import Test.General
import Test.Optics
import Test.Person

import Data.Foldable (traverse_)
import Data.Function (($), (.))
import Data.Functor  (($>))
import Prelude       (Integer, (+))
import System.IO     (IO)

import Control.Exception (ArithException (DivideByZero))

import Control.Applicative       (liftA2, (<|>))
import Control.Monad             (replicateM_, return, (>>=))
import Control.Monad.Trans.Class (MonadTrans (lift))

import Hedgehog (Property, annotate, discover, (===))

main :: IO ()
main = main' $$(discover)

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
    lift (startWait la >>= (=== 1))

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

prop_apply_startWait_both :: Property
prop_apply_startWait_both = contExample do
    annotate "'startWait' on an applicative complex runs both actions"
    tick <- expectTicks 2

    outcome <- contIO do
        la1 <- lazyAsync tick
        la2 <- lazyAsync tick
        let complex = liftA2 (+) la1 la2
        lift (startWaitCatch complex)

    lift (focus _Success outcome >>= (=== 3))

prop_apply_once :: Property
prop_apply_once = contExample do
    annotate "actions included in multiple applicative complexes still can only run once"
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

prop_choose :: Property
prop_choose = contExample do
    annotate "(<|>) can tolerate the failure of either part"
    la1 <- lazyAsync (return (5 :: Integer))
    la2 <- lazyAsync (throw' DivideByZero)

    let complex1 = la1 <|> la2
        complex2 = la2 <|> la1

    lift do
        startWait complex1 >>= (=== 5)
        startWait complex2 >>= (=== 5)

prop_rank2 :: Property
prop_rank2 = contExample do
    annotate "memoizeRank2 separately memoizes each field of a higher-kinded datatype"
    tick <- expectTicks 2

    person <- memoizeRank2
        Person
            { name = tick $> "Chris"
            , age  = tick $> 34
            , location =
                Location
                  { city  = tick $> "Ronan"
                  , state = tick $> "Montana"
                  }
            }

    lift $ name person               >>= (=== "Chris")
    lift $ (state . location) person >>= (=== "Montana")
