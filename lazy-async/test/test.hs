module Main (main) where

import Data.Bool       (not)
import Data.Eq         (Eq)
import Data.Foldable   (traverse_)
import Data.Function   (($), (.))
import Data.Maybe      (Maybe (Just))
import Numeric.Natural (Natural)
import Prelude         (Integer, ($!), (+))
import System.Exit     (exitFailure)
import System.IO       (IO)
import Text.Show       (Show)
import Time            (sec, threadDelay)

import Control.Exception (ArithException (DivideByZero),
                          Exception (fromException), SomeException, throw)

import Control.Applicative         (liftA2, (*>))
import Control.Monad               (Monad (return, (>>=)), when)
import Control.Monad.IO.Class      (MonadIO (..))
import Control.Monad.Trans.Control (MonadBaseControl (restoreM))

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar,
                               readTVarIO, writeTVar)

import Hedgehog (Group (Group), MonadTest, Property, PropertyName, PropertyT,
                 checkParallel, failure, property, success, withTests, (===))

import qualified LazyAsync as LA

main :: IO ()
main = checkParallel group >>= \ok -> when (not ok) exitFailure

group :: Group
group = Group "LazyAsync" properties

properties :: [(PropertyName, Property)]
properties =

  [ (,) "'LazyAsync' does not start automatically" $ example $
      expectTicks 0 $ \tick -> LA.withLazyAsync tick $ \la ->
        do
          threadDelay $ sec 1
          LA.poll la >>= statusIncomplete

  , (,) "'start' prompts a 'LazyAsync' to run" $ example $
      expectTicks 1 $ \tick -> LA.withLazyAsync tick $ \la ->
        do
          LA.start la
          threadDelay $ sec 1

  , (,) "'startWait' prompts a 'LazyAsync' to run" $ example $
      expectTicks 1 $ \tick -> LA.withLazyAsync tick $ \la ->
        do
          _ <- LA.startWait la
          return ()

  , (,) "'start' is idempotent" $ example $
      expectTicks 1 $ \tick -> LA.withLazyAsync tick $ \la ->
        do
          LA.start la
          LA.start la
          threadDelay $ sec 1

  , (,) "'startWait' is idemponent" $ example $
      expectTicks 1 $ \tick -> LA.withLazyAsync tick $ \la ->
        do
          LA.startWait la >>= restoreM >>= (=== 1)
          LA.startWait la >>= restoreM >>= (=== 1)

  , (,) "'startWaitCatch' catches exceptions" $ example $
      LA.withLazyAsync (throw DivideByZero :: PropertyT IO Integer) $ \la ->
          LA.startWaitCatch la >>= outcomeFailure >>= exceptionIs DivideByZero

  , (,) "'startWaitCatch' is idempotent" $ example $
      expectTicks 1 $ \tick -> LA.withLazyAsync tick $ \la ->
        do
          _ <- LA.startWaitCatch la
          _ <- LA.startWaitCatch la
          return ()

  , (,) "'startWait' on an applicative complex runs both actions" $ example $
      expectTicks 2 $ \tick ->
        LA.withLazyAsync tick $ \la1 ->
        LA.withLazyAsync tick $ \la2 ->
          do
            _ <- LA.startWaitCatch (liftA2 (,) la1 la2)
            return ()

  , (,) "actions included in more than one applicative context still can only run once" $ example $
      expectTicks 3 $ \tick ->
        LA.withLazyAsync tick $ \la1 ->
        LA.withLazyAsync tick $ \la2 ->
        LA.withLazyAsync tick $ \la3 ->
          do
            let a = liftA2 (,) la1 la2
                b = liftA2 (,) la3 la2
                c = liftA2 (,) la1 la3
            traverse_ LA.start [a, b, c]
            traverse_ LA.wait [a, b, c]

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

statusIncomplete :: MonadTest m => LA.Status a -> m ()
statusIncomplete LA.Incomplete = success
statusIncomplete _             = failure

statusDone :: MonadTest m => LA.Status a -> m (LA.Outcome a)
statusDone (LA.Done x) = return x
statusDone _           = failure

outcomeFailure :: MonadTest m => LA.Outcome a -> m SomeException
outcomeFailure (LA.Failure x) = return x
outcomeFailure _              = failure

outcomeSuccess :: MonadTest m => LA.Outcome a -> m a
outcomeSuccess (LA.Success x) = return x
outcomeSuccess _              = failure

exceptionIs :: MonadTest m => (Exception e, Eq e, Show e) => e -> SomeException -> m ()
exceptionIs a b = Just a === fromException b
