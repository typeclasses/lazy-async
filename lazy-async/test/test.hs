module Main (main) where

import Control.Applicative    ((*>))
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO,
                               readTVarIO)
import Control.Monad          (Monad (return, (>>=)), when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bool              (not)
import Data.Function          (($))
import Hedgehog               (Group (Group), MonadTest, Property, PropertyName,
                               checkParallel, property, withTests, (===))
import Numeric.Natural        (Natural)
import Prelude                (succ)
import System.Exit            (exitFailure)
import System.IO              (IO)
import Time                   (sec, threadDelay)

import qualified LazyAsync as LA

main :: IO ()
main = checkParallel group >>= \ok -> when (not ok) exitFailure

group :: Group
group = Group "LazyAsync" properties

properties :: [(PropertyName, Property)]
properties =

  [ (,) "'LazyAsync' does not start automatically" $ withTests 1 $ property $
      expectTicks 0 $ \tick -> LA.withLazyAsync tick $ \_ -> threadDelay $ sec 1

  , (,) "'start' prompts a 'LazyAsync' to run" $ withTests 1 $ property $
      expectTicks 1 $ \tick -> LA.withLazyAsync tick $ \la ->
        LA.start la *> threadDelay (sec 1)

  , (,) "'start' is idempotent" $ withTests 1 $ property $
      expectTicks 1 $ \tick -> LA.withLazyAsync tick $ \la ->
        LA.start la *> LA.start la *> threadDelay (sec 1)

  ]

-- type Log = TVar [String]

-- newLog :: MonadIO m => m Log
-- newLog = liftIO $ newTVarIO []

-- (<<) :: MonadIO m => Log -> String -> m ()
-- log << str = liftIO $ atomically $ modifyTVar' log (str :)

type Counter = TVar Natural

newCounter :: MonadIO m => m Counter
newCounter = liftIO $ newTVarIO 0

tickCounter :: MonadIO m => Counter -> m ()
tickCounter counter = liftIO $ atomically $ modifyTVar' counter succ

assertCount :: (MonadIO m, MonadTest m) => Counter -> Natural -> m ()
assertCount counter expected = liftIO (readTVarIO counter) >>= (=== expected)

type Tick m = m ()

expectTicks :: (MonadIO m, MonadTest m) =>
    Natural -- ^ Expected number of times the 'Tick' action runs
    -> (Tick m -> m b) -> m b
expectTicks n run =
  do
    counter <- newCounter
    x <- run (tickCounter counter)
    assertCount counter n
    return x
