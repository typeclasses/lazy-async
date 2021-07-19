module Test.Counter (expectTicks) where

import Data.Function   (($))
import Hedgehog        (MonadTest, (===))
import Numeric.Natural (Natural)
import Prelude         (($!), (+))

import Control.Monad            (return)
import Control.Monad.IO.Class   (MonadIO, liftIO)
import Control.Monad.Trans.Cont (ContT (ContT))

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar,
                               readTVarIO, writeTVar)

type Counter = TVar Natural

newCounter :: MonadIO m => m Counter
newCounter = liftIO $ newTVarIO 0

tickCounter :: MonadIO m => Counter -> m Natural
tickCounter counter = liftIO $ atomically $ do
    x <- readTVar counter
    _ <- writeTVar counter $! x + 1
    return $ x + 1

assertCount :: (MonadIO m, MonadTest m) => Counter -> Natural -> m ()
assertCount counter expected = do
    x <- liftIO $ readTVarIO counter
    x === expected

expectTicks :: (MonadIO m, MonadTest m) =>
    Natural -- ^ Expected number of times the 'Tick' action runs
    -> ContT r m (m Natural)
expectTicks n = ContT $ \run -> do
    counter <- newCounter
    x <- run $ tickCounter counter
    assertCount counter n
    return x
