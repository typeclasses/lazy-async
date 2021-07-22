module Test.Counter (expectTicks) where

import Data.Function   (($))
import Hedgehog        (MonadTest, (===))
import Numeric.Natural (Natural)
import Prelude         (($!), (+))
import System.IO       (IO)

import Control.Monad            (return)
import Control.Monad.Base       (MonadBase, liftBase)
import Control.Monad.Trans.Cont (ContT (ContT))

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar,
                               readTVarIO, writeTVar)

type Counter = TVar Natural

newCounter :: MonadBase IO m => m Counter
newCounter = liftBase $ newTVarIO 0

tickCounter :: MonadBase IO m => Counter -> m Natural
tickCounter counter = liftBase $ atomically $ do
    x <- readTVar counter
    _ <- writeTVar counter $! x + 1
    return $ x + 1

assertCount :: (MonadBase IO m, MonadTest m) => Counter -> Natural -> m ()
assertCount counter expected = do
    x <- liftBase $ readTVarIO counter
    x === expected

expectTicks :: (MonadBase IO m, MonadTest m) =>
    Natural -- ^ Expected number of times the 'Tick' action runs
    -> ContT r m (m Natural)
expectTicks n = ContT $ \run -> do
    counter <- newCounter
    x <- run $ tickCounter counter
    assertCount counter n
    return x
