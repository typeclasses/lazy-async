module LazyAsync.Spawning (withLazyAsync, withLazyAsyncIO) where

import Control.Applicative             ((*>))
import Control.Concurrent.Async.Lifted (withAsync)
import Control.Concurrent.STM          (atomically, check)
import Control.Concurrent.STM.TVar     (TVar, newTVarIO, readTVar)
import Control.Monad                   ((>>=))
import Control.Monad.Base              (MonadBase, liftBase)
import Control.Monad.Trans.Control     (MonadBaseControl, StM)
import Data.Bool                       (Bool (False))
import Data.Function                   (($), (.))
import LazyAsync.LazyAsync             (LazyAsync (A1))
import System.IO                       (IO)

withLazyAsync :: MonadBaseControl IO m => m a -> (LazyAsync (StM m a) -> m b) -> m b
withLazyAsync action continue =
    newTVar False >>= \s -> withAsync (waitForTrue s *> action) $ continue . A1 s

-- | Specialization of 'withLazyAsync'
withLazyAsyncIO :: IO a -> (LazyAsync a -> IO b) -> IO b
withLazyAsyncIO = withLazyAsync

waitForTrue :: MonadBase IO m => TVar Bool -> m ()
waitForTrue x = liftBase $ atomically $ readTVar x >>= check

newTVar :: MonadBase IO m => a -> m (TVar a)
newTVar = liftBase . newTVarIO
