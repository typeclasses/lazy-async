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

-- | Create a situation wherein an action shall run asynchronously, if and when it is needed, at most once
--
-- The 'LazyAsync' is only available within the continuation; when the continuation ends, the action is stopped
withLazyAsync :: MonadBaseControl IO m =>
    m a -- ^ Action
    -> (LazyAsync (StM m a) -> m b) -- ^ Continuation
    -> m b
withLazyAsync action continue =
    newTVar False >>= \s -> withAsync (waitForTrue s *> action) $ continue . A1 s

-- | Specialization of 'withLazyAsync'
withLazyAsyncIO ::
    IO a  -- ^ Action
    -> (LazyAsync a -> IO b) -- ^ Continuation
    -> IO b
withLazyAsyncIO = withLazyAsync

waitForTrue :: MonadBase IO m => TVar Bool -> m ()
waitForTrue x = liftBase $ atomically $ readTVar x >>= check

newTVar :: MonadBase IO m => a -> m (TVar a)
newTVar = liftBase . newTVarIO
