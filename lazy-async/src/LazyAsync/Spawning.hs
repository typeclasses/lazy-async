module LazyAsync.Spawning (withLazyAsync, withLazyAsyncIO) where

import Control.Applicative         ((*>))
import Control.Concurrent.STM      (atomically, check)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar)
import Control.Monad               ((>>=))
import Control.Monad.IO.Unlift     (MonadIO, MonadUnliftIO, liftIO, withRunInIO)
import Data.Bool                   (Bool (False))
import Data.Function               (($), (.))
import LazyAsync.LazyAsync         (LazyAsync (A1))
import System.IO                   (IO)

import qualified Control.Concurrent.Async as A

withLazyAsync :: MonadUnliftIO m => m a -> (LazyAsync a -> m b) -> m b
withLazyAsync action continue =
  do
    s <- liftIO $ newTVarIO False
    withAsync (waitForTrue s *> action) $ continue . A1 s

-- | Specialization of 'withLazyAsync'
withLazyAsyncIO :: IO a -> (LazyAsync a -> IO b) -> IO b
withLazyAsyncIO = withLazyAsync

waitForTrue :: MonadIO m => TVar Bool -> m ()
waitForTrue x = liftIO $ atomically $ readTVar x >>= check

withAsync :: MonadUnliftIO m => m a -> (A.Async a -> m b) -> m b
withAsync a b = withRunInIO $ \run -> A.withAsync (run a) (run . b)
