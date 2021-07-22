{-# language Safe #-}

module LazyAsync.Polling where

import Control.Applicative         (pure, (<*>))
import Control.Concurrent.STM      (STM, atomically)
import Control.Monad               (fmap, return, (=<<))
import Control.Monad.Base          (liftBase)
import Control.Monad.IO.Class      (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl, StM, restoreM)
import Data.Functor.Compose        (Compose (Compose, getCompose))
import Data.Traversable            (sequenceA)
import LazyAsync.LazyAsync         (LazyAsync (A0, A1, Ap))
import LazyAsync.Status            (Status)
import System.IO                   (IO)

-- | Checks whether an asynchronous action has completed yet
--
-- Does __not__ start the action
poll :: (MonadBaseControl base m, MonadIO base) => LazyAsync (StM m a) -> m (Status a)
poll la = sequenceA =<< liftBase (fmap (fmap restoreM) (liftIO (pollIO la)))

-- | Specialization of 'poll'
pollIO :: LazyAsync a -> IO (Status a)
pollIO la = atomically (pollSTM la)

-- | Same as 'poll', but in 'STM'
pollSTM :: LazyAsync a -> STM (Status a)
pollSTM (A0 x)   = return (pure x)
pollSTM (A1 _ a) = a
pollSTM (Ap x y) = getCompose (Compose (pollSTM x) <*> Compose (pollSTM y))
