{-# language Safe #-}

module LazyAsync.Polling where

import Control.Applicative    (pure, (<*>))
import Control.Concurrent.STM (STM, atomically)
import Control.Monad          (return)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor.Compose   (Compose (Compose, getCompose))
import LazyAsync.LazyAsync    (LazyAsync (A0, A1, Ap))
import LazyAsync.Status       (Status)
import System.IO              (IO)

-- | Checks whether an asynchronous action has completed yet
--
-- Does __not__ start the action
poll :: MonadIO m => LazyAsync a -> m (Status a)
poll la = liftIO (pollIO la)

-- | Specialization of 'poll'
pollIO :: LazyAsync a -> IO (Status a)
pollIO la = atomically (pollSTM la)

-- | Same as 'poll', but in 'STM'
pollSTM :: LazyAsync a -> STM (Status a)
pollSTM (A0 x)   = return (pure x)
pollSTM (A1 _ a) = a
pollSTM (Ap x y) = getCompose (Compose (pollSTM x) <*> Compose (pollSTM y))
