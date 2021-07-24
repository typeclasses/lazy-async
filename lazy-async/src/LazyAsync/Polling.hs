{-# language Safe #-}

module LazyAsync.Polling where

import Control.Applicative         (empty, liftA2, pure, (<*>), (<|>))
import Control.Concurrent.STM      (STM, atomically)
import Control.Monad               (fmap, return, (=<<))
import Control.Monad.Base          (liftBase)
import Control.Monad.IO.Class      (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl, StM, restoreM)
import Data.Traversable            (sequenceA)
import LazyAsync.Types.Apply       (Apply (..))
import LazyAsync.Types.LazyAsync   (LazyAsync (..), StartPoll (..))
import LazyAsync.Types.Status      (Status)
import System.IO                   (IO)

-- | Checks whether an asynchronous action has completed yet
--
-- Does __not__ start the action
poll :: (MonadBaseControl base m, MonadIO base) => LazyAsync (StM m a) -> m (Status a)
poll la = sequenceA =<< liftBase (fmap (fmap restoreM) (liftIO (pollIO la)))

-- | Akin to 'poll'
pollIO :: LazyAsync a -> IO (Status a)
pollIO la = atomically (pollSTM la)

-- | Akin to 'poll'
pollSTM :: LazyAsync a -> STM (Status a)
pollSTM (Pure x)             = return (pure x)
pollSTM (A1 (StartPoll _ a)) = a
pollSTM (Ap (Apply x y))     = liftA2 (<*>) (pollSTM x) (pollSTM y)
pollSTM (Choose x y)         = liftA2 (<|>) (pollSTM x) (pollSTM y)
pollSTM Empty                = return empty
