{-# language Safe #-}

module LazyAsync.Actions.Poll where

import LazyAsync.Actions.Empty (emptyStatus)
import LazyAsync.Actions.Pure  (pureStatus)
import LazyAsync.Types         (Complex (..), LazyAsync (..), StartPoll (..),
                                Status (..))

import LazyAsync.Prelude (IO, MonadBaseControl, MonadIO, STM, StM, atomically,
                          fmap, liftA2, liftBase, liftIO, restoreM, return,
                          sequenceA, (=<<))

-- | 🕵️ Checks whether an asynchronous action has completed yet
--
-- 🛑 Does not start the action
poll :: (MonadBaseControl base m, MonadIO base) => LazyAsync (StM m a) -> m (Status a)
poll la = sequenceA =<< liftBase (fmap (fmap restoreM) (liftIO (pollIO la)))

-- | Akin to 'poll'
pollIO :: LazyAsync a -> IO (Status a)
pollIO la = atomically (pollSTM la)

-- | Akin to 'poll'
pollSTM :: LazyAsync a -> STM (Status a)
pollSTM (Pure x)             = return (pureStatus x)
pollSTM (A1 (StartPoll _ a)) = a
pollSTM (A2 (Complex o x y)) = liftA2 (o) (pollSTM x) (pollSTM y)
pollSTM Empty                = return emptyStatus
