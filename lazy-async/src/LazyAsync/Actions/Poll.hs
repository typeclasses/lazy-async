{-# language Safe #-}

module LazyAsync.Actions.Poll where

import LazyAsync.Types.Apply     (Apply (..))
import LazyAsync.Types.LazyAsync (LazyAsync (..))
import LazyAsync.Types.StartPoll (StartPoll (..))
import LazyAsync.Types.Status    (Status)

import LazyAsync.Prelude (Alternative (empty, (<|>)),
                          Applicative (liftA2, pure, (<*>)), Functor (fmap), IO,
                          MonadBase (liftBase), MonadBaseControl (..),
                          MonadIO (..), STM, Traversable (sequenceA),
                          atomically, return, (=<<))

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
