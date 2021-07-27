{-# language Safe #-}

module LazyAsync.Actions.Wait where

import LazyAsync.Actions.Poll (pollSTM)

import LazyAsync.Types (LazyAsync, Outcome (..), Status (..))

import LazyAsync.Prelude (Functor (fmap), IO, MonadBase (liftBase),
                          MonadBaseControl (..), MonadIO (..), MonadThrow (..),
                          STM, Traversable (sequenceA), atomically, retry,
                          return, (=<<), (>=>), (>>=))

-- | Akin to 'waitCatch'
waitCatchSTM :: LazyAsync a -> STM (Outcome a)
waitCatchSTM = pollSTM >=> statusOutcomeSTM

-- | ⏸️ Waits for the action to complete and ✅ returns its value
--
-- 💣 If the action throws an exception, then the exception is returned
--
-- 🛑 Does not start the action
waitCatch :: (MonadBaseControl base m, MonadIO base) => LazyAsync (StM m a) -> m (Outcome a)
waitCatch x = sequenceA =<< liftBase (fmap (fmap restoreM) (liftIO (waitCatchIO x)))

-- | Akin to 'waitCatch'
waitCatchIO :: LazyAsync a -> IO (Outcome a)
waitCatchIO la = atomically (waitCatchSTM la)

-- | ⏸️ Waits for the action to complete and ✅ returns its value
--
-- 💣 If the action throws an exception, then the exception is re-thrown
--
-- 🛑 Does not start the action
wait :: (MonadBaseControl base m, MonadIO base) => LazyAsync (StM m a) -> m a
wait x = liftBase (liftIO (waitCatchIO x) >>= (\o -> liftIO (outcomeSuccess o))) >>= restoreM

-- | Akin to 'wait'
waitIO :: LazyAsync a -> IO a
waitIO = wait

statusOutcomeSTM :: Status a -> STM (Outcome a)
statusOutcomeSTM Incomplete = retry
statusOutcomeSTM (Done x)   = return x

outcomeSuccess :: MonadThrow m => Outcome a -> m a
outcomeSuccess (Failure e) = throwM e
outcomeSuccess (Success x) = return x
