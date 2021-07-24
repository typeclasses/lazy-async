{-# language Safe #-}

module LazyAsync.Waiting where

import Control.Applicative         ((*>))
import Control.Concurrent.STM      (STM, atomically, retry)
import Control.Monad               (fmap, return, (=<<), (>=>), (>>=))
import Control.Monad.Base          (liftBase)
import Control.Monad.Catch         (MonadThrow, throwM)
import Control.Monad.IO.Class      (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl, StM, restoreM)
import Data.Traversable            (sequenceA)
import LazyAsync.Polling           (pollSTM)
import LazyAsync.Starting          (start)
import LazyAsync.Types.LazyAsync   (LazyAsync)
import LazyAsync.Types.Outcome     (Outcome (Failure, Success))
import LazyAsync.Types.Status      (Status (Done, Incomplete))
import System.IO                   (IO)

-- | Akin to 'waitCatch'
waitCatchSTM :: LazyAsync a -> STM (Outcome a)
waitCatchSTM = pollSTM >=> statusOutcomeSTM

-- | Waits for the action to complete, and returns its value
--
-- If the action throws an exception, then the exception is returned
--
-- Does __not__ start the action
waitCatch :: (MonadBaseControl base m, MonadIO base) => LazyAsync (StM m a) -> m (Outcome a)
waitCatch x = sequenceA =<< liftBase (fmap (fmap restoreM) (liftIO (waitCatchIO x)))

-- | Akin to 'waitCatch'
waitCatchIO :: LazyAsync a -> IO (Outcome a)
waitCatchIO la = atomically (waitCatchSTM la)

-- | Waits for the action to complete, and returns its value
--
-- If the action throws an exception, then the exception is re-thrown
--
-- Does __not__ start the action
wait :: (MonadBaseControl base m, MonadIO base) => LazyAsync (StM m a) -> m a
wait x = liftBase (liftIO (waitCatchIO x) >>= (\o -> liftIO (outcomeSuccess o))) >>= restoreM

-- | Akin to 'wait'
waitIO :: LazyAsync a -> IO a
waitIO = wait

-- | Starts an asynchronous action, waits for it to complete, and returns its value
--
-- If the action throws an exception, then the exception is re-thrown
--
-- @('startWait' x)@ is equivalent to @('start' x '*>' 'wait' x)@
startWait :: (MonadBaseControl base m, MonadIO base) => LazyAsync (StM m a) -> m a
startWait x = start x *> wait x

-- | Akin to 'startWait'
startWaitIO :: LazyAsync a -> IO a
startWaitIO = startWait

-- | Starts an asynchronous action, waits for it to complete, and returns its value
--
-- If the action throws an exception, then the exception is returned
--
-- @('startWaitCatch' x)@ is equivalent to @('start' x '*>' 'waitCatch' x)@
startWaitCatch :: (MonadBaseControl base m, MonadIO base) => LazyAsync (StM m a) -> m (Outcome a)
startWaitCatch x = start x *> waitCatch x

-- | Akin to 'startWaitCatch'
startWaitCatchIO :: LazyAsync a -> IO (Outcome a)
startWaitCatchIO = startWaitCatch

statusOutcomeSTM :: Status a -> STM (Outcome a)
statusOutcomeSTM Incomplete = retry
statusOutcomeSTM (Done x)   = return x

outcomeSuccess :: MonadThrow m => Outcome a -> m a
outcomeSuccess (Failure e) = throwM e
outcomeSuccess (Success x) = return x
