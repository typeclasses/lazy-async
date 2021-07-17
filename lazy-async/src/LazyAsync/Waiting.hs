{-# language Safe #-}

module LazyAsync.Waiting where

import Control.Applicative    ((*>))
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Monad          (return, (>=>))
import Control.Monad.Catch    (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import LazyAsync.LazyAsync    (LazyAsync)
import LazyAsync.Outcome      (Outcome (Failure, Success))
import LazyAsync.Polling      (pollSTM)
import LazyAsync.Starting     (start, startIO)
import LazyAsync.Status       (Status (Done, Incomplete))
import System.IO              (IO)

-- | Same as 'waitCatch', but in 'STM'
waitCatchSTM :: LazyAsync a -> STM (Outcome a)
waitCatchSTM = pollSTM >=> statusOutcomeSTM

-- | Waits for the action to complete, and returns its value
--
-- If the action throws an exception, then the exception is returned
--
-- Does __not__ start the action
waitCatch :: MonadIO m => LazyAsync a -> m (Outcome a)
waitCatch la = liftIO (waitCatchIO la)

-- | Specialization of 'waitCatch'
waitCatchIO :: LazyAsync a -> IO (Outcome a)
waitCatchIO la = atomically (waitCatchSTM la)

-- | Waits for the action to complete, and returns its value
--
-- If the action throws an exception, then the exception is re-thrown
--
-- Does __not__ start the action
wait :: MonadIO m => LazyAsync a -> m a
wait la = liftIO (waitIO la)

-- | Specialization of 'wait'
waitIO :: LazyAsync a -> IO a
waitIO = waitCatch >=> outcomeSuccess

-- | Starts an asynchronous action, waits for it to complete, and returns its value
--
-- If the action throws an exception, then the exception is re-thrown
--
-- @('startWait' x)@ is equivalent to @('start' x '*>' 'wait' x)@
startWait :: MonadIO m => LazyAsync a -> m a
startWait x = start x *> wait x

-- | Specialization of 'startWait'
startWaitIO :: LazyAsync a -> IO a
startWaitIO ao = startIO ao *> waitIO ao

-- | Starts an asynchronous action, waits for it to complete, and returns its value
--
-- If the action throws an exception, then the exception is returned
--
-- @('startWaitCatch' x)@ is equivalent to @('start' x '*>' 'waitCatch' x)@
startWaitCatch :: MonadIO m => LazyAsync a -> m (Outcome a)
startWaitCatch x = start x *> waitCatch x

-- | Specialization of 'startWaitCatch'
startWaitCatchIO :: LazyAsync a -> IO (Outcome a)
startWaitCatchIO ao = start ao *> waitCatch ao

statusOutcomeSTM :: Status a -> STM (Outcome a)
statusOutcomeSTM Incomplete = retry
statusOutcomeSTM (Done x)   = return x

outcomeSuccess :: MonadThrow m => Outcome a -> m a
outcomeSuccess (Failure e) = throwM e
outcomeSuccess (Success x) = return x
