module LazyAsync.Waiting where

import Control.Applicative    ((*>))
import Control.Concurrent.STM (STM, atomically)
import Control.Monad          ((>=>))
import Data.Function          ((.))
import LazyAsync.Conversions  (outcomeSuccess, statusOutcomeSTM)
import LazyAsync.LazyAsync    (LazyAsync)
import LazyAsync.Outcome      (Outcome)
import LazyAsync.Polling      (pollSTM)
import LazyAsync.Starting     (start)
import System.IO              (IO)

-- | Same as 'waitCatch', but in STM
waitCatchSTM :: LazyAsync a -> STM (Outcome a)
waitCatchSTM = pollSTM >=> statusOutcomeSTM

-- | Wait for the action to complete, and return its value.
-- If the action throws an exception, then the exception is returned.
-- This does __not__ start the action.
waitCatch :: LazyAsync a -> IO (Outcome a)
waitCatch = atomically . waitCatchSTM

-- | Wait for the action to complete, and return its value.
-- If the action throws an exception, then the exception is re-thrown.
-- This does __not__ start the action.
wait :: LazyAsync a -> IO a
wait = waitCatch >=> outcomeSuccess

-- | Begin running an asynchronous action, if it has not already begun.
-- Then wait for it to complete, and return its value.
-- If the action throws an exception, then the exception is re-thrown.
startWait :: LazyAsync a -> IO a
startWait ao = start ao *> wait ao

-- | Begin running an asynchronous action, if it has not already begun.
-- Then wait for it to complete, and return its value.
-- If the action throws an exception, then the exception is returned.
startWaitCatch :: LazyAsync a -> IO (Outcome a)
startWaitCatch ao = start ao *> waitCatch ao
