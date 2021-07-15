module LazyAsync.Waiting where

import Control.Applicative    ((*>))
import Control.Concurrent.STM (atomically)
import Control.Monad          ((>=>))
import LazyAsync.Conversions  (outcomeSuccess, statusOutcomeSTM)
import LazyAsync.LazyAsync    (LazyAsync)
import LazyAsync.Outcome      (Outcome)
import LazyAsync.Polling      (pollSTM)
import LazyAsync.Starting     (start)
import System.IO              (IO)

-- | Begin running an asynchronous action, if it has not already begun.
-- Then wait for it to complete, and return its value.
-- If the action threw an exception, then the exception is re-thrown.
wait :: LazyAsync a -> IO a
wait = waitCatch >=> outcomeSuccess

-- | Begin running an asynchronous action, if it has not already begun.
-- Then wait for it to complete, and return its value.
-- If the action threw an exception, then the exception is returned.
waitCatch :: LazyAsync a -> IO (Outcome a)
waitCatch ao = start ao *> atomically ((pollSTM >=> statusOutcomeSTM) ao)
