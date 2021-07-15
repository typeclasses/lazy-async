module LazyAsync.Waiting where

import Control.Applicative    ((*>))
import Control.Concurrent.STM (atomically)
import Control.Monad          ((>=>))
import LazyAsync.Conversions  (doneSuccess, statusDoneSTM)
import LazyAsync.Done         (Done)
import LazyAsync.LazyAsync    (LazyAsync)
import LazyAsync.Polling      (pollSTM)
import LazyAsync.Starting     (start)
import System.IO              (IO)

-- | Begin running an asynchronous action, if it has not already begun.
-- Then wait for it to complete, and return its value.
-- If the action threw an exception, then the exception is re-thrown.
wait :: LazyAsync a -> IO a
wait = waitCatch >=> doneSuccess

-- | Begin running an asynchronous action, if it has not already begun.
-- Then wait for it to complete, and return its value.
-- If the action threw an exception, then the exception is returned.
waitCatch :: LazyAsync a -> IO (Done a)
waitCatch ao = start ao *> atomically ((pollSTM >=> statusDoneSTM) ao)
