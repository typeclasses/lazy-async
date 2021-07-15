module AsyncOnce.Waiting where

import AsyncOnce.AsyncOnce    (AsyncOnce)
import AsyncOnce.Conversions  (doneSuccess, pollDoneSTM)
import AsyncOnce.Done         (Done)
import AsyncOnce.Polling      (pollSTM)
import AsyncOnce.Starting     (start)
import Control.Applicative    ((*>))
import Control.Concurrent.STM (atomically)
import Control.Monad          ((>=>))
import System.IO              (IO)

-- | Begin running an asynchronous action, if it has not already begun.
-- Then wait for it to complete, and return its value.
-- If the action threw an exception, then the exception is re-thrown.
wait :: AsyncOnce a -> IO a
wait = waitCatch >=> doneSuccess

-- | Begin running an asynchronous action, if it has not already begun.
-- Then wait for it to complete, and return its value.
-- If the action threw an exception, then the exception is returned.
waitCatch :: AsyncOnce a -> IO (Done a)
waitCatch ao = start ao *> atomically ((pollSTM >=> pollDoneSTM) ao)
