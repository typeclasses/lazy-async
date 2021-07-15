module AsyncOnce.Waiting where

import AsyncOnce.AsyncOnce
import AsyncOnce.Conversions
import AsyncOnce.Done
import AsyncOnce.Polling
import AsyncOnce.Starting

import Relude

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
