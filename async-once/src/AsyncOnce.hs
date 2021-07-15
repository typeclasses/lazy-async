module AsyncOnce
  ( -- * Asynchronous actions
    AsyncOnce
    -- * Spawning
  , withAsyncOnce
    -- * Getting results
  , wait
    -- ** Catching exceptions
  , waitCatch, Done (..)
    -- ** Polling
  , poll, Poll (..)
    -- * Starting manually
  , start
    -- * Transactions
  , pollSTM
  ) where

import AsyncOnce.AsyncOnce
import AsyncOnce.Conversions
import AsyncOnce.Done
import AsyncOnce.PollType
import AsyncOnce.Polling

-- relude
import Relude

-- async
import qualified Control.Concurrent.Async as Async

-- stm
import Control.Concurrent.STM (check)

-- transformers
import Control.Monad.Trans.Cont

withAsyncOnce :: IO a -> (AsyncOnce a -> IO b) -> IO b
withAsyncOnce action = runContT $ do
    s <- newTVarIO False
    a <- ContT $ Async.withAsync $ waitForTrueIO s *> action
    return $ A1 s a

waitForTrueIO :: TVar Bool -> IO ()
waitForTrueIO x = atomically $ readTVar x >>= check

-- | Begin running an asynchronous action, if it has not already begun.
start :: AsyncOnce a -> IO ()
start A0{} = return ()
start (A1 s _) = atomically $ writeTVar s True
start (A2 x y) = start x *> start y

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
