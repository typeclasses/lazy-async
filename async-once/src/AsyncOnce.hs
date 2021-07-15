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
    -- * Starting and stopping
  , start, cancel
  ) where

import Relude
import Control.Monad.Trans.Cont
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.Async (Async)
import Control.Concurrent.STM (check)

data AsyncOnce a =
  AsyncOnce
    { aoStart :: TVar Bool
    , aoAsync :: Async a
    }

data Poll a = Incomplete | Done (Done a)

data Done a = Failure SomeException | Success a

withAsyncOnce :: IO a -> (AsyncOnce a -> IO b) -> IO b
withAsyncOnce action =
  runContT $
    do
      aoStart <- newTVarIO False

      aoAsync <- ContT $ Async.withAsync $
        do
          atomically $ readTVar aoStart >>= check
          action

      return $ AsyncOnce{ aoStart, aoAsync }

-- | Begin running an asynchronous action, if it has not already begun.
start :: AsyncOnce a -> IO ()
start AsyncOnce{ aoStart } = atomically $ writeTVar aoStart True

-- | Begin running an asynchronous action, if it has not already begun.
-- Then wait for it to complete, and return its value.
-- If the action threw an exception, then the exception is re-thrown.
wait :: AsyncOnce a -> IO a
wait ao@AsyncOnce{ aoAsync } = start ao *> Async.wait aoAsync

-- | Begin running an asynchronous action, if it has not already begun.
-- Then wait for it to complete, and return its value.
-- If the action threw an exception, then the exception is returned.
waitCatch :: AsyncOnce a -> IO (Done a)
waitCatch ao@AsyncOnce{ aoAsync } = start ao *> (either Failure Success <$> Async.waitCatch aoAsync)

-- | Check whether an asynchronous action has completed yet.
-- If the action has not yet started, it remains unstarted.
poll :: AsyncOnce a -> IO (Poll a)
poll AsyncOnce{ aoAsync } = maybe Incomplete (Done . either Failure Success) <$> Async.poll aoAsync

cancel :: AsyncOnce a -> IO ()
cancel AsyncOnce{ aoAsync } = Async.cancel aoAsync
