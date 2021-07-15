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

import AsyncOnce.Conversions
import AsyncOnce.Done
import AsyncOnce.Poll

-- relude
import Relude

-- async
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.Async (Async)

-- stm
import Control.Concurrent.STM (check)

-- transformers
import Control.Monad.Trans.Cont

data AsyncOnce a = A0 a | A1 (A1 a) | forall x. A2 (AsyncOnce (x -> a)) (AsyncOnce x)

data A1 a = AsyncOnce{ aoStart :: TVar Bool, aoAsync :: Async a }
    deriving Functor

instance Functor AsyncOnce where
    fmap f = \case
        A0 x -> A0 (f x)
        A1 x -> A1 (fmap f x)
        A2 x y -> A2 (fmap (fmap f) x) y

instance Applicative AsyncOnce where
    pure = A0
    (<*>) = A2

withAsyncOnce :: IO a -> (AsyncOnce a -> IO b) -> IO b
withAsyncOnce action =
  runContT $
    do
      aoStart <- newTVarIO False
      aoAsync <- ContT $ Async.withAsync $ waitForTrueIO aoStart *> action
      return $ A1 $ AsyncOnce{ aoStart, aoAsync }

waitForTrueIO :: TVar Bool -> IO ()
waitForTrueIO x = atomically $ readTVar x >>= check

-- | Begin running an asynchronous action, if it has not already begun.
start :: AsyncOnce a -> IO ()
start = \case
    A0{} -> return ()
    A1 AsyncOnce{ aoStart } -> atomically $ writeTVar aoStart True
    A2 x y -> start x *> start y

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

-- | Check whether an asynchronous action has completed yet.
-- If the action has not yet started, it remains unstarted.
poll :: AsyncOnce a -> IO (Poll a)
poll = atomically . pollSTM

-- | Same as 'poll', but in 'STM'
pollSTM :: AsyncOnce a -> STM (Poll a)
pollSTM = \case
    A0 x -> return $ pure x
    A1 AsyncOnce{ aoAsync } -> Async.pollSTM aoAsync <&> maybeEitherPoll
    A2 x y -> getCompose $ Compose (pollSTM x) <*> Compose (pollSTM y)
