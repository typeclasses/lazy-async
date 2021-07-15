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

data AsyncOnce a =
    A0 a
  | A1 (TVar Bool) (Async a)
  | forall x. A2 (AsyncOnce (x -> a)) (AsyncOnce x)

instance Functor AsyncOnce where
    f `fmap` A0 x = A0 (f x)
    f `fmap` A1 s a = A1 s (fmap f a)
    f `fmap` A2 x y = A2 (fmap (fmap f) x) y

instance Applicative AsyncOnce where
    pure = A0
    (<*>) = A2

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

-- | Check whether an asynchronous action has completed yet.
-- If the action has not yet started, it remains unstarted.
poll :: AsyncOnce a -> IO (Poll a)
poll = atomically . pollSTM

-- | Same as 'poll', but in 'STM'
pollSTM :: AsyncOnce a -> STM (Poll a)
pollSTM (A0 x) = return $ pure x
pollSTM (A1 _ a) = Async.pollSTM a <&> maybeEitherPoll
pollSTM (A2 x y) = getCompose $ Compose (pollSTM x) <*> Compose (pollSTM y)
