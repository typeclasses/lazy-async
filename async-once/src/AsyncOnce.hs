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

-- relude
import Relude

-- base
import Control.Exception (throw)

-- async
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.Async (Async)

-- stm
import Control.Concurrent.STM (check)
import qualified Control.Concurrent.STM as STM

-- transformers
import Control.Monad.Trans.Cont

data AsyncOnce a = A0 a | A1 (A1 a) | forall x. A2 (AsyncOnce (x -> a)) (AsyncOnce x)

data A1 a = AsyncOnce{ aoStart :: TVar Bool, aoAsync :: Async a }
    deriving Functor

data Poll a = Incomplete | Done (Done a)
    deriving Functor

data Done a = Failure SomeException | Success a
    deriving Functor

instance Functor AsyncOnce where
    fmap f = \case
        A0 x -> A0 (f x)
        A1 x -> A1 (fmap f x)
        A2 x y -> A2 (fmap (fmap f) x) y

instance Applicative Done where
    pure = Success

    Failure e <*> _         = Failure e
    _         <*> Failure e = Failure e
    Success f <*> Success x = Success $ f x

instance Applicative Poll where
    pure = Done . pure

    Done (Failure e) <*> _                = Done $ Failure e
    _                <*> Done (Failure e) = Done $ Failure e
    Done (Success f) <*> Done (Success x) = Done $ Success $ f x
    _                <*> _                = Incomplete

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


---  Boring internal conversions  ---

eitherDone :: Either SomeException a -> Done a
eitherDone = \case{ Left e -> Failure e; Right x -> Success x }

maybeEitherPoll :: Maybe (Either SomeException a) -> Poll a
maybeEitherPoll = \case{ Nothing -> Incomplete; Just x -> Done (eitherDone x) }

pollDoneSTM :: Poll a -> STM (Done a)
pollDoneSTM = \case{ Incomplete -> STM.retry; Done x -> return x }

doneSuccess :: Done a -> IO a
doneSuccess = \case{ Failure e -> throw e; Success x -> return x }
