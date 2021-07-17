{-# language Safe #-}

module LazyAsync.Polling where

import Control.Applicative    (pure, (<*>))
import Control.Concurrent.STM (STM, atomically)
import Control.Exception      (SomeException)
import Control.Monad          (return)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either            (Either (Left, Right))
import Data.Function          (($), (.))
import Data.Functor           ((<&>))
import Data.Functor.Compose   (Compose (Compose, getCompose))
import Data.Maybe             (Maybe (Just, Nothing))
import LazyAsync.LazyAsync    (LazyAsync (A0, A1, A2))
import LazyAsync.Outcome      (Outcome (Failure, Success))
import LazyAsync.Status       (Status (Done, Incomplete))
import System.IO              (IO)

import qualified LazyAsync.Async as Async

-- | Checks whether an asynchronous action has completed yet
--
-- Does __not__ start the action
poll :: MonadIO m => LazyAsync a -> m (Status a)
poll = liftIO  . pollIO

-- | Specialization of 'poll'
pollIO :: LazyAsync a -> IO (Status a)
pollIO = atomically . pollSTM

-- | Same as 'poll', but in 'STM'
pollSTM :: LazyAsync a -> STM (Status a)
pollSTM (A0 x)   = return $ pure x
pollSTM (A1 _ a) = Async.pollSTM a <&> maybeEitherStatus
pollSTM (A2 x y) = getCompose $ Compose (pollSTM x) <*> Compose (pollSTM y)

eitherDone :: Either SomeException a -> Outcome a
eitherDone (Left e)  = Failure e
eitherDone (Right x) = Success x

maybeEitherStatus :: Maybe (Either SomeException a) -> Status a
maybeEitherStatus Nothing  = Incomplete
maybeEitherStatus (Just x) = Done (eitherDone x)
