module LazyAsync.Conversions where

import Control.Concurrent.STM (STM, retry)
import Control.Exception      (SomeException)
import Control.Monad          (return)
import Control.Monad.Catch    (MonadThrow, throwM)
import Data.Either            (Either (Left, Right))
import Data.Maybe             (Maybe (Just, Nothing))
import LazyAsync.Outcome      (Outcome (Failure, Success))
import LazyAsync.Status       (Status (Done, Incomplete))

eitherDone :: Either SomeException a -> Outcome a
eitherDone (Left e)  = Failure e
eitherDone (Right x) = Success x

maybeEitherStatus :: Maybe (Either SomeException a) -> Status a
maybeEitherStatus Nothing  = Incomplete
maybeEitherStatus (Just x) = Done (eitherDone x)

statusOutcomeSTM :: Status a -> STM (Outcome a)
statusOutcomeSTM Incomplete = retry
statusOutcomeSTM (Done x)   = return x

outcomeSuccess :: MonadThrow m => Outcome a -> m a
outcomeSuccess (Failure e) = throwM e
outcomeSuccess (Success x) = return x
