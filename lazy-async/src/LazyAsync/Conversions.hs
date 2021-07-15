module LazyAsync.Conversions where

import Control.Concurrent.STM (STM, retry)
import Control.Exception      (SomeException, throw)
import Control.Monad          (return)
import Data.Either            (Either (Left, Right))
import Data.Maybe             (Maybe (Just, Nothing))
import LazyAsync.Outcome      (Outcome (Failure, Success))
import LazyAsync.Status       (Status (Done, Incomplete))
import System.IO              (IO)

eitherDone :: Either SomeException a -> Outcome a
eitherDone (Left e)  = Failure e
eitherDone (Right x) = Success x

maybeEitherStatus :: Maybe (Either SomeException a) -> Status a
maybeEitherStatus Nothing  = Incomplete
maybeEitherStatus (Just x) = Done (eitherDone x)

statusOutcomeSTM :: Status a -> STM (Outcome a)
statusOutcomeSTM Incomplete = retry
statusOutcomeSTM (Done x)   = return x

outcomeSuccess :: Outcome a -> IO a
outcomeSuccess (Failure e) = throw e
outcomeSuccess (Success x) = return x
