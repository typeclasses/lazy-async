module LazyAsync.Conversions where

import Control.Concurrent.STM (STM, retry)
import Control.Exception      (SomeException, throw)
import Control.Monad          (return)
import Data.Either            (Either (Left, Right))
import Data.Maybe             (Maybe (Just, Nothing))
import LazyAsync.Done         (Done (Failure, Success))
import LazyAsync.Status       (Status (Done, Incomplete))
import System.IO              (IO)

eitherDone :: Either SomeException a -> Done a
eitherDone (Left e)  = Failure e
eitherDone (Right x) = Success x

maybeEitherStatus :: Maybe (Either SomeException a) -> Status a
maybeEitherStatus Nothing  = Incomplete
maybeEitherStatus (Just x) = Done (eitherDone x)

statusDoneSTM :: Status a -> STM (Done a)
statusDoneSTM Incomplete = retry
statusDoneSTM (Done x)   = return x

doneSuccess :: Done a -> IO a
doneSuccess (Failure e) = throw e
doneSuccess (Success x) = return x
