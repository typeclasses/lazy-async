module AsyncOnce.Conversions where

import AsyncOnce.Done
import AsyncOnce.Poll

import Control.Concurrent.STM (STM, retry)
import Control.Monad (return)
import Control.Exception (throw, SomeException)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import System.IO (IO)

eitherDone :: Either SomeException a -> Done a
eitherDone (Left e) = Failure e
eitherDone (Right x) = Success x

maybeEitherPoll :: Maybe (Either SomeException a) -> Poll a
maybeEitherPoll Nothing = Incomplete
maybeEitherPoll (Just x) = Done (eitherDone x)

pollDoneSTM :: Poll a -> STM (Done a)
pollDoneSTM Incomplete = retry
pollDoneSTM (Done x) = return x

doneSuccess :: Done a -> IO a
doneSuccess (Failure e) = throw e
doneSuccess (Success x) = return x
