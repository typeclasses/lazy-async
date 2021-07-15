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
eitherDone = \case{ Left e -> Failure e; Right x -> Success x }

maybeEitherPoll :: Maybe (Either SomeException a) -> Poll a
maybeEitherPoll = \case{ Nothing -> Incomplete; Just x -> Done (eitherDone x) }

pollDoneSTM :: Poll a -> STM (Done a)
pollDoneSTM = \case{ Incomplete -> retry; Done x -> return x }

doneSuccess :: Done a -> IO a
doneSuccess = \case{ Failure e -> throw e; Success x -> return x }
