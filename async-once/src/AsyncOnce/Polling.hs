module AsyncOnce.Polling where

import AsyncOnce.AsyncOnce    (AsyncOnce (A0, A1, A2))
import AsyncOnce.Conversions  (maybeEitherPoll)
import AsyncOnce.PollType     (Poll)
import Control.Applicative    (pure, (<*>))
import Control.Concurrent.STM (STM, atomically)
import Control.Monad          (return)
import Data.Function          (($), (.))
import Data.Functor           ((<&>))
import Data.Functor.Compose   (Compose (Compose, getCompose))
import System.IO              (IO)

import qualified Control.Concurrent.Async as Async

-- | Check whether an asynchronous action has completed yet.
-- If the action has not yet started, it remains unstarted.
poll :: AsyncOnce a -> IO (Poll a)
poll = atomically . pollSTM

-- | Same as 'poll', but in 'STM'
pollSTM :: AsyncOnce a -> STM (Poll a)
pollSTM (A0 x)   = return $ pure x
pollSTM (A1 _ a) = Async.pollSTM a <&> maybeEitherPoll
pollSTM (A2 x y) = getCompose $ Compose (pollSTM x) <*> Compose (pollSTM y)
