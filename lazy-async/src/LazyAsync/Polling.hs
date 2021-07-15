module LazyAsync.Polling where

import Control.Applicative    (pure, (<*>))
import Control.Concurrent.STM (STM, atomically)
import Control.Monad          (return)
import Data.Function          (($), (.))
import Data.Functor           ((<&>))
import Data.Functor.Compose   (Compose (Compose, getCompose))
import LazyAsync.Conversions  (maybeEitherStatus)
import LazyAsync.LazyAsync    (LazyAsync (A0, A1, A2))
import LazyAsync.Status       (Status)
import System.IO              (IO)

import qualified Control.Concurrent.Async as Async

-- | Check whether an asynchronous action has completed yet.
-- This does __not__ start the action.
poll :: LazyAsync a -> IO (Status a)
poll = atomically . pollSTM

-- | Same as 'poll', but in 'STM'
pollSTM :: LazyAsync a -> STM (Status a)
pollSTM (A0 x)   = return $ pure x
pollSTM (A1 _ a) = Async.pollSTM a <&> maybeEitherStatus
pollSTM (A2 x y) = getCompose $ Compose (pollSTM x) <*> Compose (pollSTM y)
