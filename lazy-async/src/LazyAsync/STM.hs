{-# language Safe #-}

module LazyAsync.STM
  ( {- * Asynchronous actions -}  LazyAsync,
    {- * Combining actions    -}  apply,
    {- * Starting             -}  start,
    {- * Waiting              -}  waitCatch, Outcome (..), applyOutcome,
    {- * Polling              -}  poll, Status (..), applyStatus
  ) where

import           LazyAsync (LazyAsync, Outcome (..), Status (..), apply,
                            applyOutcome, applyStatus)
import qualified LazyAsync as LA

import Control.Concurrent.STM (STM)

-- | Checks whether an asynchronous action has completed yet
--
-- Does __not__ start the action
poll :: LazyAsync a -> STM (Status a)
poll = LA.pollSTM

-- | Starts an asynchronous action, if it has not already been started
start :: LazyAsync a -> STM ()
start = LA.startSTM

-- | Waits for the action to complete, and returns its value or exception
--
-- Does __not__ start the action
waitCatch :: LazyAsync a -> STM (Outcome a)
waitCatch = LA.waitCatchSTM
