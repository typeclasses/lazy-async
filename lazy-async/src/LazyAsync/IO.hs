{-# language Safe #-}

module LazyAsync.IO
  ( {- * Asynchronous actions -}  LazyAsync,
    {- * Spawning             -}  withLazyAsync,
    {- * Getting results      -}  startWait,
    {- * Combining actions    -}  apply,
    {- * Catching exceptions  -}  startWaitCatch, Outcome (..), applyOutcome,
    {- * Polling              -}  poll, Status (..), applyStatus,
    {- * Starting manually    -}  start, wait, waitCatch
  ) where

import           LazyAsync (LazyAsync, Outcome (..), Status (..), apply,
                            applyOutcome, applyStatus)
import qualified LazyAsync as LA

import System.IO (IO)

{- | Creates a situation wherein:

  * The action shall begin running only once it is needed (that is, until prompted by 'start')
  * The action shall run asynchronously (other than where it is 'wait'ed upon)
  * The action shall run at most once
  * The action shall run only within the continuation (when the continuation ends, the action is stopped)
-}
withLazyAsync ::
    IO a  -- ^ Action
    -> (LazyAsync a -> IO b) -- ^ Continuation
    -> IO b
withLazyAsync = LA.withLazyAsyncIO

-- | Starts an asynchronous action, waits for it to complete, and returns its value
--
-- If the action throws an exception, then the exception is re-thrown
--
-- @('startWait' x)@ is equivalent to @('start' x '*>' 'wait' x)@
startWait :: LazyAsync a -> IO a
startWait = LA.startWaitIO

-- | Starts an asynchronous action, waits for it to complete, and returns its value
--
-- If the action throws an exception, then the exception is returned
--
-- @('startWaitCatch' x)@ is equivalent to @('start' x '*>' 'waitCatch' x)@
startWaitCatch :: LazyAsync a -> IO (Outcome a)
startWaitCatch = LA.startWaitCatchIO

-- | Checks whether an asynchronous action has completed yet
--
-- Does __not__ start the action
poll :: LazyAsync a -> IO (Status a)
poll = LA.pollIO

-- | Starts an asynchronous action, if it has not already been started
start :: LazyAsync a -> IO ()
start = LA.startIO

-- | Waits for the action to complete, and returns its value
--
-- If the action throws an exception, then the exception is re-thrown
--
-- Does __not__ start the action
wait :: LazyAsync a -> IO a
wait = LA.waitIO

-- | Waits for the action to complete, and returns its value
--
-- If the action throws an exception, then the exception is returned
--
-- Does __not__ start the action
waitCatch :: LazyAsync a -> IO (Outcome a)
waitCatch = LA.waitCatchIO
