{-# language Safe #-}

module LazyAsync.Actions.StartWait where

import Control.Applicative         ((*>))
import Control.Monad.IO.Class      (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl, StM)
import LazyAsync.Actions.Start     (start)
import LazyAsync.Actions.Wait      (wait, waitCatch)
import LazyAsync.Types.LazyAsync   (LazyAsync)
import LazyAsync.Types.Outcome     (Outcome)
import System.IO                   (IO)

-- | Starts an asynchronous action, waits for it to complete, and returns its value
--
-- If the action throws an exception, then the exception is re-thrown
--
-- @('startWait' x)@ is equivalent to @('start' x '*>' 'wait' x)@
startWait :: (MonadBaseControl base m, MonadIO base) => LazyAsync (StM m a) -> m a
startWait x = start x *> wait x

-- | Akin to 'startWait'
startWaitIO :: LazyAsync a -> IO a
startWaitIO = startWait

-- | Starts an asynchronous action, waits for it to complete, and returns its value
--
-- If the action throws an exception, then the exception is returned
--
-- @('startWaitCatch' x)@ is equivalent to @('start' x '*>' 'waitCatch' x)@
startWaitCatch :: (MonadBaseControl base m, MonadIO base) => LazyAsync (StM m a) -> m (Outcome a)
startWaitCatch x = start x *> waitCatch x

-- | Akin to 'startWaitCatch'
startWaitCatchIO :: LazyAsync a -> IO (Outcome a)
startWaitCatchIO = startWaitCatch
