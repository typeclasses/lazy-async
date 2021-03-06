{-# language Safe #-}

module LazyAsync.Actions.StartWait where

import LazyAsync.Actions.Start (start)
import LazyAsync.Actions.Wait  (wait, waitCatch)

import LazyAsync.Types (LazyAsync, Outcome)

import LazyAsync.Prelude (Applicative ((*>)), IO, MonadBaseControl (StM),
                          MonadIO)

-- | π Starts an asynchronous action,
-- βΈοΈ waits for it to complete, and
-- β returns its value
--
-- π£ If the action throws an exception, then the exception is re-thrown
--
-- π @('startWait' x)@ is equivalent to @('start' x '*>' 'wait' x)@
startWait :: (MonadBaseControl base m, MonadIO base) => LazyAsync (StM m a) -> m a
startWait x = start x *> wait x

-- | Akin to 'startWait'
startWaitIO :: LazyAsync a -> IO a
startWaitIO = startWait

-- | π Starts an asynchronous action,
-- βΈοΈ waits for it to complete, and
-- β returns its value
--
-- π£ If the action throws an exception, then the exception is returned
--
-- π @('startWaitCatch' x)@ is equivalent to @('start' x '*>' 'waitCatch' x)@
startWaitCatch :: (MonadBaseControl base m, MonadIO base) => LazyAsync (StM m a) -> m (Outcome a)
startWaitCatch x = start x *> waitCatch x

-- | Akin to 'startWaitCatch'
startWaitCatchIO :: LazyAsync a -> IO (Outcome a)
startWaitCatchIO = startWaitCatch
