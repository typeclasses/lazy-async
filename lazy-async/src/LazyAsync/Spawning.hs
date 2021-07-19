{-# language Safe #-}

module LazyAsync.Spawning( lazyAsync, lazyAsyncIO ) where

import Control.Applicative         ((*>))
import Control.Concurrent.STM      (atomically, check)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar)
import Control.Monad               ((>>=))
import Control.Monad.Base          (MonadBase, liftBase)
import Control.Monad.Trans.Cont    (ContT (ContT))
import Control.Monad.Trans.Control (MonadBaseControl, StM)
import Data.Bool                   (Bool (False))
import LazyAsync.Async             (withAsync)
import LazyAsync.LazyAsync         (LazyAsync (A1))
import System.IO                   (IO)

{- | Creates a situation wherein:

  * The action shall begin running only once it is needed (that is, until prompted by 'LazyAsync.start')
  * The action shall run asynchronously (other than where it is 'LazyAsync.wait'ed upon)
  * The action shall run at most once
  * The action shall run only within the continuation (when the continuation ends, the action is stopped)
-}
lazyAsync :: MonadBaseControl IO m =>
    m a -- ^ Action
    -> ContT r m (LazyAsync (StM m a))
lazyAsync action = ContT (withLazyAsync action)

-- | Specialization of 'lazyAsync'
lazyAsyncIO :: IO a -> ContT r IO (LazyAsync a)
lazyAsyncIO action = ContT (withLazyAsyncIO action)

withLazyAsync :: MonadBaseControl IO m =>
    m a -- ^ Action
    -> (LazyAsync (StM m a) -> m b) -- ^ Continuation
    -> m b
withLazyAsync action continue =
    newTVar False >>= \s -> withAsync (waitForTrue s *> action) (\a -> continue (A1 s a))

-- | Specialization of 'withLazyAsync'
withLazyAsyncIO ::
    IO a  -- ^ Action
    -> (LazyAsync a -> IO b) -- ^ Continuation
    -> IO b
withLazyAsyncIO = withLazyAsync

waitForTrue :: MonadBase IO m => TVar Bool -> m ()
waitForTrue x = liftBase (atomically (readTVar x >>= check))

newTVar :: MonadBase IO m => a -> m (TVar a)
newTVar x = liftBase (newTVarIO x)
