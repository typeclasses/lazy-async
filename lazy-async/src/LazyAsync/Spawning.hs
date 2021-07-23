{-# language Safe #-}

module LazyAsync.Spawning( lazyAsync, lazyAsyncIO ) where

import Control.Applicative         ((*>))
import Control.Concurrent.STM      (atomically, check)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Exception           (SomeException)
import Control.Monad.IO.Class
import Control.Monad               ((>>=))
import Control.Monad.Base          (MonadBase, liftBase)
import Control.Monad.Trans.Cont    (ContT (ContT))
import Control.Monad.Trans.Control (MonadBaseControl, StM)
import Data.Bool                   (Bool (..))
import Data.Either                 (Either (..))
import Data.Functor                ((<&>))
import Data.Maybe                  (Maybe (..))
import LazyAsync.Async             (pollSTM, withAsync)
import LazyAsync.LazyAsync         (LazyAsync (A1))
import LazyAsync.Outcome           (Outcome (..))
import LazyAsync.Status            (Status (..))
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
lazyAsyncIO = lazyAsync

withLazyAsync :: MonadBaseControl IO m =>
    m a -- ^ Action
    -> (LazyAsync (StM m a) -> m b) -- ^ Continuation
    -> m b
withLazyAsync action continue =
    newTVar False >>= \s -> withAsync (waitForTrue s *> action) (\a -> continue (A1 (writeTVar s True) (pollSTM a <&> maybeEitherStatus)))

waitForTrue :: (MonadBase base m, MonadIO base) => TVar Bool -> m ()
waitForTrue x = liftBase (liftIO (atomically (readTVar x >>= check)))

newTVar :: (MonadBase base m, MonadIO base) => a -> m (TVar a)
newTVar x = liftBase (liftIO (newTVarIO x))

maybeEitherStatus :: Maybe (Either SomeException a) -> Status a
maybeEitherStatus Nothing  = Incomplete
maybeEitherStatus (Just x) = Done (eitherDone x)

eitherDone :: Either SomeException a -> Outcome a
eitherDone (Left e)  = Failure e
eitherDone (Right x) = Success x
