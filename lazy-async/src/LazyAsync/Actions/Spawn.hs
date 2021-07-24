{-# language Safe #-}

module LazyAsync.Actions.Spawn ( lazyAsync, withLazyAsyncIO ) where

import Control.Applicative         ((*>))
import Control.Concurrent.STM      (atomically, check)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Exception           (SomeException)
import Control.Monad               (return, (>>=))
import Control.Monad.Base          (MonadBase, liftBase)
import Control.Monad.IO.Class      (MonadIO, liftIO)
import Control.Monad.Trans.Class   (lift)
import Control.Monad.Trans.Cont    (ContT (ContT), runContT)
import Control.Monad.Trans.Control (MonadBaseControl, StM)
import Data.Bool                   (Bool (..))
import Data.Either                 (Either (..))
import Data.Functor                (fmap, (<&>))
import Data.Maybe                  (Maybe (..))
import LazyAsync.Async             (pollSTM, withAsync)
import LazyAsync.Types.LazyAsync   (LazyAsync (A1))
import LazyAsync.Types.Outcome     (Outcome (..))
import LazyAsync.Types.StartPoll   (StartPoll (..))
import LazyAsync.Types.Status      (Status (..))
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
lazyAsync action = fmap A1 (startPoll action)

startPoll :: MonadBaseControl IO m =>
    m a -- ^ Action
    -> ContT b m (StartPoll (StM m a))
startPoll action =
  do
    s <- lift (newTVar False)
    a <- ContT (withAsync (waitForTrue s *> action))
    return (StartPoll (writeTVar s True) (pollSTM a <&> maybeEitherStatus))

-- | Akin to 'lazyAsync'
withLazyAsyncIO :: IO a -> (LazyAsync a -> IO b) -> IO b
withLazyAsyncIO action = runContT (lazyAsync action)

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
