{-# language Safe #-}

module LazyAsync.Actions.Spawn ( lazyAsync, withLazyAsyncIO ) where

import LazyAsync.Async (pollSTM, withAsync)

import LazyAsync.Types (LazyAsync (A1), Outcome (..), StartPoll (..),
                        Status (..))

import LazyAsync.Prelude (Applicative ((*>)), Bool (..), ContT (..),
                          Either (..), Functor (fmap), IO, Maybe (..),
                          MonadBase (..), MonadBaseControl (StM), MonadIO (..),
                          SomeException, TVar, atomically, check, lift,
                          newTVarIO, readTVar, return, writeTVar, (<&>), (>>=))

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
