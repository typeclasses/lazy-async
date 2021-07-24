{-# language Safe #-}

module LazyAsync.Actions.Start where

import LazyAsync.Types (Complex (..), LazyAsync (..), StartPoll (..))

import LazyAsync.Prelude (Applicative ((*>)), IO, MonadBase (..), MonadIO (..),
                          STM, atomically, return)

-- | Starts an asynchronous action, if it has not already been started
start :: (MonadBase base m, MonadIO base) => LazyAsync a -> m ()
start Pure{}               = return ()
start Empty{}              = return ()
start (A1 (StartPoll s _)) = liftBase (liftIO (atomically s))
start (A2 (Complex _ x y)) = start x *> start y

-- | Akin to 'start'
startIO :: LazyAsync a -> IO ()
startIO = start

-- | Akin to 'start'
startSTM :: LazyAsync a -> STM ()
startSTM Pure{}               = return ()
startSTM Empty{}              = return ()
startSTM (A1 (StartPoll s _)) = s
startSTM (A2 (Complex _ x y)) = startSTM x *> startSTM y
