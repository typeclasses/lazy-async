{-# language Safe #-}

module LazyAsync.Starting where

import Control.Applicative    ((*>))
import Control.Concurrent.STM (STM, atomically, writeTVar)
import Control.Monad          (return)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bool              (Bool (True))
import Data.Function          (($))
import LazyAsync.LazyAsync    (LazyAsync (A0, A1, A2))
import System.IO              (IO)

-- | Starts an asynchronous action, if it has not already been started
start :: MonadIO m => LazyAsync a -> m ()
start A0{}     = return ()
start (A1 s _) = liftIO $ atomically $ writeTVar s True
start (A2 x y) = start x *> start y

-- | Specialization of 'start'
startIO :: LazyAsync a -> IO ()
startIO = start

-- | Same as 'start', but in 'STM'
startSTM :: LazyAsync a -> STM ()
startSTM A0{}     = return ()
startSTM (A1 s _) = writeTVar s True
startSTM (A2 x y) = startSTM x *> startSTM y
