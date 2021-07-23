{-# language Safe #-}

module LazyAsync.Starting where

import Control.Applicative    ((*>))
import Control.Concurrent.STM (STM, atomically)
import Control.Monad          (return)
import Control.Monad.Base     (MonadBase, liftBase)
import Control.Monad.IO.Class (MonadIO, liftIO)
import LazyAsync.LazyAsync    (LazyAsync (..), StartPoll (StartPoll))
import System.IO              (IO)

-- | Starts an asynchronous action, if it has not already been started
start :: (MonadBase base m, MonadIO base) => LazyAsync a -> m ()
start A0{}                 = return ()
start (A1 (StartPoll s _)) = liftBase (liftIO (atomically s))
start (Ap x y)             = start x *> start y
start (Choose x y)         = start x *> start y
start Empty                = return ()

-- | Akin to 'start'
startIO :: LazyAsync a -> IO ()
startIO = start

-- | Akin to 'start'
startSTM :: LazyAsync a -> STM ()
startSTM A0{}                 = return ()
startSTM (A1 (StartPoll s _)) = s
startSTM (Ap x y)             = startSTM x *> startSTM y
startSTM (Choose x y)         = startSTM x *> startSTM y
startSTM Empty                = return ()
