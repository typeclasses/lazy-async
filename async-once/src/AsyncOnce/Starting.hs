module AsyncOnce.Starting where

import AsyncOnce.AsyncOnce    (AsyncOnce (A0, A1, A2))
import Control.Applicative    ((*>))
import Control.Concurrent.STM (STM, atomically, writeTVar)
import Control.Monad          (return)
import Data.Bool              (Bool (True))
import Data.Function          (($))
import System.IO              (IO)

-- | Begin running an asynchronous action, if it has not already begun.
start :: AsyncOnce a -> IO ()
start A0{}     = return ()
start (A1 s _) = atomically $ writeTVar s True
start (A2 x y) = start x *> start y

-- | Same as 'start', but in 'STM'
startSTM :: AsyncOnce a -> STM ()
startSTM A0{}     = return ()
startSTM (A1 s _) = writeTVar s True
startSTM (A2 x y) = startSTM x *> startSTM y
