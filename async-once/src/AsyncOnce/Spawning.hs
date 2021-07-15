module AsyncOnce.Spawning (withAsyncOnce) where

import AsyncOnce.AsyncOnce         (AsyncOnce (A1))
import Control.Applicative         ((*>))
import Control.Concurrent.Async    (withAsync)
import Control.Concurrent.STM      (atomically, check)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar)
import Control.Monad               (return, (>>=))
import Control.Monad.Trans.Class   (lift)
import Control.Monad.Trans.Cont    (ContT (ContT, runContT))
import Data.Bool                   (Bool (False))
import Data.Function               (($))
import System.IO                   (IO)

withAsyncOnce :: IO a -> (AsyncOnce a -> IO b) -> IO b
withAsyncOnce action = runContT $ do
    s <- lift $ newTVarIO False
    a <- ContT $ withAsync $ waitForTrueIO s *> action
    return $ A1 s a

waitForTrueIO :: TVar Bool -> IO ()
waitForTrueIO x = atomically $ readTVar x >>= check
