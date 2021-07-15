module AsyncOnce.Spawning (withAsyncOnce) where

import AsyncOnce.AsyncOnce

-- relude
import Relude

import Control.Concurrent.Async ( withAsync )
import Control.Concurrent.STM (check)
import Control.Monad.Trans.Cont ( ContT(ContT, runContT) )

withAsyncOnce :: IO a -> (AsyncOnce a -> IO b) -> IO b
withAsyncOnce action = runContT $ do
    s <- newTVarIO False
    a <- ContT $ withAsync $ waitForTrueIO s *> action
    return $ A1 s a

waitForTrueIO :: TVar Bool -> IO ()
waitForTrueIO x = atomically $ readTVar x >>= check
