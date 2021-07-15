module LazyAsync.Spawning (withLazyAsync) where

import Control.Applicative         ((*>))
import Control.Concurrent.Async    (withAsync)
import Control.Concurrent.STM      (atomically, check)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar)
import Control.Monad               (return, (>>=))
import Control.Monad.Trans.Class   (lift)
import Control.Monad.Trans.Cont    (ContT (ContT, runContT))
import Data.Bool                   (Bool (False))
import Data.Function               (($))
import LazyAsync.LazyAsync         (LazyAsync (A1))
import System.IO                   (IO)

withLazyAsync :: IO a -> (LazyAsync a -> IO b) -> IO b
withLazyAsync action = runContT $ do
    s <- lift $ newTVarIO False
    a <- ContT $ withAsync $ waitForTrueIO s *> action
    return $ A1 s a

waitForTrueIO :: TVar Bool -> IO ()
waitForTrueIO x = atomically $ readTVar x >>= check
