module LazyAsync.Spawning (withLazyAsync) where

import Control.Applicative         ((*>))
import Control.Concurrent.Async    (withAsync)
import Control.Concurrent.STM      (atomically, check)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar)
import Control.Monad               ((>>=))
import Data.Bool                   (Bool (False))
import Data.Function               (($))
import LazyAsync.LazyAsync         (LazyAsync (A1))
import System.IO                   (IO)

withLazyAsync :: IO a -> (LazyAsync a -> IO b) -> IO b
withLazyAsync action continue =
    newTVarIO False >>= \s ->
    withAsync (waitForTrueIO s *> action) $ \a ->
    continue $ A1 s a

waitForTrueIO :: TVar Bool -> IO ()
waitForTrueIO x = atomically $ readTVar x >>= check
