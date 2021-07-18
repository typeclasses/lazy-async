module LazyAsync.Memoize where

import Control.Monad.IO.Class      (MonadIO)
import Control.Monad.Trans.Cont    (ContT)
import Control.Monad.Trans.Control (MonadBaseControl, StM)
import Data.Functor                (fmap)
import LazyAsync.Spawning          (lazyAsyncCont, lazyAsyncContIO)
import LazyAsync.Waiting           (startWait, startWaitIO)
import System.IO                   (IO)

memoize :: (MonadIO m, MonadBaseControl IO m) => m a -> ContT r m (m (StM m a))
memoize action = fmap startWait (lazyAsyncCont action)

-- Specialization of 'memoize'
memoizeIO :: IO a -> ContT r IO (IO a)
memoizeIO action = fmap startWaitIO (lazyAsyncContIO action)
