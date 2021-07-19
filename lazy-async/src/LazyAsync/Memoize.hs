module LazyAsync.Memoize where

import Control.Monad               (fmap, (>>=))
import Control.Monad.IO.Class      (MonadIO)
import Control.Monad.Trans.Cont    (ContT)
import Control.Monad.Trans.Control (MonadBaseControl, restoreM)
import LazyAsync.Spawning          (lazyAsync, lazyAsyncIO)
import LazyAsync.Waiting           (startWait, startWaitIO)
import System.IO                   (IO)

{- | Creates a situation wherein:

  * The action shall begin running only once the memoized action runs
  * The action shall run at most once
  * The action shall run only within the continuation (when the continuation ends, the action is stopped)
-}
memoize :: (MonadIO m, MonadBaseControl IO m) =>
    m a -- ^ Action
    -> ContT r m (m a) -- ^ Memoized action, in a continuation
memoize action = fmap (\la -> startWait la >>= restoreM) (lazyAsync action)

-- | Specialization of 'memoize'
memoizeIO :: IO a -> ContT r IO (IO a)
memoizeIO action = fmap startWaitIO (lazyAsyncIO action)
