{-# language Safe #-}

{- |

__What is this__ — A 'LazyAsync' is an action that doesn't start right away.
When it does run, it runs in a separate thread.

__How to get one__ — The 'lazyAsync' function makes a 'LazyAsync' available
within a 'ContT' context because it ensures the asynchronous action is cancelled
when the continuation ends, to avoid accidentally leaving any unneeded threads
running in the background.

__How to use it__ — You can incite a 'LazyAsync' to begin by using 'start', and
then you can use 'wait' to block until it completes. There is also 'startWait',
which does both.

If the only thing you ever do with your 'LazyAsync's is 'startWait' on them,
then you may consider using 'memoize' instead, which does not require
interacting with the 'LazyAsync' type at all.

-}

module LazyAsync
  ( {- * LazyAsync            -}  LazyAsync,
    {- * Spawning             -}  lazyAsync,
    {- * Getting results      -}  startWait,
    {- * Combining actions    -}  apply, choose,
    {- * Catching (Outcome)   -}  startWaitCatch, Outcome (..),
                                  applyOutcome, chooseOutcome,
    {- * Polling (Status)     -}  poll, Status (..),
                                  applyStatus, chooseStatus,
    {- * Starting manually    -}  start, wait, waitCatch,
    {- * Transactions         -}  pollSTM, startSTM, waitCatchSTM,
    {- * Memoization          -}  memoize,
    {- * Notes on monads      -}  {- $monads -}
    {- * Unlifted variants    -}  {- $unlifted -}
                                  startWaitIO, startWaitCatchIO,
                                  startIO, pollIO, waitIO, waitCatchIO,
                                  withLazyAsyncIO, withMemoizedIO,
    {- * Re-exports           -}  {- $re-exports -}
                                  ContT (ContT, runContT), evalContT,
                                  MonadBaseControl (liftBaseWith, restoreM, StM),
                                  MonadBase (liftBase),
                                  MonadIO (liftIO)
  ) where

import LazyAsync.Memoize
import LazyAsync.Polling
import LazyAsync.Spawning
import LazyAsync.Starting
import LazyAsync.Types.LazyAsync
import LazyAsync.Types.Outcome
import LazyAsync.Types.Status
import LazyAsync.Waiting

import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Control

{- $monads

__Working with ContT__ — Compose actions within the 'ContT' monadic context, and
apply 'evalContT' at the top to run the continuation. You can also apply
'runContT' to a 'ContT' action to convert it to a "continuation-passing style"
higher-order function.

__Working with MonadBaseControl and StM__ — Most of the functions in this module
are generalized using 'MonadBaseControl', which allows you to work in monads
other than 'System.IO.IO' (to see an example of this, see the test suite for
this package, which creates 'LazyAsync's in Hedgehog's @PropertyT@ context).
'StM' is a type family which often "disappears" (that is, @'StM' m a ~ a@ for
many @m@).

-}

{- $unlifted

If you are uninterested in monad transformers, you may prefer the
functions in this section.

  * All of the @m@ type variables are herein specialized to 'System.IO.IO'.
    This eliminates 'MonadBase', 'MonadBaseControl', 'MonadIO', and 'StM'
    from the types.

  * Async spawning is done with explicit continuation passing instead of
    'ContT' actions.

-}

{- $re-exports

Some key monad lifting concepts from other
packages are re-exported from this module.

__base__ ("Control.Monad.IO.Class")

  * 'MonadIO'
  * 'liftIO'

__transformers__  ("Control.Monad.Trans.Cont")

  * 'ContT'
  * 'runContT'
  * 'evalContT'

__monad-base__ ("Control.Monad.Base")

  * 'MonadBase'
  * 'liftBase'

__monad-control__  ("Control.Monad.Trans.Control")

  * 'MonadBaseControl'
  * 'liftBaseWith'
  * 'restoreM'
  * 'StM'

-}
