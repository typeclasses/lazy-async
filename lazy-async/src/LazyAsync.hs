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
  ( {- * Asynchronous actions -}  LazyAsync,
    {- * Spawning             -}  lazyAsync,
    {- * Getting results      -}  startWait,
    {- * Combining actions    -}  apply,
    {- * Catching exceptions  -}  startWaitCatch, Outcome (..),
                                  applyOutcome, chooseOutcome,
    {- * Polling              -}  poll, Status (..),
                                  applyStatus, chooseStatus,
    {- * Starting manually    -}  start, wait, waitCatch,
    {- * Transactions         -}  pollSTM, startSTM, waitCatchSTM,
    {- * Memoization          -}  memoize,
    {- * Notes on monads      -}  {- $monads -}
    {- * IO specializations   -}  {- $io -}
                                  startWaitIO, startWaitCatchIO,
                                  startIO, pollIO, waitIO, waitCatchIO,
                                  lazyAsyncIO, memoizeIO,
    {- * Re-exports           -}  {- $re-exports -}
                                  ContT (ContT, runContT), evalContT,
                                  MonadBaseControl (liftBaseWith, restoreM, StM),
                                  MonadBase (liftBase),
                                  MonadIO (liftIO)
  ) where

import LazyAsync.LazyAsync
import LazyAsync.Memoize
import LazyAsync.Outcome
import LazyAsync.Polling
import LazyAsync.Spawning
import LazyAsync.Starting
import LazyAsync.Status
import LazyAsync.Waiting

import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Control

{- $monads

__Working with ContT__ — Compose actions within the 'ContT' monadic context, and
apply 'evalContT' at the top to run the continuation. If you do not want to work
within 'ContT', then apply 'runContT' to the result of 'lazyAsync' to convert to
explicit continuation-passing style.

__Working with MonadBaseControl and StM__ — Most of the functions in this module
are generalized using 'MonadBaseControl', which allows you to work in monads
other than 'System.IO.IO' (to see an example of this, see the test suite for
this package, which creates 'LazyAsync's in Hedgehog's @PropertyT@ context).
'StM' is a type family which often "disappears" (that is, @StM m a ~ a@ for many
@m@).

-}

{- $io

If you have any difficulty understanding the 'MonadBaseControl' constraints in
the functions above, you may benefit from this section, in which all of the @m@
type variables are replaced with 'System.IO.IO'.

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
