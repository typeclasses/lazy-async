{-# language Safe #-}

{- | __What is this__ — A 'LazyAsync' is an action that doesn't start right
away. When it does run, it runs in a separate thread.

__How to get one__ — The 'lazyAsync' function makes a 'LazyAsync' available
within a 'ContT' context because it ensures the asynchronous action is cancelled
when the continuation ends, to avoid accidentally leaving any unneeded threads
running in the background.

__How to use it__ — You can incite a 'LazyAsync' to begin by using 'start', and
then you can use 'wait' to block until it completes. There is also 'startWait',
which does both. If the only thing you ever do with your 'LazyAsync's is
'startWait' on them, then you may consider using 'memoize' instead, which does
not require interacting with the 'LazyAsync' type at all.

__About the monads__ — Most of the functions in this module are generalized
using 'MonadBaseControl', which allows you to work in monads other than
'System.IO.IO' (to see an example of this, see the test suite for this package,
which creates 'LazyAsync's in Hedgehog's @PropertyT@ context). If you have any
difficulty understanding these constraints, you may benefit from looking at the
"IO specializations" section at the bottom of the module, in which all of the
@m@ type variables are replaced with 'System.IO.IO'. -}

module LazyAsync
  ( {- * Asynchronous actions -}  LazyAsync,
    {- * Spawning             -}  lazyAsync,
    {- * Getting results      -}  startWait,
    {- * Combining actions    -}  apply,
    {- * Catching exceptions  -}  startWaitCatch, Outcome (..), applyOutcome,
    {- * Polling              -}  poll, Status (..), applyStatus,
    {- * Starting manually    -}  start, wait, waitCatch,
    {- * Transactions         -}  pollSTM, startSTM, waitCatchSTM,
    {- * Memoization          -}  memoize,
    {- * IO specializations   -}  startWaitIO, startWaitCatchIO,
                                  startIO, pollIO, waitIO, waitCatchIO,
                                  lazyAsyncIO, memoizeIO,
    {- * Re-exports           -}  ContT (ContT, runContT), evalContT,
                                  MonadBaseControl (liftBaseWith, restoreM, StM),
                                  MonadBase (liftBase)
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
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Control
