{-# language Safe #-}

module LazyAsync
  ( {- * Asynchronous actions -}  LazyAsync,
    {- * Spawning             -}  lazyAsync,
    {- * Getting results      -}  startWait,
    {- * Combining actions    -}  apply,
    {- * Catching exceptions  -}  startWaitCatch, Outcome (..), applyOutcome,
    {- * Polling              -}  poll, Status (..), applyStatus,
    {- * Starting manually    -}  start, wait, waitCatch,
    {- * Transactions         -}  pollSTM, startSTM, waitCatchSTM,
    {- * Memoizing IO actions -}  memoize,
    {- * IO specializations   -}  startWaitIO, startWaitCatchIO,
                                  startIO, pollIO, waitIO, waitCatchIO,
                                  lazyAsyncIO, memoizeIO
  ) where

import LazyAsync.LazyAsync (LazyAsync, apply)
import LazyAsync.Memoize   (memoize, memoizeIO)
import LazyAsync.Outcome   (Outcome (..), applyOutcome)
import LazyAsync.Polling   (poll, pollIO, pollSTM)
import LazyAsync.Spawning  (lazyAsync, lazyAsyncIO)
import LazyAsync.Starting  (start, startIO, startSTM)
import LazyAsync.Status    (Status (..), applyStatus)
import LazyAsync.Waiting   (startWait, startWaitCatch, startWaitCatchIO,
                            startWaitIO, wait, waitCatch, waitCatchIO,
                            waitCatchSTM, waitIO)
