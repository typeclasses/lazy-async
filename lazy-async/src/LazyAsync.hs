{-# language Safe #-}

module LazyAsync
  ( {- * Asynchronous actions -}  LazyAsync,
    {- * Spawning             -}  withLazyAsync,
    {- * Getting results      -}  startWait,
    {- * Combining actions    -}  apply,
    {- * Catching exceptions  -}  startWaitCatch, Outcome (..), applyOutcome,
    {- * Polling              -}  poll, Status (..), applyStatus,
    {- * Starting manually    -}  start, wait, waitCatch,
    {- * Transactions         -}  pollSTM, startSTM, waitCatchSTM,
    {- * IO specializations   -}  withLazyAsyncIO, startWaitIO, startWaitCatchIO,
                                  startIO, pollIO, waitIO, waitCatchIO,
    {- * Cont -}                  lazyAsyncCont, lazyAsyncContIO
  ) where

import LazyAsync.LazyAsync (LazyAsync, apply)
import LazyAsync.Outcome   (Outcome (..), applyOutcome)
import LazyAsync.Polling   (poll, pollIO, pollSTM)
import LazyAsync.Spawning  (lazyAsyncCont, lazyAsyncContIO, withLazyAsync,
                            withLazyAsyncIO)
import LazyAsync.Starting  (start, startIO, startSTM)
import LazyAsync.Status    (Status (..), applyStatus)
import LazyAsync.Waiting   (startWait, startWaitCatch, startWaitCatchIO,
                            startWaitIO, wait, waitCatch, waitCatchIO,
                            waitCatchSTM, waitIO)
