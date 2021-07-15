module LazyAsync
  ( {- * Asynchronous actions -} LazyAsync,
    {- * Spawning -}             withLazyAsync, withLazyAsyncIO,
    {- * Getting results -}      startWait, startWaitIO,
    {- * Catching exceptions -}  startWaitCatch, startWaitCatchIO, Outcome (..),
    {- * Polling -}              poll, pollIO, Status (..),
    {- * Starting manually -}    start, startIO, wait, waitIO,
                                 waitCatch, waitCatchIO,
    {- * Transactions -}         pollSTM, startSTM, waitCatchSTM
  ) where

import LazyAsync.LazyAsync (LazyAsync)
import LazyAsync.Outcome   (Outcome (..))
import LazyAsync.Polling   (poll, pollIO, pollSTM)
import LazyAsync.Spawning  (withLazyAsync, withLazyAsyncIO)
import LazyAsync.Starting  (start, startIO, startSTM)
import LazyAsync.Status    (Status (..))
import LazyAsync.Waiting   (startWait, startWaitCatch, startWaitCatchIO,
                            startWaitIO, wait, waitCatch, waitCatchIO,
                            waitCatchSTM, waitIO)
