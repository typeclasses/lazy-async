module LazyAsync
  ( {- * Asynchronous actions -} LazyAsync,
    {- * Spawning -}             withLazyAsync,
    {- * Getting results -}      startWait,
    {- * Catching exceptions -}  startWaitCatch, Outcome (..),
    {- * Polling -}              poll, Status (..),
    {- * Starting manually -}    start, wait, waitCatch,
    {- * Transactions -}         pollSTM, startSTM, waitCatchSTM,
    {- * IO specializations -}   withLazyAsyncIO, startWaitIO, startWaitCatchIO,
                                 startIO, pollIO, waitIO, waitCatchIO
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
