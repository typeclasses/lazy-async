module LazyAsync
  ( {- * Asynchronous actions -} LazyAsync,
    {- * Spawning -}             withLazyAsync,
    {- * Getting results -}      startWait,
    {- * Catching exceptions -}  startWaitCatch, Outcome (..),
    {- * Polling -}              poll, Status (..),
    {- * Starting manually -}    start, wait, waitCatch,
    {- * Transactions -}         pollSTM, startSTM, waitCatchSTM
  ) where

import LazyAsync.LazyAsync (LazyAsync)
import LazyAsync.Outcome   (Outcome (..))
import LazyAsync.Polling   (poll, pollSTM)
import LazyAsync.Spawning  (withLazyAsync)
import LazyAsync.Starting  (start, startSTM)
import LazyAsync.Status    (Status (..))
import LazyAsync.Waiting   (startWait, startWaitCatch, wait, waitCatch, waitCatchSTM)
