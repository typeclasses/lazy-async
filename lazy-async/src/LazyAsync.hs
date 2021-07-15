module LazyAsync
  ( {- * Asynchronous actions -}     LazyAsync,
    {- * Spawning -}                 withLazyAsync,
    {- * Getting results -}          wait,
        {- ** Catching exceptions -} waitCatch, Outcome (..),
        {- ** Polling -}             poll, Status (..),
    {- * Starting manually -}        start,
    {- * Transactions -}             pollSTM, startSTM
  ) where

import LazyAsync.LazyAsync (LazyAsync)
import LazyAsync.Outcome   (Outcome (..))
import LazyAsync.Polling   (poll, pollSTM)
import LazyAsync.Spawning  (withLazyAsync)
import LazyAsync.Starting  (start, startSTM)
import LazyAsync.Status    (Status (..))
import LazyAsync.Waiting   (wait, waitCatch)
