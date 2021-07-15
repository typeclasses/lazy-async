module LazyAsync
  ( {- * Asynchronous actions -}     LazyAsync,
    {- * Spawning -}                 withLazyAsync,
    {- * Getting results -}          wait,
        {- ** Catching exceptions -} waitCatch, Done (..),
        {- ** Polling -}             poll, Status (..),
    {- * Starting manually -}        start,
    {- * Transactions -}             pollSTM, startSTM
  ) where

import LazyAsync.Done      (Done (..))
import LazyAsync.LazyAsync (LazyAsync)
import LazyAsync.Polling   (poll, pollSTM)
import LazyAsync.Spawning  (withLazyAsync)
import LazyAsync.Starting  (start, startSTM)
import LazyAsync.Status    (Status (..))
import LazyAsync.Waiting   (wait, waitCatch)
