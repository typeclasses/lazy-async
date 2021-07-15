module LazyAsync
  ( {- * Asynchronous actions -}     LazyAsync,
    {- * Spawning -}                 withLazyAsync,
    {- * Getting results -}          wait,
        {- ** Catching exceptions -} waitCatch, Done (..),
        {- ** Polling -}             poll, Poll (..),
    {- * Starting manually -}        start,
    {- * Transactions -}             pollSTM, startSTM
  ) where

import LazyAsync.Done      (Done (..))
import LazyAsync.LazyAsync (LazyAsync)
import LazyAsync.PollType  (Poll (..))
import LazyAsync.Polling   (poll, pollSTM)
import LazyAsync.Spawning  (withLazyAsync)
import LazyAsync.Starting  (start, startSTM)
import LazyAsync.Waiting   (wait, waitCatch)
