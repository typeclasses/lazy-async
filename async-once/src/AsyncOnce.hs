module AsyncOnce
  ( {- * Asynchronous actions -}     AsyncOnce,
    {- * Spawning -}                 withAsyncOnce,
    {- * Getting results -}          wait,
        {- ** Catching exceptions -} waitCatch, Done (..),
        {- ** Polling -}             poll, Poll (..),
    {- * Starting manually -}        start,
    {- * Transactions -}             pollSTM, startSTM
  ) where

import AsyncOnce.AsyncOnce (AsyncOnce)
import AsyncOnce.Done      (Done (..))
import AsyncOnce.PollType  (Poll (..))
import AsyncOnce.Polling   (poll, pollSTM)
import AsyncOnce.Spawning  (withAsyncOnce)
import AsyncOnce.Starting  (start, startSTM)
import AsyncOnce.Waiting   (wait, waitCatch)
