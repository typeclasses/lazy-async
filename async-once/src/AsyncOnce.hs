module AsyncOnce
  ( -- * Asynchronous actions
    AsyncOnce
    -- * Spawning
  , withAsyncOnce
    -- * Getting results
  , wait
    -- ** Catching exceptions
  , waitCatch, Done (..)
    -- ** Polling
  , poll, Poll (..)
    -- * Starting manually
  , start
    -- * Transactions
  , pollSTM
  ) where

import AsyncOnce.AsyncOnce
import AsyncOnce.Done
import AsyncOnce.PollType
import AsyncOnce.Polling
import AsyncOnce.Spawning
import AsyncOnce.Starting
import AsyncOnce.Waiting
