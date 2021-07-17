{-# language Trustworthy #-}

module LazyAsync.Async (Async, pollSTM, withAsync) where

import Control.Concurrent.Async.Lifted
