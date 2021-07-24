{-# language Trustworthy #-}

module LazyAsync.Async (Async, pollSTM, withAsync, async, cancel) where

import Control.Concurrent.Async.Lifted
