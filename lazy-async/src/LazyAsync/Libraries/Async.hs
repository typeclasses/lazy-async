{-# language Trustworthy #-}

module LazyAsync.Libraries.Async (Async, pollSTM, withAsync, async, cancel) where

import Control.Concurrent.Async.Lifted
