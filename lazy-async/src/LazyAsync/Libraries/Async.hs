{-# language Trustworthy #-}

module LazyAsync.Libraries.Async (Async, pollSTM, withAsync, async, cancel, Forall, Pure) where

import Control.Concurrent.Async.Lifted
import Control.Concurrent.Async.Lifted.Safe (Forall, Pure)
