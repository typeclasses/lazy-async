{-# language Trustworthy #-}

module LazyAsync.Libraries.Async (Async, pollSTM, withAsync, async, cancel, Forall, Pure) where

import Control.Concurrent.Async.Lifted (Async, async, cancel, pollSTM,
                                        withAsync)

import Control.Concurrent.Async.Lifted.Safe (Forall, Pure)
