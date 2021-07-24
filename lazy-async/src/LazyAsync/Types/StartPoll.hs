module LazyAsync.Types.StartPoll where

import Control.Concurrent.STM (STM)
import Data.Functor           (Functor)
import LazyAsync.Types.Status (Status)

data StartPoll a = StartPoll
    (STM ()) -- ^ Start
    (STM (Status a)) -- ^ Poll
  deriving Functor
