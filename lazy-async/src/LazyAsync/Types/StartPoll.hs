module LazyAsync.Types.StartPoll where

import LazyAsync.Types.Status (Status)

import LazyAsync.Prelude (Functor, STM)

data StartPoll a = StartPoll
    (STM ()) -- ^ Start
    (STM (Status a)) -- ^ Poll
  deriving Functor
