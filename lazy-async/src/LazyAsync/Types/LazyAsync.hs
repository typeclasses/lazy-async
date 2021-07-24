{-# language Safe #-}

module LazyAsync.Types.LazyAsync where

import LazyAsync.Types.Complex   (Complex (..))
import LazyAsync.Types.StartPoll (StartPoll)
import LazyAsync.Types.Status    (Status)

import LazyAsync.Prelude (Functor)

-- | An asynchronous action that does not start right away
data LazyAsync a =
    Empty -- ^ Triviality that gives rise to 'empty'
  | Pure a -- ^ Triviality that gives rise to 'pure'
  | A1 (StartPoll a) -- ^ A single action
  | A2 (Complex LazyAsync Status a) -- ^ A complex of two 'LazyAsync's
  deriving Functor
