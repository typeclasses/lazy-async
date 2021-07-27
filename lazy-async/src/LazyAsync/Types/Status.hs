{-# language Safe #-}

module LazyAsync.Types.Status where

import LazyAsync.Types.Outcome (Outcome)

import LazyAsync.Prelude (Foldable, Functor, Show, Traversable)

-- | Whether a 'LazyAsync.LazyAsync' action has
-- completed yet, and, if so, what it produced
--
-- Obtained using 'LazyAsync.poll'
data Status a =
    Incomplete -- ^ ⏳
        -- The 'LazyAsync.LazyAsync' action has not finished
        -- (and might not have even started yet)
  | Done (Outcome a) -- ^ ⌛
        -- The 'LazyAsync.LazyAsync' action has ended, either
        -- by ✅ returning normally or by 💣 throwing an exception
    deriving (Foldable, Functor, Show, Traversable)
