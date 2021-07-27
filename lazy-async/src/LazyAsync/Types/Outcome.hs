{-# language Safe #-}

module LazyAsync.Types.Outcome where

import LazyAsync.Prelude (Foldable, Functor, Show, SomeException, Traversable)

-- | The result of a 'LazyAsync.LazyAsync' that is 'LazyAsync.Done' running
--
-- Obtained using 'LazyAsync.waitCatch'
data Outcome a =
    Failure SomeException -- ^ 💣 The 'LazyAsync.LazyAsync.LazyAsync' action threw an exception
  | Success a -- ^ ✅ The 'LazyAsync.LazyAsync.LazyAsync' action completed normally
    deriving (Foldable, Functor, Show, Traversable)
