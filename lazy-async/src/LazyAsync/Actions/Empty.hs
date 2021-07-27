{-# language Safe #-}

module LazyAsync.Actions.Empty where

import LazyAsync.Types (LazyAsync (..), NoAlternative (..), Outcome (..),
                        Status (..))

import LazyAsync.Prelude (toException)

emptyOutcome :: Outcome a
emptyOutcome = Failure (toException NoAlternative)

emptyStatus :: Status a
emptyStatus = Done emptyOutcome

emptyLazyAsync :: LazyAsync a
emptyLazyAsync = Empty
