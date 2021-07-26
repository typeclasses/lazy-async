{-# language Safe #-}

module LazyAsync.Actions.Pure where

import LazyAsync.Types (LazyAsync (..), Outcome (..), Status (..))

pureOutcome :: a -> Outcome a
pureOutcome = Success

pureStatus :: a -> Status a
pureStatus x = Done (pureOutcome x)

pureLazyAsync :: a -> LazyAsync a
pureLazyAsync = Pure
