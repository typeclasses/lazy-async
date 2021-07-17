{-# language Safe #-}

module LazyAsync.Outcome where

import Control.Applicative (Applicative (pure, (<*>)))
import Control.Exception   (SomeException)
import Data.Functor        (Functor (fmap))
import Text.Show           (Show)

-- | The result of a 'LazyAsync.LazyAsync' that is 'LazyAsync.Done' running
--
-- Obtained using 'LazyAsync.waitCatch'
data Outcome a =
    Failure SomeException -- ^ The 'LazyAsync.LazyAsync.LazyAsync' action threw an exception
  | Success a -- ^ The 'LazyAsync.LazyAsync.LazyAsync' action completed normally
    deriving Show

instance Functor Outcome where
    _ `fmap` Failure e = Failure e
    f `fmap` Success x = Success (f x)

-- | '<*>' = 'applyOutcome'
instance Applicative Outcome where
    pure = Success
    (<*>) = applyOutcome

-- | Behaves the same as '<*>' for 'Data.Either.Either', halting at the leftmost 'Failure'
applyOutcome :: Outcome (a -> b) -> Outcome a -> Outcome b
applyOutcome fo ao =
    case fo of
        Failure e -> Failure e
        Success f ->
            case ao of
                Failure e -> Failure e
                Success x -> Success (f x)
