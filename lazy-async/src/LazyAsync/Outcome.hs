{-# language Safe #-}

module LazyAsync.Outcome where

import Control.Applicative (Alternative (empty, (<|>)),
                            Applicative (pure, (<*>)))
import Control.Exception   (Exception, SomeException, toException)
import Data.Foldable       (Foldable (foldr))
import Data.Functor        (Functor, fmap)
import Data.Traversable    (Traversable (sequenceA))
import Text.Show           (Show)

-- | The result of a 'LazyAsync.LazyAsync' that is 'LazyAsync.Done' running
--
-- Obtained using 'LazyAsync.waitCatch'
data Outcome a =
    Failure SomeException -- ^ The 'LazyAsync.LazyAsync.LazyAsync' action threw an exception
  | Success a -- ^ The 'LazyAsync.LazyAsync.LazyAsync' action completed normally
    deriving (Functor, Show)

-- | '<*>' = 'applyOutcome'
instance Applicative Outcome where
    pure = Success
    (<*>) = applyOutcome

-- | '<|>' = 'chooseOutcome'
instance Alternative Outcome where
    empty = Failure (toException NoAlternative)
    (<|>) = chooseOutcome

instance Foldable Outcome where
    foldr _ z (Failure _) = z
    foldr f z (Success x) = f x z

instance Traversable Outcome where
    sequenceA (Failure e)  = pure (Failure e)
    sequenceA (Success mx) = fmap Success mx

-- | Behaves the same as '<*>' for 'Data.Either.Either', halting at the leftmost 'Failure'
applyOutcome :: Outcome (a -> b) -> Outcome a -> Outcome b
applyOutcome fo ao =
    case fo of
        Failure e -> Failure e
        Success f ->
            case ao of
                Failure e -> Failure e
                Success x -> Success (f x)

-- | Behaves the same as '<|>' for 'Data.Either.Either', returning the leftmost 'Success'
chooseOutcome :: Outcome a -> Outcome a -> Outcome a
chooseOutcome x y =
    case x of
        Failure _ -> y
        _         -> x

data NoAlternative = NoAlternative
    deriving Show

instance Exception NoAlternative
