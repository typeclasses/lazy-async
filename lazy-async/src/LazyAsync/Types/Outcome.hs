{-# language Safe #-}

module LazyAsync.Types.Outcome where

import LazyAsync.Prelude (Alternative (..), Applicative (pure, (<*>)),
                          Exception (..), Foldable, Functor, Show,
                          SomeException, Traversable)

-- | The result of a 'LazyAsync.LazyAsync' that is 'LazyAsync.Done' running
--
-- Obtained using 'LazyAsync.waitCatch'
data Outcome a =
    Failure SomeException -- ^ The 'LazyAsync.LazyAsync.LazyAsync' action threw an exception
  | Success a -- ^ The 'LazyAsync.LazyAsync.LazyAsync' action completed normally
    deriving (Foldable, Functor, Show, Traversable)

-- | '<*>' = 'applyOutcome'
instance Applicative Outcome where
    pure = Success
    (<*>) = applyOutcome

-- | '<|>' = 'chooseOutcome'
instance Alternative Outcome where
    empty = Failure (toException NoAlternative)
    (<|>) = chooseOutcome

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
        Failure{} -> y
        _         -> x

data NoAlternative = NoAlternative
    deriving Show

instance Exception NoAlternative
