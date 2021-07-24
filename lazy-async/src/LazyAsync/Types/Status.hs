{-# language Safe #-}

module LazyAsync.Types.Status where

import LazyAsync.Types.Outcome (Outcome (Failure, Success))

import LazyAsync.Prelude (Alternative (..), Applicative (pure, (<*>)), Foldable,
                          Functor, Show, Traversable)

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

-- | '<*>' = 'applyStatus'
instance Applicative Status where
    pure x = Done (pure x)
    (<*>) = applyStatus

-- | '<|>' = 'chooseStatus'
instance Alternative Status where
    empty = Done empty
    (<|>) = chooseStatus

{- | Combines two 'LazyAsync.LazyAsync' statuses to produce the status of their
conjunction

💣 Returns the leftmost 'Failure', if there is one

⏳ Otherwise, if any part of a conjunction is 'Incomplete', then the whole thing
evaluates to 'Incomplete'

✅ Only when all parts have completed as 'Success' does the whole succeed

For example, @'applyStatus' 'Incomplete' ('Failure' e)@ = @'Failure' e@ -}
applyStatus :: Status (a -> b) -> Status a -> Status b
applyStatus a b =
    case a of
        Done (Success f) ->
            case b of
                Done (Success x) -> Done (Success (f x))
                Done (Failure e) -> Done (Failure e)
                Incomplete       -> Incomplete
        Done (Failure e) -> Done (Failure e)
        Incomplete ->
            case b of
                Done (Failure e) -> Done (Failure e)
                _                -> Incomplete

{- | Combines two 'LazyAsync.LazyAsync' statuses to produce the status of their
disjunction

✅ Returns the leftmost 'Success', if there is one

⏳ Otherwise, if any part of a disjunction is 'Incomplete', then the whole thing
evaluates to 'Incomplete'

💣 Only when all parts have completed as 'Failure' does the whole fail -}
chooseStatus :: Status a -> Status a -> Status a
chooseStatus x y =
    case x of
        Done Success{} -> x
        Done Failure{} -> y
        Incomplete ->
            case y of
                Done Failure{} -> x
                _              -> y
