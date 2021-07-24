{-# language Safe #-}

module LazyAsync.Types.Status where

import Control.Applicative     (Alternative (empty, (<|>)),
                                Applicative (pure, (<*>)))
import Data.Foldable           (Foldable (foldr))
import Data.Functor            (Functor, fmap)
import Data.Traversable        (Traversable (sequenceA))
import LazyAsync.Types.Outcome (Outcome (Failure, Success))
import Text.Show               (Show)

-- | Whether a 'LazyAsync.LazyAsync' action has
-- completed yet, and, if so, what it produced
--
-- Obtained using 'LazyAsync.poll'
data Status a =
    Incomplete -- ^ The 'LazyAsync.LazyAsync' action has not finished
               --   (and might not have even started yet)
  | Done (Outcome a) -- ^ The 'LazyAsync.LazyAsync' action has ended, either
                     --   by returning normally or by throwing an exception
    deriving (Functor, Show)

-- | '<*>' = 'applyStatus'
instance Applicative Status where
    pure x = Done (pure x)
    (<*>) = applyStatus

-- | '<|>' = 'chooseStatus'
instance Alternative Status where
    empty = Done empty
    (<|>) = chooseStatus

instance Foldable Status where
    foldr _ z Incomplete = z
    foldr f z (Done o)   = foldr f z o

instance Traversable Status where
    sequenceA Incomplete = pure Incomplete
    sequenceA (Done o)   = fmap Done (sequenceA o)

{- | Combines two 'LazyAsync.LazyAsync' statuses to produce a summary
of the status of the overall complex

If any part of a complex is 'Failure', then the complex evaluates to
'Failure', even if some parts are 'Incomplete'

For example, @'applyStatus' 'Incomplete' ('Failure' e)@ = @'Failure' e@ -}
applyStatus :: Status (a -> b) -> Status a -> Status b
Done (Failure e) `applyStatus` _                = Done (Failure e)
_                `applyStatus` Done (Failure e) = Done (Failure e)
Done (Success f) `applyStatus` Done (Success x) = Done (Success (f x))
_                `applyStatus` _                = Incomplete

{- | Returns the leftmost 'Success', if there is one

Otherwise, if any part of a complex is 'Incomplete', then the complex
evaluates to 'Incomplete'

Only when all parts have completed as 'Failure' does the whole fail -}
chooseStatus :: Status a -> Status a -> Status a
chooseStatus x y =
    case x of
        Done Success{} -> x
        Done Failure{} -> y
        Incomplete ->
            case y of
                Done Failure{} -> x
                _              -> y
