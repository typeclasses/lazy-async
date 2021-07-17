{-# language Safe #-}

module LazyAsync.Status where

import Control.Applicative (Applicative (pure, (<*>)))
import Data.Function       (($), (.))
import Data.Functor        (Functor (fmap))
import LazyAsync.Outcome   (Outcome (Failure, Success))
import Text.Show           (Show)

-- | Whether a 'LazyAsync.LazyAsync' action
-- has completed yet, and, if so, what it produced
--
-- Obtained using 'LazyAsync.poll'
data Status a =
    Incomplete -- ^ The 'LazyAsync.LazyAsync' action has not finished
               --   (and might not have even started yet)
  | Done (Outcome a) -- ^ The 'LazyAsync.LazyAsync' action has ended, either
                     --   by returning normally or by throwing an exception
    deriving Show

instance Functor Status where
    _ `fmap` Incomplete = Incomplete
    f `fmap` Done x     = Done (f `fmap` x)

-- | '<*>' = 'applyStatus'
instance Applicative Status where
    pure = Done . pure
    (<*>) = applyStatus


{- | Combines two 'LazyAsync.LazyAsync' statuses to produce a summary
of the status of the overall complex

If any part of a complex is 'Failure', then the complex evaluates to
'Failure', even if some parts are 'Incomplete'

For example, @'applyStatus' 'Incomplete' ('Failure' e)@ = @'Failure' e@ -}
applyStatus :: Status (a -> b) -> Status a -> Status b
Done (Failure e) `applyStatus` _                = Done $ Failure e
_                `applyStatus` Done (Failure e) = Done $ Failure e
Done (Success f) `applyStatus` Done (Success x) = Done $ Success $ f x
_                `applyStatus` _                = Incomplete
