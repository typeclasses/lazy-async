module LazyAsync.Status where

import Control.Applicative (Applicative (pure, (<*>)))
import Data.Function       (($), (.))
import Data.Functor        (Functor (fmap))
import LazyAsync.Outcome   (Outcome (Failure, Success))

data Status a = Incomplete | Done (Outcome a)

instance Functor Status where
    _ `fmap` Incomplete = Incomplete
    f `fmap` Done x     = Done (f `fmap` x)

instance Applicative Status where
    pure = Done . pure

    Done (Failure e) <*> _                = Done $ Failure e
    _                <*> Done (Failure e) = Done $ Failure e
    Done (Success f) <*> Done (Success x) = Done $ Success $ f x
    _                <*> _                = Incomplete
