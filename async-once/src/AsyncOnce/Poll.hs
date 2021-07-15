module AsyncOnce.Poll where

import AsyncOnce.Done

import Relude

data Poll a = Incomplete | Done (Done a)

instance Functor Poll where
    _ `fmap` Incomplete = Incomplete
    f `fmap` Done x = Done (f `fmap` x)

instance Applicative Poll where
    pure = Done . pure

    Done (Failure e) <*> _                = Done $ Failure e
    _                <*> Done (Failure e) = Done $ Failure e
    Done (Success f) <*> Done (Success x) = Done $ Success $ f x
    _                <*> _                = Incomplete
