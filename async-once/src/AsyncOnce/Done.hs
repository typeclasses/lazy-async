module AsyncOnce.Done where

import Relude

data Done a = Failure SomeException | Success a
    deriving Functor

instance Applicative Done where
    pure = Success

    Failure e <*> _         = Failure e
    _         <*> Failure e = Failure e
    Success f <*> Success x = Success $ f x
