module AsyncOnce.Done where

import Control.Applicative (Applicative (pure, (<*>)))
import Control.Exception   (SomeException)
import Data.Function       (($))
import Data.Functor        (Functor (fmap))

data Done a = Failure SomeException | Success a

instance Functor Done where
    _ `fmap` Failure e = Failure e
    f `fmap` Success x = Success (f x)

instance Applicative Done where
    pure = Success

    Failure e <*> _         = Failure e
    _         <*> Failure e = Failure e
    Success f <*> Success x = Success $ f x
