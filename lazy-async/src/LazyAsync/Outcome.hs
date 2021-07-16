module LazyAsync.Outcome where

import Control.Applicative (Applicative (pure, (<*>)))
import Control.Exception   (SomeException)
import Data.Function       (($))
import Data.Functor        (Functor (fmap))
import Text.Show           (Show)

data Outcome a = Failure SomeException | Success a
    deriving Show

instance Functor Outcome where
    _ `fmap` Failure e = Failure e
    f `fmap` Success x = Success (f x)

instance Applicative Outcome where
    pure = Success

    Failure e <*> _         = Failure e
    _         <*> Failure e = Failure e
    Success f <*> Success x = Success $ f x
