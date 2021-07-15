module AsyncOnce.AsyncOnce where

import Relude
import Control.Concurrent.Async (Async)

data AsyncOnce a =
    A0 a
  | A1 (TVar Bool) (Async a)
  | forall x. A2 (AsyncOnce (x -> a)) (AsyncOnce x)

instance Functor AsyncOnce where
    f `fmap` A0 x = A0 (f x)
    f `fmap` A1 s a = A1 s (fmap f a)
    f `fmap` A2 x y = A2 (fmap (fmap f) x) y

instance Applicative AsyncOnce where
    pure = A0
    (<*>) = A2
