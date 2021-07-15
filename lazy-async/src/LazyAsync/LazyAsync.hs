module LazyAsync.LazyAsync where

import Control.Applicative         (Applicative (pure, (<*>)))
import Control.Concurrent.Async    (Async)
import Control.Concurrent.STM.TVar (TVar)
import Data.Bool                   (Bool)
import Data.Functor                (Functor (fmap))

-- | An asynchronous action that does not start right away
data LazyAsync a =
    A0 a
  | A1 (TVar Bool) (Async a)
  | forall x. A2 (LazyAsync (x -> a)) (LazyAsync x)

instance Functor LazyAsync where
    f `fmap` A0 x   = A0 (f x)
    f `fmap` A1 s a = A1 s (fmap f a)
    f `fmap` A2 x y = A2 (fmap (fmap f) x) y

instance Applicative LazyAsync where
    pure = A0
    (<*>) = A2
