{-# language Safe #-}

module LazyAsync.Types.Complex where

import LazyAsync.Prelude (Functor)

data Complex f g a = forall x y. Complex (g x -> g y -> g a) (f x) (f y)

deriving instance Functor g => Functor (Complex f g)
