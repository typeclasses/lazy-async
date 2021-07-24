{-# language Safe #-}

module LazyAsync.Types.Apply where

import Data.Functor (Functor)

data Apply f a = forall x. Apply (f (x -> a)) (f x)

deriving instance Functor f => Functor (Apply f)
