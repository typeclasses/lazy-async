{-# language Safe #-}

module LazyAsync.Types.Resource where

import LazyAsync.Prelude (Functor)

{- | A resource and an action that releases it

A /resource/ is something that can be /acquired/ and then /released/, where
releasing an object once it is no longer needed is important because the supply
is exhaustible.

-}
data Resource m a = Resource{ release :: m (), resource :: a }
    deriving Functor
