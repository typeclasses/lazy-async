module LazyAsync.Types.Resource where

import LazyAsync.Prelude (Functor)

data Resource m a = Resource{ release :: m (), resource :: a }
    deriving Functor
