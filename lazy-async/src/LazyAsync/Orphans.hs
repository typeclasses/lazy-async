{-# options_ghc -Wno-orphans #-}

module LazyAsync.Orphans where

import LazyAsync.Actions
import LazyAsync.Prelude
import LazyAsync.Types

-- | '<*>' = 'LazyAsync.apply'
instance Applicative LazyAsync where
    pure = Pure
    (<*>) = apply

-- | '<|>' = 'LazyAsync.choose'
instance Alternative LazyAsync where
    empty = Empty
    (<|>) = choose
