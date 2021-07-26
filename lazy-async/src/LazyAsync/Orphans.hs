{-# language Safe #-}

{-# options_ghc -Wno-orphans #-}

module LazyAsync.Orphans where

import LazyAsync.Actions
import LazyAsync.Prelude
import LazyAsync.Types

-- | 🌈 '<*>' is equivalent to 'LazyAsync.apply'
instance Applicative LazyAsync where
    pure = Pure
    (<*>) = apply

-- | 🌈 '<|>' is equivalent to 'LazyAsync.choose'
instance Alternative LazyAsync where
    empty = Empty
    (<|>) = choose
