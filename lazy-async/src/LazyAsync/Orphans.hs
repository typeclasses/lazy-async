{-# language Safe #-}

{-# options_ghc -Wno-orphans #-}

module LazyAsync.Orphans where

import LazyAsync.Actions
import LazyAsync.Prelude
import LazyAsync.Types

-- | 🌈 '<*>' is equivalent to 'LazyAsync.apply'
instance Applicative LazyAsync where
    pure = pureLazyAsync
    (<*>) = apply

-- | 🌈 '<|>' is equivalent to 'LazyAsync.choose'
instance Alternative LazyAsync where
    empty = emptyLazyAsync
    (<|>) = choose

-- | 🌈 '<*>' is equivalent to 'applyStatus'
instance Applicative Status where
    pure = pureStatus
    (<*>) = applyStatus

-- | 🌈 '<|>' is equivalent to 'chooseStatus'
instance Alternative Status where
    empty = emptyStatus
    (<|>) = chooseStatus

-- | 🌈 '<*>' is equivalent to 'applyOutcome'
instance Applicative Outcome where
    pure = pureOutcome
    (<*>) = applyOutcome

-- | 🌈 '<|>' is equivalent to 'chooseOutcome'
instance Alternative Outcome where
    empty = emptyOutcome
    (<|>) = chooseOutcome
