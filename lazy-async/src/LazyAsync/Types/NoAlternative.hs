{-# language Safe #-}

module LazyAsync.Types.NoAlternative where

import LazyAsync.Prelude (Exception, Show)

data NoAlternative = NoAlternative deriving Show

instance Exception NoAlternative
