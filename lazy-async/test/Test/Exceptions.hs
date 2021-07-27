module Test.Exceptions where

import Data.Eq    (Eq)
import Data.Maybe (Maybe (Just))
import Hedgehog   (MonadTest, (===))
import Prelude    (Integer)
import Text.Show  (Show)

import Control.Exception (Exception, SomeException, fromException, throw)

throw' :: Exception e => e -> m Integer
throw' = throw

exceptionIs :: MonadTest m => (Exception e, Eq e, Show e) => e -> SomeException -> m ()
exceptionIs a b = Just a === fromException b
