module Test.General where

import Control.Concurrent       (threadDelay)
import Control.Monad            (when, (>>=))
import Control.Monad.Base       (MonadBase, liftBase)
import Control.Monad.IO.Class   (MonadIO, liftIO)
import Control.Monad.Trans.Cont (ContT, evalContT)
import Data.Bool                (not)
import Data.Function            ((.))
import System.Exit              (exitFailure)
import System.IO                (IO)

import Hedgehog (Group, Property, PropertyT, checkParallel, property, withTests)

main' :: Group -> IO ()
main' group = checkParallel group >>= \ok -> when (not ok) exitFailure

example :: PropertyT IO () -> Property
example = withTests 1 . property

contExample :: ContT () (PropertyT IO) () -> Property
contExample = example . evalContT

contIO :: MonadIO m => ContT a IO a -> ContT () m a
contIO = liftIO . evalContT

pause :: MonadBase IO m => m ()
pause = liftBase (threadDelay 1000000)
