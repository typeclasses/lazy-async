module Main (main) where

import Hedgehog
import System.IO
import System.Exit
import Control.Monad
import Data.Bool
import Data.Function
import Time
import Control.Monad.IO.Class
import Control.Concurrent.STM
import Data.String

import qualified LazyAsync as LA

main :: IO ()
main = checkParallel group >>= \ok -> when (not ok) exitFailure

group :: Group
group = Group "LazyAsync" properties

properties :: [(PropertyName, Property)]
properties =
  [

    (,) "LazyAsync is lazy" $ withTests 1 $ property $
      do
        log <- liftIO $ newTVarIO []
        let write :: String -> IO (); write x = atomically $ modifyTVar' log (x :)
        liftIO $ LA.withLazyAsync (write "Hi!") $ \_ ->
            threadDelay (sec 1)
        liftIO (readTVarIO log) >>= (=== [])

  , (,) "LazyAsync can be started manually" $ withTests 1 $ property $
      do
        log <- liftIO $ newTVarIO []
        let write :: String -> IO (); write x = atomically $ modifyTVar' log (x :)
        liftIO $ LA.withLazyAsync (write "Hi!") $ \la ->
          do
            LA.start la
            threadDelay (sec 1)
        liftIO (readTVarIO log) >>= (=== ["Hi!"])

  ]
