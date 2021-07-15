module AsyncOnce.Starting where

import AsyncOnce.AsyncOnce

import Relude

-- | Begin running an asynchronous action, if it has not already begun.
start :: AsyncOnce a -> IO ()
start A0{} = return ()
start (A1 s _) = atomically $ writeTVar s True
start (A2 x y) = start x *> start y
