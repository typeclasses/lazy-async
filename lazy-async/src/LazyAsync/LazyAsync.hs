{-# language Safe #-}

module LazyAsync.LazyAsync where

import Control.Applicative         (Applicative (pure, (<*>)))
import Control.Concurrent.STM.TVar (TVar)
import Data.Bool                   (Bool)
import Data.Functor                (Functor (fmap))
import LazyAsync.Async             (Async)

-- | An asynchronous action that does not start right away
data LazyAsync a =
    A0 a -- ^ Triviality that gives rise to 'pure'
  | A1 (TVar Bool) (Async a) -- ^ A single action
  | forall x. A2 (LazyAsync (x -> a)) (LazyAsync x)
        -- ^ A complex of two 'LazyAsync's

instance Functor LazyAsync where
    f `fmap` A0 x   = A0 (f x)
    f `fmap` A1 s a = A1 s (fmap f a)
    f `fmap` A2 x y = A2 (fmap (fmap f) x) y

-- | '<*>' = 'apply'
instance Applicative LazyAsync where pure = A0; (<*>) = A2

apply :: LazyAsync (a -> b) -- ^ Left part
      -> LazyAsync a        -- ^ Right part
      -> LazyAsync b        -- ^ Complex of the left and right parts
apply = A2
{- ^
Combines the results of two 'LazyAsync's

The behavior of a complex 'LazyAsync':

  * __'LazyAsync.start'__  — Starts all of the parts immediately

  * __'LazyAsync.wait'__ — Returns a 'LazyAsync.Success' result after
    all of the parts complete successfully. As soon as one part fails,
    the whole complex fails immediately (but any 'LazyAsync.Incomplete'
    parts keep running in the background)

  * __'LazyAsync.poll'__ — Returns 'LazyAsync.Failure' if any part
    has failed; otherwise 'LazyAsync.Incomplete' if any part has
    not finished; otherwise 'LazyAsync.Success'

If multiple parts of a complex fail, the 'LazyAsync.wait' and
'LazyAsync.poll' operations only reveal one of the exceptions.
"Which one?" is unspecified and not guaranteed to be consistent. -}
