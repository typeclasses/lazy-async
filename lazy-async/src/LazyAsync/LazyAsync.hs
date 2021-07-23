{-# language Safe #-}

module LazyAsync.LazyAsync where

import Control.Applicative    (Alternative (empty, (<|>)),
                               Applicative (pure, (<*>)))
import Control.Concurrent.STM (STM)
import Data.Functor           (Functor (fmap))
import LazyAsync.Status       (Status)

-- | An asynchronous action that does not start right away
data LazyAsync a =
    A0 a -- ^ Triviality that gives rise to 'pure'
  | A1 -- ^ A single action
      (STM ()) -- ^ Start
      (STM (Status a)) -- ^ Poll
  | forall x. Ap (LazyAsync (x -> a)) (LazyAsync x)
        -- ^ A complex of two 'LazyAsync's
  | Choose (LazyAsync a) (LazyAsync a)
  | Empty

instance Functor LazyAsync where
    f `fmap` A0 x       = A0 (f x)
    f `fmap` A1 s a     = A1 s (fmap (fmap f) a)
    f `fmap` Ap x y     = Ap (fmap (fmap f) x) y
    f `fmap` Choose x y = Choose (fmap f x) (fmap f y)
    _ `fmap` Empty      = Empty

-- | '<*>' = 'apply'
instance Applicative LazyAsync where
    pure = A0
    (<*>) = Ap

-- | '<|>' = 'choose'
instance Alternative LazyAsync where
    empty = Empty
    (<|>) = choose

apply :: LazyAsync (a -> b) -- ^ Left part
      -> LazyAsync a        -- ^ Right part
      -> LazyAsync b        -- ^ Complex of the left and right parts
apply = Ap
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

If multiple parts of a complex fail, the 'LazyAsync.wait' and 'LazyAsync.poll'
operations only reveal one of the exceptions. Which one? — The leftmost
exception of the asyncs that have failed so far. Since this may change, which
exception is visible is not necessarily consistent over time. -}

choose :: LazyAsync a -> LazyAsync a -> LazyAsync a
choose = Choose
