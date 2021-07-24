{-# language Safe #-}

module LazyAsync.Types.LazyAsync where

import LazyAsync.Types.Complex   (Complex (..))
import LazyAsync.Types.StartPoll (StartPoll)
import LazyAsync.Types.Status    (Status)

import LazyAsync.Prelude (Alternative (empty, (<|>)), Applicative (pure, (<*>)),
                          Functor)

-- | An asynchronous action that does not start right away
data LazyAsync a =
    Pure a -- ^ Triviality that gives rise to 'pure'
  | A1 (StartPoll a) -- ^ A single action
  | A2 (Complex LazyAsync Status a) -- ^ A complex of two 'LazyAsync's
  | Empty
  deriving Functor

-- | '<*>' = 'apply'
instance Applicative LazyAsync where
    pure = Pure
    (<*>) = apply

-- | '<|>' = 'choose'
instance Alternative LazyAsync where
    empty = Empty
    (<|>) = choose

apply :: LazyAsync (a -> b) -- ^ Left part
      -> LazyAsync a        -- ^ Right part
      -> LazyAsync b        -- ^ Complex of the left and right parts
apply f x = A2 (Complex (<*>) f x)
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
choose a b = A2 (Complex (<|>) a b)
