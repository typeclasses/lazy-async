{-# language Safe #-}

module LazyAsync.Types.LazyAsync (LazyAsync (..), apply, choose) where

import LazyAsync.Types.Complex   (Complex (..))
import LazyAsync.Types.StartPoll (StartPoll)
import LazyAsync.Types.Status    (Status)

import LazyAsync.Prelude (Alternative (empty, (<|>)), Applicative (pure, (<*>)),
                          Functor)

-- | An asynchronous action that does not start right away
data LazyAsync a =
    Empty -- ^ Triviality that gives rise to 'empty'
  | Pure a -- ^ Triviality that gives rise to 'pure'
  | A1 (StartPoll a) -- ^ A single action
  | A2 (Complex LazyAsync Status a) -- ^ A complex of two 'LazyAsync's
  deriving Functor

-- | '<*>' = 'apply'
instance Applicative LazyAsync where
    pure = Pure
    (<*>) = apply

-- | '<|>' = 'choose'
instance Alternative LazyAsync where
    empty = Empty
    (<|>) = choose

a2 :: (Status x -> Status y -> Status a) -> LazyAsync x -> LazyAsync y -> LazyAsync a
a2 o a b = A2 (Complex o a b)

apply :: LazyAsync (a -> b) -- ^ Left part
      -> LazyAsync a        -- ^ Right part
      -> LazyAsync b        -- ^ Conjunction
apply = a2 (<*>)
{- ^
Conjunctively combines the results of two 'LazyAsync's

The behavior of a conjunctive 'LazyAsync':

  * __'LazyAsync.start'__  — Starts all of the parts immediately

  * __'LazyAsync.wait'__ — Returns a 'LazyAsync.Success' result after
    all of the parts complete successfully. As soon as one part fails,
    the whole conjunction fails immediately (but any
    'LazyAsync.Incomplete' parts keep running in the background)

  * __'LazyAsync.poll'__ — Returns 'LazyAsync.Failure' if any part
    has failed; otherwise 'LazyAsync.Incomplete' if any part has
    not finished; otherwise 'LazyAsync.Success'

If multiple parts of a conjunction fail, the 'LazyAsync.wait' and
'LazyAsync.poll' operations only reveal one of the exceptions. Which one? — The
leftmost exception of the parts that have failed so far. Since this may change,
which exception is visible is not necessarily consistent over time. -}

choose :: LazyAsync a -- ^ Left part
       -> LazyAsync a -- ^ Right part
       -> LazyAsync a -- ^ Disjunction
choose = a2 (<|>)
{- ^
Disjunctively combines the results of two 'LazyAsync's

The behavior of a disjunctive 'LazyAsync':

  * __'LazyAsync.start'__  — Starts all of the parts immediately

  * __'LazyAsync.wait'__ — Returns a 'LazyAsync.Success' result after
    any one part completes successfully. As soon as one part succeeds,
    the whole disjunction succeeds immediately (but any
    'LazyAsync.Incomplete' parts keep running in the background)

  * __'LazyAsync.poll'__ — Returns 'LazyAsync.Success' if any part
    has succeeded; otherwise 'LazyAsync.Incomplete' if any part has
    not finished; otherwise 'LazyAsync.Failure'

If multiple parts of a disjunction succeed, the 'LazyAsync.wait' and
'LazyAsync.poll' operations only reveal one of the result values. Which one? —
The leftmost result of the parts that have succeeded so far. Since this may
change, which value is visible is not necessarily consistent over time. -}
