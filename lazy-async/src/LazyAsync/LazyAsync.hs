{-# language Safe #-}

module LazyAsync.LazyAsync where

import Control.Applicative         (Applicative (pure, (<*>)))
import Control.Concurrent.STM.TVar (TVar)
import Data.Bool                   (Bool)
import Data.Functor                (Functor (fmap))
import LazyAsync.Async             (Async)

-- | An asynchronous action that does not start right away
data LazyAsync a =
    A0 a
  | A1 (TVar Bool) (Async a)
  | forall x. A2 (LazyAsync (x -> a)) (LazyAsync x)

instance Functor LazyAsync where
    f `fmap` A0 x   = A0 (f x)
    f `fmap` A1 s a = A1 s (fmap f a)
    f `fmap` A2 x y = A2 (fmap (fmap f) x) y

-- | '<*>' = 'apply'
instance Applicative LazyAsync where
    pure = A0
    (<*>) = A2

{- | Constructs a complex 'LazyAsync' that combines results from two other
asynchronous actions.

* When we 'LazyAsync.start' a complex, it starts all of its parts.

* When we 'LazyAsync.wait' on a complex, it returns a 'LazyAsync.Success' result
once all of its parts complete successfully.

* When any part resolves to a 'LazyAsync.Failure' outcome, the complex fails and
'LazyAsync.wait' throws an exception immediately.

* When we 'LazyAsync.wait' on a complex that suffers failures in multiple parts,
the exception thrown is the one that comes from the part that failed first
chronologically.

* When one part of a complex fails, the other parts remain running. This is
because their results may still be wanted elsewhere.

When we 'LazyAsync.poll' a complex that has suffered failures in multiple parts,
we see the leftmost 'LazyAsync.Failure'. This can have potentially surprising
consequences:

* Whe exception we see when we 'LazyAsync.poll' a failed 'LazyAsync' might not
be the same exception that was thrown by 'LazyAsync.wait'.

* 'LazyAsync.poll'ing a failed 'LazyAsync' is not guaranteed to give the same
exception each time.

-}
apply ::
    LazyAsync (a -> b) -> LazyAsync a
    -> LazyAsync b -- ^ Complex
apply = A2
