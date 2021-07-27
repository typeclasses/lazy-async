{-# language Safe #-}

module LazyAsync.Actions.Merge where

import LazyAsync.Types (Complex (..), LazyAsync (..), Outcome (..), Status (..))

merge :: ( Status    a -> Status    b -> Status    c )
             -- ^ Status merge function
      -> ( LazyAsync a -> LazyAsync b -> LazyAsync c )
merge (*) a b = A2 (Complex (*) a b)
{- ^ A combination of two 'LazyAsync's, where the 'Status' of the
combination is a function of the statuses of each of its parts

🚀 __'LazyAsync.start'__ starts both parts immediately

The behavior of 🕵️ __'LazyAsync.poll'__ and
💣 __'LazyAsync.wait'__ is determined by the status merge function

-}

apply :: LazyAsync (a -> b) -- ^ Left part
      -> LazyAsync a        -- ^ Right part
      -> LazyAsync b        -- ^ Conjunction
apply = merge applyStatus
{- ^
Conjunctively combines the results of two 'LazyAsync's

🚀 __'LazyAsync.start'__ starts both parts immediately

⏸️ __'LazyAsync.wait'__ returns a 'LazyAsync.Success' result after both
parts complete successfully. As soon as one part fails, the whole conjunction
fails immediately (but any 'LazyAsync.Incomplete' part keeps running in the
background)

🕵️ __'LazyAsync.poll'__ returns 'LazyAsync.Failure' if either part has failed;
otherwise 'LazyAsync.Incomplete' if either part has not finished; otherwise
'LazyAsync.Success'

💣 The 'LazyAsync.wait' and 'LazyAsync.poll' operations disclose the
leftmost exception of the parts that have failed so far, which may not
be consistent over time

🌈 'apply' is equivalent to @('merge' 'applyStatus')@
-}

choose :: LazyAsync a -- ^ Left part
       -> LazyAsync a -- ^ Right part
       -> LazyAsync a -- ^ Disjunction
choose = merge chooseStatus
{- ^
Disjunctively combines the results of two 'LazyAsync's

🚀 __'LazyAsync.start'__ starts both parts immediately

⏸️ __'LazyAsync.wait'__ returns a 'LazyAsync.Success' result after either part
completes successfully. As soon as one part succeeds, the whole disjunction
succeeds immediately (but any 'LazyAsync.Incomplete' part keeps running in the
background)

🕵️ __'LazyAsync.poll'__ returns 'LazyAsync.Success' if either part has
succeeded; otherwise 'LazyAsync.Incomplete' if either part has not finished;
otherwise 'LazyAsync.Failure'

✅ The 'LazyAsync.wait' and 'LazyAsync.poll' operations disclose the leftmost
result of the parts that have succeeded so far, which may not be consistent
over time

🌈 'choose' is equivalent to @('merge' 'chooseStatus')@
-}

{- | Combines two 'LazyAsync.LazyAsync' statuses to produce the status of their
conjunction

💣 Returns the leftmost 'Failure', if there is one

⏳ Otherwise, if any part of a conjunction is 'Incomplete', then the whole thing
evaluates to 'Incomplete'

✅ Only when all parts have completed as 'Success' does the whole succeed

For example, @'applyStatus' 'Incomplete' ('Failure' e)@ = @'Failure' e@ -}
applyStatus :: Status (a -> b) -> Status a -> Status b
applyStatus a b =
    case a of
        Done (Success f) ->
            case b of
                Done (Success x) -> Done (Success (f x))
                Done (Failure e) -> Done (Failure e)
                Incomplete       -> Incomplete
        Done (Failure e) -> Done (Failure e)
        Incomplete ->
            case b of
                Done (Failure e) -> Done (Failure e)
                _                -> Incomplete

{- | Combines two 'LazyAsync.LazyAsync' statuses to produce the status of their
disjunction

✅ Returns the leftmost 'Success', if there is one

⏳ Otherwise, if any part of a disjunction is 'Incomplete', then the whole thing
evaluates to 'Incomplete'

💣 Only when all parts have completed as 'Failure' does the whole fail -}
chooseStatus :: Status a -> Status a -> Status a
chooseStatus x y =
    case x of
        Done Success{} -> x
        Done Failure{} -> y
        Incomplete ->
            case y of
                Done Failure{} -> x
                _              -> y

-- | Behaves the same as 'Control.Applicative.<*>' for
-- 'Data.Either.Either', halting at the leftmost 'Failure'
applyOutcome :: Outcome (a -> b) -> Outcome a -> Outcome b
applyOutcome fo ao =
    case fo of
        Failure e -> Failure e
        Success f ->
            case ao of
                Failure e -> Failure e
                Success x -> Success (f x)

-- | Behaves the same as 'Control.Applicative.<|>' for
-- 'Data.Either.Either', returning the leftmost 'Success'
chooseOutcome :: Outcome a -> Outcome a -> Outcome a
chooseOutcome x y =
    case x of
        Failure{} -> y
        _         -> x
