{-# language Safe #-}

module LazyAsync.Actions.Merge where

import LazyAsync.Types (Complex (Complex), LazyAsync (A2), Status, applyStatus,
                        chooseStatus)

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
