{-# language Safe #-}

module LazyAsync.Actions.Merge where

import LazyAsync.Types (Complex (Complex), LazyAsync (A2), Status)

import LazyAsync.Prelude ((<*>), (<|>))

merge :: ( Status    a -> Status    b -> Status    c )
      -> ( LazyAsync a -> LazyAsync b -> LazyAsync c )
merge (*) a b = A2 (Complex (*) a b)

apply :: LazyAsync (a -> b) -- ^ Left part
      -> LazyAsync a        -- ^ Right part
      -> LazyAsync b        -- ^ Conjunction
apply = merge (<*>)
{- ^
Conjunctively combines the results of two 'LazyAsync's

🚀 __'LazyAsync.start'__ starts all of the parts of a conjunctive 'LazyAsync'
immediately

⏸️ __'LazyAsync.wait'__ returns a 'LazyAsync.Success' result after all of the
parts complete successfully. As soon as one part fails, the whole conjunction
fails immediately (but any 'LazyAsync.Incomplete' parts keep running in the
background)

🕵️ __'LazyAsync.poll'__ returns 'LazyAsync.Failure' if any part has failed;
otherwise 'LazyAsync.Incomplete' if any part has not finished; otherwise
'LazyAsync.Success'

💣 If multiple parts of a conjunction fail, the 'LazyAsync.wait' and
'LazyAsync.poll' operations only reveal the leftmost exception of the parts that
have failed so far. Since this may change, which exception is visible is not
necessarily consistent over time.

🌈 'apply' is equivalent to @'merge' ('<*>')@
-}

choose :: LazyAsync a -- ^ Left part
       -> LazyAsync a -- ^ Right part
       -> LazyAsync a -- ^ Disjunction
choose = merge (<|>)
{- ^
Disjunctively combines the results of two 'LazyAsync's

🚀 __'LazyAsync.start'__ starts all of the parts of a disjunctive 'LazyAsync'
immediately

⏸️ __'LazyAsync.wait'__ returns a 'LazyAsync.Success' result after any one part
completes successfully. As soon as one part succeeds, the whole disjunction
succeeds immediately (but any 'LazyAsync.Incomplete' parts keep running in the
background)

🕵️ __'LazyAsync.poll'__ returns 'LazyAsync.Success' if any part has succeeded;
otherwise 'LazyAsync.Incomplete' if any part has not finished; otherwise
'LazyAsync.Failure'

✅ If multiple parts of a disjunction succeed, the 'LazyAsync.wait' and
'LazyAsync.poll' operations only reveal the leftmost result of the parts that
have succeeded so far. Since this may change, which value is visible is not
necessarily consistent over time.

🌈 'choose' is equivalent to @'merge' ('<|>')@
-}
