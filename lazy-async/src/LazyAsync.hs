module LazyAsync
  ( {- * Asynchronous actions -}  LazyAsync,
    {- * Spawning -}              withLazyAsync,
    {- * Getting results -}       startWait,
    {- * Combining actions -}     {- $CombiningActions -}
    {- * Catching exceptions -}   startWaitCatch, Outcome (..),
    {- * Polling -}               poll, Status (..),
    {- * Starting manually -}     start, wait, waitCatch,
    {- * Transactions -}          pollSTM, startSTM, waitCatchSTM,
    {- * IO specializations -}    withLazyAsyncIO, startWaitIO, startWaitCatchIO,
                                  startIO, pollIO, waitIO, waitCatchIO
  ) where

import LazyAsync.LazyAsync (LazyAsync)
import LazyAsync.Outcome   (Outcome (..))
import LazyAsync.Polling   (poll, pollIO, pollSTM)
import LazyAsync.Spawning  (withLazyAsync, withLazyAsyncIO)
import LazyAsync.Starting  (start, startIO, startSTM)
import LazyAsync.Status    (Status (..))
import LazyAsync.Waiting   (startWait, startWaitCatch, startWaitCatchIO,
                            startWaitIO, wait, waitCatch, waitCatchIO,
                            waitCatchSTM, waitIO)

{- $CombiningActions

'LazyAsync' has an 'Control.Applicative.Applicative' instance that lets us build
up complexes that combine results from multiple asynchronous actions.

* When we 'start' a complex, it starts all of its parts.

* When we 'wait' on a complex, it returns a 'Success' result once all of its
parts complete successfully.

* When any part resolves to a 'Failure' outcome, the complex fails and 'wait'
throws an exception immediately.

* When we 'wait' on a complex that suffers failures in multiple parts, the
exception thrown is the one that comes from the part that failed first
chronologically.

* When one part of a complex fails, the other parts remain running. This is
because their results may still be wanted elsewhere.

When we 'poll' a complex that has suffered failures in multiple parts, we see
the leftmost 'Failure'. This can have potentially surprising consequences:

* Whe exception we see when we 'poll' a failed 'LazyAsync' might not be the same
exception that was thrown by 'wait'.

* 'poll'ing a failed 'LazyAsync' is not guaranteed to give the same exception
each time.

The 'Control.Applicative.Applicative' instances for 'Outcome' and 'Status' are
chosen to produce the fail-fast behavior of 'LazyAsync'.

* 'Outcome' behaves the same as 'Data.Either.Either', halting at the leftmost
'Failure'.

* If any part of a complex is 'Failure', then the complex evaluates to
'Failure', even if some parts are 'Incomplete'. For example,
@'Incomplete' 'Control.Applicative.<*>' 'Failure' e@ = @'Failure' e@.

-}
