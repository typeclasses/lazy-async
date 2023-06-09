Sometimes we have a bunch of `IO` actions that do things like read files, make
HTTP requests, or query a database. Some of the information that these actions
produce might not end up being needed, depending on the breaks. In the interest
of avoiding unnecessary effort, we don't want to simply run all the actions and
collect their results upfront. We also don't want to simply run an action right
before its result is needed, because it might be needed in more than one place,
which opens the possibility of unnecessarily running the same action more than
once. In situations like these, we use `LazyAsync`.

Under the hood, an `IO` action is turned into a `LazyAsync` by constructing two
things: An `Async` (from the [async] package), and a `TVar Bool` (from the [stm]
package). The `TVar`, initialized to `False`, indicates whether the action is
wanted yet. The asynchronous thread waits until the `TVar` turns `True` and then
runs the action.

  [async]: https://hackage.haskell.org/package/async

  [stm]: https://hackage.haskell.org/package/stm
