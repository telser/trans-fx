trans-fx
========

This is the source repository of the `trans-fx` Haskell library and its friends.

`trans-fx` is a reimagining of `transformers` and `mtl` taking advantage of some newer features of GHC, specifically the `QuantifiedContexts` language extension. Our aim is to provide a composable, extensible, and mockable grammar of monadic effects that is efficient and easy to use.

Currently this is very much _in progress_ and should not be used by anyone on pain of frustrating type errors. :)

`trans-fx` consists of a few separate libraries, split up to keep dependencies under control.

* `trans-fx` is the core library and depends only on `base`.
* `trans-fx-testing` includes helper code for writing tests.
