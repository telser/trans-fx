trans-fx
========

This is the source repository of the `trans-fx` family of Haskell libraries, which together comprise an experimental effect framework based on _monad transformer transformers_.

`trans-fx` is a reimagining of monad transformers in the style of `transformers` and `mtl` taking advantage of some newer features of GHC. Our aim is to provide a composable, extensible, and mockable grammar of monadic effects that is efficient, easy to use, and robust enough for production systems.

Currently this is very much _in progress_ and will only be of interest to the most intrepid functoristas. :)

The closest thing to documentation is this [little tutorial](http://nbloomf.blog/trans-fx/index.html).

`trans-fx` is split into a few separate libraries to help keep dependencies under control.

* `trans-fx-core` is the foundation of the effect system and depends only on `base`.
* `trans-fx-demo` is a collection of tutorials on how to use and extend the library.
* `trans-fx-test` includes helper code for writing tests: `Arbitrary` instances and lawful class tests.
* `trans-fx-io` is a collection of simple effect layers using IO.
