---
title: Effects with monad transformer transformers
---

Hi! This is a technical introduction to `trans-fx`, an effect framework for Haskell. To say anything interesting I'll have to make a few assumptions about you, dear reader, and I think it's best to be up front about these. This document will make the most sense to you if:

1. You have some experience writing in a functional programming language, and a language with Hindley-Milner style type inference, and found these ideas to be to your liking.
2. You appreciate the use of monads to control side-effects and the use of monad transformers as a strategy for building complex monads from simpler ones.
3. You are invested in testing as a tool for building software that is responsive to changing requirements and resistant to decay.

To use `trans-fx` we'll need a pretty recent version of GHC; the library code depends on some newer language extensions. Client code will also benefit from the following extensions:

> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
>
> module Control.FX.Demo.Intro where

In this intro we'll focus on pure examples. For this we can import just one module:

> import Control.FX

Examples that use `IO` are not much more complicated, but we'll save them for later.



What is different?
==================

This library is heavily influenced by but incompatible with `transformers` and `mtl`. The two most significant differences are as follows.

First, our `MonadTrans` class has an extra quantified constraint.

```haskell
class
  ( forall m. (Monad m) => Monad (t m)
  ) => MonadTrans
    (t :: (* -> *) -> * -> *)
  where
    lift :: ( Monad m ) => m a -> t m a
```

This means we can't have 'undisciplined' monad transformers that turn monads into non-monads; `t m` is required to have a monad instance constrained like `(Monad m) => Monad (t m)`. This class definition uses the `QuantifiedConstraints` extension of GHC.

The second major departure from `mtl` is that most of our monadic effect classes take an additional type parameter. For instance, here's the definition of `MonadState`:

```haskell
class
  ( Monad m, MonadIdentity mark
  ) => MonadState mark s m
  where
    get :: m (mark s)
    put :: (mark s) -> m ()
```

`mark` is the extra parameter, and it is required to be isomorphic to `Identity`. What is the point of that? We could get rid of `mark` without changing the meanings of the effect functions `get` and `put`. But including `mark`  in the class definition allows for a single monad to have multiple `MonadState` instances, even with the same state type, and the typechecker can disambiguate them using `mark`. In the library this requires the use of `UndecidableInstances` (yuk!) but in a disciplined way. Another feature is that if the name of `mark` is chosen thoughtfully it acts like machine checked documentation of where effect values come from.



Monad Construction Kit
======================

The basic abstraction of `trans-fx` is called a _monad transformer transformer_, defined like this:

```haskell
class
  ( forall t. (MonadTrans t) => MonadTrans (u t)
  , forall t m. (Monad m, MonadTrans t) => Monad (u t m)
  ) => MonadTransTrans
    (u :: ((* -> *) -> (* -> *)) -> (* -> *) -> * -> *)
  where
    liftT
      :: (Monad m, MonadTrans t)
      => t m a -> u t m a
```

Similar to our `MonadTrans`, `MonadTransTrans` has quantified constraints that ensure it can't have undisciplined instances. Intuitively, where a monad transformer takes monads to monads, a monad transformer transformer takes monad transformers to monad transformers.

What is the purpose of doing that? Isn't just one kind of transformer enough to build composable effects? In principle, yes. But splitting the _base monad_ of the transformer paradigm into a _base monad and transformer_ will let us write more modular and more easily testable code, as we will see. In a nutshell, in the transformer paradigm we can defer the choice of `m` to client code, but with transformer transformers we defer the choice of `t` to the client and then choose `m` at _run time_. `m` is the effect monad. In production it will be `IO` or similar, but for testing we can swap out `IO` for a test environment. Then `t` is the client's escape hatch for adding functionality to a stack of prepackaged transformer transformers.

Hopefully this will make more sense with some concrete examples.

Anyway, monad transformer transformers act like a _monad construction kit_, much like transformers do. The two most basic tools in our kit are `IdentityTT` and `OverTT`.



IdentityTT
----------

`IdentityTT` is the "trivial" transformer transformer adding no new features. This sounds boring, but like the `mark` parameter on our monadic effect classes, `IdentityTT` has a very important role to play.

```haskell
data IdentityTT t m a
  = IdentityTT { unIdentityTT :: t m a }
```

Here's an example using `IdentityTT`.

> newtype Foo t m a = Foo
>   { unFoo :: IdentityTT t m a
>   } deriving (Functor, Applicative, Monad, MonadTrans, MonadTransTrans)

The definition of `Foo` is typical for code using `trans-fx`: it is a newtype wrapper around a transformer transformer built from prefabricated parts. Note also how we're using `GeneralizedNewtypeDeriving` here; this lets us avoid a bunch of tedious boilerplate.

Right away we can start writing monadic code with `Foo`, although we can't do much! All we know about `IdentityTT t m a` is that it's a monad.

> test1 :: (Monad m, MonadTrans t) => Foo t m Bool
> test1 = return True

The most useful thing we can do with a monadic computation, after defining it, is _run_ it. All of the built in monads have a corresponding `run` function. But unlike the run functions from `transformers`, the run function in `trans-fx` is generic; there is a `RunMonad` class with `run`, a `RunMonadTrans` class with `runT`, and a `RunMonadTransTrans` class with `runTT`. In each case `run*` takes an _evaluation context_ and a monadic computation and produces a value inside an _output context_.

For transformer transformers that looks like this:

```haskell
class
  ( MonadTransTrans u, Commutant f
  ) => RunMonadTransTrans z u f | u -> z f
  where
    runTT
      :: (Monad m, MonadTrans t)
      => z m -> u t m a -> t m (f a)
```

where `Commutant` is a class of functors which 'commute' with every applicative functor. (Handling `run*` generically like this makes it simple to run composites.)

The run instance for `IdentityTT` looks like this:

```haskell
instance
  RunMonadTransTrans Unit IdentityTT Identity
  where
    runTT
      :: (Monad m, MonadTrans t)
      => Unit m
      -> IdentityTT t m a
      -> t m (Identity a)
    runTT Unit (IdentityTT x) = fmap Identity x

data Unit (a :: * -> *) = Unit
```

Typical client code will settle on one inner transformer `t` and maybe a handful of effect monads `m`. We can specialize `runTT` to our particular monad for convenience.

> runFoo :: (Monad m) => Foo IdentityT m a -> m a
> runFoo = fmap unIdentity . unIdentityT . runTT Unit . unFoo

Now we can run `Foo` computations in different effect monads.

```haskell
$> runFoo test1 :: IO Bool
True

$> runFoo test1 :: Identity Bool
Identity {unIdentity :: True}

$> runFoo test1 :: Maybe Bool
Just True
```

Neat! This example is typical of how we can use `trans-fx` in three steps:

1. Decide which side effects we want in a monad,
2. Define a `newtype` transformer transformer that provides those effects, and
3. Define a specialized `run` function to evaluate monadic computations.

Next we'll add some more effects with `OverTT`.



OverTT
------

`OverTT` is barely a step up from `IdentityTT`. `OverTT u v` takes a transformer transformer `u` and a transformer `v`, and applies `v` "over the top" of `u`. Specifically:

```haskell
data OverTT u v t m a
  = OverTT { unOverTT :: v (u t m) a }
```

The way I remember this is that `u` goes (u)nder and `v` goes o(v)er. The library comes with the usual monad transformers built-in:

* `IdentityT`: the identity transformer
* `StateT mark s`: mutable state
* `ReadOnlyT mark r`: read-only state
* `WriteOnlyT mark w`: write-only state
* `ExceptT mark e`: exceptions
* `CompositeT`: composite transformers

(Actually `v` needs to be something a little stronger -- a `MonadFunctor` -- but the built in transformers all satisfy this.)

Here's an example type. `Bar` gives us access to an additional write-only state, a list of booleans tagged with `W`.

> newtype Bar t m a = Bar
>   { unBar ::
>       OverTT
>         IdentityTT
>         (WriteOnlyT W [Bool]) t m a
>   } deriving
>     ( Functor, Applicative, Monad
>     , MonadWriteOnly W [Bool] )

Note the `MonadWriteOnly` instance with a `W` parameter -- remember this has to be isomorphic to `Identity`, so its implementation is trivial. (I haven't found a good way to avoid writing this code, but it's not terrible.) We can name `W` anything we want and in practice it should have a unique and descriptive name.

> data W a = W
>   { unW :: a
>   } deriving (Eq, Show)
> instance Functor W where
>   fmap f (W x) = W (f x)
> instance Applicative W where
>   pure = W
>   (W f) <*> (W x) = W (f x)
> instance Monad W where
>   return = W
>   (W x) >>= f = f x
> instance MonadIdentity W where
>   unwrap = unW
> instance (Semigroup x) => Semigroup (W x) where
>   (W a) <> (W b) = W (a <> b)
> instance (Monoid x) => Monoid (W x) where
>   mempty = W mempty
>   mappend = (<>)

Next a helper for running `Bar`s. `Sing` is a helper type for building the evaluation context for `OverTT`.

> runBar
>   :: ( Monad m )
>   => Bar IdentityT m a
>   -> m (Pair (W [Bool]) a)
> runBar =
>   fmap (unIdentity . unCompose)
>     . unIdentityT . runTT (Sing Unit (W ())) . unBar

Now `Bar` comes with the additional technology of the `MonadWriteOnly` class, namely special functions `draft` and `tell`.

> test2 :: (Monad m, MonadTrans t) => Bar t m ()
> test2 = do
>   tell $ W [True]
>   tell $ W [False]
>   return ()

And we can run `Bar`s in several different effect monads.

```haskell
$> runBar test2 :: IO (Pair (W [Bool]) ())
Pair {slot1 = W {unW = [True,False]}, slot2 = ()}

$> runBar test2 :: Identity (Pair (W [Bool]) ())
Identity {unIdentity =
  Pair {slot1 = W {unW = [True,False]}, slot2 = ()}}

$> runBar test2 :: Maybe (Pair (W [Bool]) ())
Just (Pair {slot1 = W {unW = [True,False]}, slot2 = ()})
```

Let's see a similar example, this time with two write only layers.

> newtype Baz t m a = Baz
>   { unBaz ::
>       OverTT
>         IdentityTT
>         (ComposeT
>           (WriteOnlyT V [Bool])
>           (WriteOnlyT W [Bool]))
>         t m a
>   } deriving
>     ( Functor, Applicative, Monad
>     , MonadWriteOnly W [Bool]
>     , MonadWriteOnly V [Bool] )

We also need the boilerplate for `V`.

> data V a = V
>   { unV :: a
>   } deriving (Eq, Show)
> instance Functor V where
>   fmap f (V x) = V (f x)
> instance Applicative V where
>   pure = V
>   (V f) <*> (V x) = V (f x)
> instance Monad V where
>   return = V
>   (V x) >>= f = f x
> instance MonadIdentity V where
>   unwrap = unV
> instance (Semigroup x) => Semigroup (V x) where
>   (V a) <> (V b) = V (a <> b)
> instance (Monoid x) => Monoid (V x) where
>   mempty = V mempty
>   mappend = (<>)

And a runner:

> runBaz
>   :: ( Monad m )
>   => Baz IdentityT m a
>   -> m (Pair (W [Bool]) (Pair (V [Bool]) a))
> runBaz =
>   fmap (unCompose . unIdentity . unCompose)
>     . unIdentityT . runTT (Sing Unit (V (), W ())) . unBaz

Now in `Baz` we have access to two different write-only states, one called `W` and one called `V`.

> test3 :: (Monad m, MonadTrans t) => Baz t m ()
> test3 = do
>   tell $ W [True]
>   tell $ V [False]
>   return ()

...and as before we can run `Baz`s in different effect monads.

```haskell
$> runBaz test3 :: IO (Pair (W [Bool]) (Pair (V [Bool]) ()))
Pair
  { slot1 = W {unW = [True]}
  , slot2 = Pair
    { slot1 = V {unV = [False]}
    , slot2 = ()}}

$> runBaz test3 :: Identity (Pair (W [Bool]) (Pair (V [Bool]) ()))
Identity {unIdentity = Pair
  { slot1 = W {unW = [True]}
  , slot2 = Pair
    { slot1 = V {unV = [False]}
    , slot2 = ()}}}
```

One more example, this time with mutable state and exceptions.

> newtype Qux t m a = Qux
>   { unQux ::
>       OverTT
>         IdentityTT
>         (ComposeT
>           (StateT V Int)
>           (ExceptT W Bool))
>         t m a
>   } deriving
>     ( Functor, Applicative, Monad
>     , MonadExcept W Bool
>     , MonadState V Int )

Run:

> runQux
>   :: ( Monad m )
>   => Qux IdentityT m a
>   -> m (Except W Bool (Pair (V Int) a))
> runQux =
>   fmap (unCompose . unIdentity . unCompose)
>     . unIdentityT . runTT (Sing Unit (V 0, W ())) . unQux

And some examples:

> test4 :: (Monad m, MonadTrans t) => Qux t m Int
> test4 = do
>   put $ V (27 :: Int)
>   V k <- get
>   return (10 * k)

```haskell
$> runQux test4 :: IO (Except W Bool (Pair (V Int) Int))
Accept (Pair {slot1 = V {unV = 27}, slot2 = 270})
```

> test5 :: (Monad m, MonadTrans t) => Qux t m ()
> test5 = do
>   put $ V (5 :: Int)
>   throw $ W False

```haskell
$> runQux test5 :: IO (Except W Bool (Pair (V Int) ()))
Except False
```
