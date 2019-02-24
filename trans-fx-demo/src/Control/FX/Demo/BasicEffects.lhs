---
title: Getting Started
---

Hi! This is a technical introduction to `trans-fx`, an effect framework library for Haskell. To say anything interesting I'll have to make a few assumptions about you, dear reader, and I think it's best to be up front about these. This document will make the most sense to you if:

1. You have some experience writing in a functional programming language, and a language with Hindley-Milner style type inference, and found these ideas to be to your liking.
2. You appreciate the use of monads to control side-effects and the use of monad transformers as a strategy for building complex monads from simpler ones.
3. You are invested in testing as a tool for building software that is responsive to changing requirements and resistant to decay.

To use `trans-fx` we'll need a pretty recent version of GHC; the library code depends on some newer (GHC >= 8.6) language extensions. Client code will also benefit considerably from the following extensions -- these are not strictly necessary, but will help cut out a _ton_ of trivial boilerplate.

> {-# LANGUAGE DerivingVia                #-}
> {-# LANGUAGE DerivingStrategies         #-}
> {-# LANGUAGE ScopedTypeVariables        #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
>
> module Control.FX.Demo.BasicEffects where

In this intro we'll focus on pure examples. For this we can import just one module:

> import Control.FX

Examples that use `IO` are not much more complicated, but we'll save them for later.



What is different?
==================

This library is heavily influenced by but incompatible with `transformers`, `mtl`, and `mmorph`. The two most significant differences are as follows.

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

`mark` is the extra parameter, and it is required to be isomorphic to `Identity` -- i.e. _trivial_. What is the point of that? We could get rid of `mark` without changing the meanings of the effect functions `get` and `put`. But including `mark` in the class definition allows for a single monad to have multiple `MonadState` instances, even with the same state type, and the typechecker can disambiguate them using `mark`. This is important because we want to allow compound transformers from different authors to use the same effect machinery out of the box.

Another handy feature of `mark` is that it acts like machine checked documentation of where effect values come from.



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

Anyway, monad transformer transformers act like a _monad construction kit_, much like transformers do. The basic algebraic effect monads have transformer-transformer analogues:

* `StateTT mark s` for mutable state `s`
* `ReadOnlyTT mark r` for read-only state `r` (a.k.a. Reader)
* `WriteOnlyTT mark w` for write-only state `w` (a.k.a. Writer)
* `ExceptTT mark e` for exceptions `e` (a.k.a. Either)
* `HaltTT mark` for stopping the computation (a.k.a. Maybe)
* `IdentityTT` for doing nothing new :)

And each basic effect monad also comes in class form, where the class methods are effect primitives.

* `MonadState mark s`, with `get` and `put`
* `MonadReadOnly mark r`, with `ask` and `local`
* `MonadWriteOnly mark w`, with `tell` and `draft`
* `MonadExcept mark e`, with `throw` and `catch`
* `MonadHalt mark`, with `halt`

If you're familiar with monad transformers (which I assume you are!) this is all familiar stuff.



An Example
----------

Let's look at an example transformer transformer stack to see what we can do.

For starters, we will need some `mark` types we can use to disambiguate effect classes. To keep it simple I'll make two, called `A` and `B`. This part is boilerplate, but we can reduce it a little with some deriving extensions.

> data A a = A { unA :: a }
>   deriving stock
>     ( Eq, Show )
>   deriving
>     ( Functor, Applicative, Monad, MonadIdentity )
>     via (Wrap A)
>   deriving
>     ( Semigroup, Monoid )
>     via (Wrap A a)
> 
> instance Renaming A where
>   namingMap = A
>   namingInv = unA
> 
> instance Commutant A where
>   commute = fmap A . unA
> 
> data B a = B { unB :: a }
>   deriving stock
>     ( Eq, Show )
>   deriving
>     ( Functor, Applicative, Monad, MonadIdentity )
>     via (Wrap B)
>   deriving
>     ( Semigroup, Monoid )
>     via (Wrap B a)
> 
> instance Renaming B where
>   namingMap = B
>   namingInv = unB
> 
> instance Commutant B where
>   commute = fmap B . unB

With that out of the way, let's make a monad! Here's a stack of state, read-only, and except transformer transformers.

> newtype Foo t m a = Foo
>   { unFoo ::
>       (StateTT A String
>       (ReadOnlyTT A Int
>       (ExceptTT B Bool
>       (StateTT B String
>         t)))) m a
>   } deriving
>     ( Functor, Applicative, Monad, MonadTrans
>     , MonadState A String
>     , MonadReadOnly A Int
>     , MonadState B String
>     , MonadExcept B Bool
>     )
>
> instance MonadTransTrans Foo where
>   liftT = Foo . liftT . liftT . liftT . liftT

The definition of `Foo` is typical for code using `trans-fx`: it is a newtype wrapper around a transformer transformer built from prefabricated parts. Note the last four deriving clauses. These give us the state, read-only, and exception primitives for free thanks to the magic of `GeneralizedNewtypeDeriving`. Note also that we've got two different `MonadState` instances.

Here's a super basic computation in the `Foo t m` monad.

> test1 :: (Monad m) => Foo IdentityT m ()
> test1 = do
>   A (k :: Int) <- ask
>   put $ A "Hello"
>   put $ B "World"
>   throw $ B True
>   return ()

To actually execute a monadic action, we can execute each layer one at a time -- each built in transformer transformer comes with a `run` function for this purpose.

(A hint for writing runners: it's much easier to write `runFoo` than it is to see what the type of `runFoo` will be in advance. I wrote the body of `runFoo` and let GHC infer the signature for me.)

> runFoo
>   :: (Monad m)
>   => Foo IdentityT m a
>   -> m (Pair (B String) (Except B Bool (A (Pair (A String) a))))
> runFoo =
>   unIdentityT
>   . runStateTT (B "bar")
>   . runExceptTT (B ())
>   . runReadOnlyTT (A 3)
>   . runStateTT (A "foo")
>   . unFoo

I really like this. :) The `run` function looks a lot like the definition of `Foo` in reverse, which makes it simple to write and edit. One of the design goals for `trans-fx` is to make _prototyping_ complex effect monads easy, and this is one example of how we aim to do that. We can rearrange the layers in the transformer transformer stack and in the run function, and no futzing with `lift` or defining aliases for the effect primitives is necessary.

(Of course we _can_ futz with lift, or rather `liftT` for transformer transformers. Here is `test1` again, this time with explicit `liftT`s.)

> test2 :: (Monad m) => Foo IdentityT m ()
> test2 = do
>   A (k :: Int) <- Foo $ liftT ask
>   Foo $ put $ A "Hello"
>   Foo $ liftT $ liftT $ liftT $ put $ B "World"
>   Foo $ liftT $ liftT $ throw $ B True
>   return ()

Now we can run `Foo` computations in different effect monads.

```haskell
$> runFoo (test1 :: Foo IdentityT IO ())
Pair
  { slot1 = B {unB = "World"}
  , slot2 = Except True }

$> runFoo (test1 :: Foo IdentityT Identity ())
Identity
  { unIdentity = Pair
    { slot1 = B {unB = "World"}
    , slot2 = Except True} }

$> runFoo (test1 :: Foo IdentityT Maybe ())
Just (Pair
  { slot1 = B {unB = "World"}
  , slot2 = Except True })
```

Neat! This example is typical of how we can use `trans-fx` to build an effectful monad in three steps:

1. Decide which side effects we want in a monad, and in what order
2. Define a `newtype` transformer transformer that provides those effects
3. Define a specialized `run` function to evaluate monadic computations
