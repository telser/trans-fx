---
title: Doing IO
---

Now let's add some IO effects.

> {-# LANGUAGE DerivingVia                #-}
> {-# LANGUAGE FlexibleInstances          #-}
> {-# LANGUAGE DerivingStrategies         #-}
> {-# LANGUAGE ScopedTypeVariables        #-}
> {-# LANGUAGE FlexibleContexts           #-}
> {-# LANGUAGE MultiParamTypeClasses      #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
>
> module Control.FX.Demo.DoingIO where

Some simple IO-lite monads are in the `trans-fx-io` package, which we can import all at once with `Control.FX.IO`.

> import Control.FX
> import Control.FX.IO

The simplest way to make a monad transformer transformer that does IO-like actions is using `PromptTT`, taken shamelessly from the `MonadPrompt` library. The Prompt pattern lets us define some monadic actions to be carried out later using an _interpreter_ that we control. The `PromptTT` type is provided by `trans-fx-core` but typically we won't use it raw. Instead we'll specialize it to some particular IO effects we care about.

The simplest of these is probably `TeletypeTT`: a monad transformer transformer that adds the ability to read and write strings of text on a teletype-like interface.

Here's a simple type using `TeletypeTT`. (`S` and `T` are again boilerplate `MonadIdentity` types for disambiguation.)

> newtype Bar t m a = Bar
>   { unBar ::
>       StateTT T String
>       (ExceptTT S Bool
>       (TeletypeTT S
>       t)) m a
>   } deriving
>     ( Functor, Applicative, Monad, MonadTrans
>     , MonadState T String
>     , MonadExcept S Bool
>     , MonadTeletype S
>     )

Couple things to note: with `TeletypeTT` added to our transformer transformer stack, we can derive two new interfaces: `MonadTeletype` comes with the functions for interacting with the teletype, and we can also derive a handler for IO exceptions originating in the teletype.

Now the `run` function for `TeletypeTT` takes an _interpreter_ that runs the teletype effects in some specific base monad. There's a default IO implementation, but we could swap that out for one that runs in a test environment. Notably, because the teletype interface is named we could have more than one teletype layer in the stack and interpret them differently.

Anyway, here is an example.

> test3 :: (Monad m, MonadTrans t) => Bar t m ()
> test3 = do
>   printLine $ S "foo"
>   put $ T "Foo"
>   return ()

And here is a runner. `evalTeletypeIO` is the default teletype interpreter. Again, it's much easier to write the runner than to see in advance what its type is, so I did that and used GHC to infer the type.

> --runBar
> --  :: Bar IdentityT IO a
> --  -> IO (Except S Bool
> --       (Pair (T String)
> --       (Except TeletypeError (S IOException) a)))
> runBar =
>   unIdentityT
>     . runTeletypeTT (Eval evalTeletypeIO)
>     . runExceptTT (S ())
>     . runStateTT (T "")
>     . unBar

(Boilerplate)

> data S a = S { unS :: a }
>   deriving stock ( Eq, Show )
>   deriving ( Functor, Applicative, Monad
>            , MonadIdentity ) via (Wrap S)
>   deriving ( Semigroup, Monoid ) via (Wrap S a)
> 
> instance Renaming S where
>   namingMap = S
>   namingInv = unS
> 
> instance Commutant S where
>   commute = fmap S . unS
>
> data T a = T { unT :: a }
>   deriving stock ( Eq, Show )
>   deriving ( Functor, Applicative, Monad
>            , MonadIdentity ) via (Wrap T)
>   deriving ( Semigroup, Monoid ) via (Wrap T a)
> 
> instance Renaming T where
>   namingMap = T
>   namingInv = unT
> 
> instance Commutant T where
>   commute = fmap T . unT

When defining an effect stack it's important to remember that (1) changing the order of the effect layers changes the semantics of the monad, and (2) effect class instances do not always "commute". Fortunately in both cases the type checker is our friend.
