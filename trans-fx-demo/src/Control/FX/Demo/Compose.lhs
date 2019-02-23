---
title: Composing Transformers
---

> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
>
> module Control.FX.Demo.Compose where

> import Control.Exception
>   ( IOException )

> import Control.FX
> import Control.FX.IO

> newtype Mung t m a = Mung
>   { unMung ::
>       TeletypeTT S
>         (OverTT
>           (TeletypeTT T)
>           (StateT S String) t)
>       m a
>   } deriving
>     ( Functor, Applicative, Monad, MonadTrans
>     , MonadState S String, MonadTeletype S, MonadTeletype T
>     , MonadExcept TeletypeError IOException )

> test6 :: (Monad m, MonadTrans t) => Mung t m ()
> test6 = do
>   printLine $ S "foo"
>   printLine $ T "foo"
>   put $ S "Foo"
>   return ()

> runMung
>   :: Mung IdentityT IO a -> IO (Compose
>                            (Except TeletypeError IOException)
>                            (Pair (S [Char]))
>                            (Except TeletypeError IOException a))
> runMung = unIdentityT . runOverTT (Eval evalTeletypeIO) (S "") . runTeletypeTT (Eval evalTeletypeIO) . unMung

> {-

> instance
>   LiftTeletype S Mung
>     (OverTT IdentityTT (StateT S String))
>   where
>     liftTeletype _ = Mung . ComposeTT

> runMung
>   :: Mung IdentityT IO a
>   -> IO (Pair
>           (S [Char])
>           (Except TeletypeError IOException a))
> runMung x = fmap unIdentity $ fmap unCompose $ fmap unCompose $ unIdentityT $ runTT (Dub (Eval evalTeletypeIO) (Sing Unit (S ""))) (unMung x)

 > foo = Mung . ComposeTT . liftT

> -}

> data S a = S
>   { unS :: a
>   } deriving (Eq, Show)
> instance Functor S where
>   fmap f (S x) = S (f x)
> instance Commutant S where
>   commute (S x) = fmap S x
> instance Applicative S where
>   pure = S
>   (S f) <*> (S x) = S (f x)
> instance Monad S where
>   return = S
>   (S x) >>= f = f x
> instance MonadIdentity S where
>   unwrap = unS
> instance (Semigroup x) => Semigroup (S x) where
>   (S a) <> (S b) = S (a <> b)
> instance (Monoid x) => Monoid (S x) where
>   mempty = S mempty
>   mappend = (<>)

> data T a = T
>   { unT :: a
>   } deriving (Eq, Show)
> instance Functor T where
>   fmap f (T x) = T (f x)
> instance Commutant T where
>   commute (T x) = fmap T x
> instance Applicative T where
>   pure = T
>   (T f) <*> (T x) = T (f x)
> instance Monad T where
>   return = T
>   (T x) >>= f = f x
> instance MonadIdentity T where
>   unwrap = unT
> instance (Semigroup x) => Semigroup (T x) where
>   (T a) <> (T b) = T (a <> b)
> instance (Monoid x) => Monoid (T x) where
>   mempty = T mempty
>   mappend = (<>)
