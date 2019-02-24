---
title: Composing Transformers
---

> {-# LANGUAGE DerivingVia                #-}
> {-# LANGUAGE FlexibleInstances          #-}
> {-# LANGUAGE DerivingStrategies         #-}
> {-# LANGUAGE MultiParamTypeClasses      #-}
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
>       (OverTT
>         (TeletypeTT T)
>         (ComposeT (StateT S String) (ExceptT T Bool))
>       t) m a
>   } deriving
>     ( Functor, Applicative, Monad, MonadTrans
>     , MonadState S String
>     , MonadTeletype S
>     , MonadTeletype T
>     , MonadExcept TeletypeError IOException
>     , MonadExcept T Bool )

> test6 :: (Monad m, MonadTrans t) => Mung t m ()
> test6 = do
>   printLine $ S "foo"
>   printLine $ T "foo"
>   put $ S "Foo"
>   throw $ T False
>   return ()

> runMung
>   :: Mung IdentityT IO a
>   -> IO (Compose
>             (Except TeletypeError IOException)
>             (Compose (Except T Bool) (Pair (S [Char])))
>             (Except TeletypeError IOException a))
> runMung =
>   unIdentityT
>     . runOverTT (Eval evalTeletypeIO) (S "", T ())
>     . runTeletypeTT (Eval evalTeletypeIO)
>     . unMung


> data S a = S { unS :: a }
>   deriving stock
>     ( Eq, Show )
>   deriving
>     ( Functor, Applicative, Monad, MonadIdentity )
>     via (Wrap S)
>   deriving
>     ( Semigroup, Monoid )
>     via (Wrap S a)
> 
> instance Renaming S where
>   namingMap = S
>   namingInv = unS
> 
> instance Commutant S where
>   commute = fmap S . unS


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
