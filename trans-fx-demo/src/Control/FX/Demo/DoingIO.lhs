---
title: Composing Transformers
---

> {-# LANGUAGE DerivingVia                #-}
> {-# LANGUAGE ScopedTypeVariables        #-}
> {-# LANGUAGE FlexibleInstances          #-}
> {-# LANGUAGE DerivingStrategies         #-}
> {-# LANGUAGE MultiParamTypeClasses      #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
>
> module Control.FX.Demo.DoingIO where

> import Control.Exception
>   ( IOException )

> import Control.FX
> import Control.FX.IO

> newtype Mung t m a = Mung
>   { unMung ::
>       TeletypeTT S
>       (StateTT T String
>       (StateTT S Bool
>       t)) m a
>   } deriving
>     ( Functor, Applicative, Monad, MonadTrans
>     , MonadState T String
>     , MonadState S Bool
>     , MonadTeletype S
>     , MonadExcept TeletypeError IOException
>     )

> test6 :: (Monad m, MonadTrans t) => Mung t m ()
> test6 = do
>   printLine $ S "foo"
>   S (p :: Bool) <- get
>   printLine $ S (show p)
>   put $ T "Foo"
>   return ()

 > runMung
 >   :: Mung IdentityT IO a
 >   -> IO (Pair (T [Char]) (Except TeletypeError IOException a))

> runMung =
>   unIdentityT
>     . runStateTT (S False)
>     . runStateTT (T "")
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
