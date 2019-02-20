-- | Module      : Control.FX.Monad.Trans.Trans.ComposeTT
--   Description : Concrete composite monad transformer transformer
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.Trans.Trans.ComposeTT (
    ComposeTT(..)
  , Dub(..)
) where

import Data.Typeable (Typeable, typeOf)



import Control.FX.Monad
import Control.FX.Monad.Trans
import Control.FX.Monad.Trans.Trans.Class
import Control.FX.Monad.Trans.Trans.ApplyTT



-- | Concrete composite monad transformer transformer
data ComposeTT
  (u1 :: ((* -> *) -> (* -> *)) -> (* -> *) -> * -> *)
  (u2 :: ((* -> *) -> (* -> *)) -> (* -> *) -> * -> *)
  (t :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = ComposeTT
        { unComposeTT :: u1 (u2 t) m a
        } deriving (Typeable)

deriving instance
  ( Show (u1 (u2 t) m a)
  ) => Show (ComposeTT u1 u2 t m a)

instance
  ( Monad m, MonadTrans t, MonadTransTrans u1, MonadTransTrans u2
  ) => Functor (ComposeTT u1 u2 t m)
  where
    fmap
      :: (a -> b)
      -> ComposeTT u1 u2 t m a
      -> ComposeTT u1 u2 t m b
    fmap f = ComposeTT . fmap f . unComposeTT

instance
  ( Monad m, MonadTrans t, MonadTransTrans u1, MonadTransTrans u2
  ) => Applicative (ComposeTT u1 u2 t m)
  where
    pure
      :: a
      -> ComposeTT u1 u2 t m a
    pure = ComposeTT . pure

    (<*>)
      :: ComposeTT u1 u2 t m (a -> b)
      -> ComposeTT u1 u2 t m a
      -> ComposeTT u1 u2 t m b
    (ComposeTT f) <*> (ComposeTT x) =
      ComposeTT (f <*> x)

instance
  ( Monad m, MonadTrans t, MonadTransTrans u1, MonadTransTrans u2
  ) => Monad (ComposeTT u1 u2 t m)
  where
    return
      :: a
      -> ComposeTT u1 u2 t m a
    return = ComposeTT . return

    (>>=)
      :: ComposeTT u1 u2 t m a
      -> (a -> ComposeTT u1 u2 t m b)
      -> ComposeTT u1 u2 t m b
    (ComposeTT x) >>= f =
      ComposeTT (x >>= (unComposeTT . f))

instance
  ( MonadTrans t, MonadTransTrans u1, MonadTransTrans u2
  ) => MonadTrans (ComposeTT u1 u2 t)
  where
    lift
      :: ( Monad m )
      => m a
      -> ComposeTT u1 u2 t m a
    lift = ComposeTT . lift

instance
  ( MonadFunctor t, MonadTransFunctor u1, MonadTransFunctor u2
  ) => MonadFunctor (ComposeTT u1 u2 t)
  where
    hoist
      :: ( Monad m, Monad n )
      => (forall x. m x -> n x)
      -> ComposeTT u1 u2 t m a
      -> ComposeTT u1 u2 t n a
    hoist f = ComposeTT . raiseT f . unComposeTT

instance
  ( MonadTransTrans u1, MonadTransTrans u2
  ) => MonadTransTrans (ComposeTT u1 u2)
  where
    liftT
      :: ( Monad m, MonadTrans t )
      => t m a
      -> ComposeTT u1 u2 t m a
    liftT = ComposeTT . liftT . liftT

instance
  ( RunMonadTransTrans z1 u1 f1, RunMonadTransTrans z2 u2 f2
  ) => RunMonadTransTrans (Dub z1 z2) (ComposeTT u1 u2) (Compose f2 f1)
  where
    runTT
      :: ( Monad m, MonadTrans t )
      => Dub z1 z2 m
      -> ComposeTT u1 u2 t m a
      -> t m (Compose f2 f1 a)
    runTT (Dub z1 z2) =
      fmap Compose . runTT z2 . runTT z1 . unComposeTT

-- | Helper type for running @CompositeTT@
data Dub
  (z1 :: (* -> *) -> *)
  (z2 :: (* -> *) -> *)
  (m :: * -> *)
    = Dub (z1 m) (z2 m)

instance
  ( Typeable z1, Typeable z2, Typeable m
  ) => Show (Dub z1 z2 m)
  where
    show
      :: Dub z1 z2 m
      -> String
    show = show . typeOf
