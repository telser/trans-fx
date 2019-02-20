-- | Module      : Control.FX.Monad.Trans.Trans.IdentityTT
--   Description : Concrete identity monad transformer transformer
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.Trans.Trans.IdentityTT (
    IdentityTT(..)
  , Unit(..)
) where



import Data.Typeable (Typeable)

import Control.FX.Monad
import Control.FX.Monad.Trans
import Control.FX.Monad.Trans.Trans.Class



-- | Concrete identity monad transformer transformer
data IdentityTT
  (t :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = IdentityTT
        { unIdentityTT :: t m a
        } deriving (Eq, Show, Typeable)

instance
  ( Monad m, MonadTrans t
  ) => Functor (IdentityTT t m)
  where
    fmap
      :: (a -> b)
      -> IdentityTT t m a
      -> IdentityTT t m b
    fmap f = IdentityTT . fmap f . unIdentityTT

instance
  ( Monad m, MonadTrans t
  ) => Applicative (IdentityTT t m)
  where
    pure
      :: a
      -> IdentityTT t m a
    pure = IdentityTT . pure

    (<*>)
      :: IdentityTT t m (a -> b)
      -> IdentityTT t m a
      -> IdentityTT t m b
    (IdentityTT f) <*> (IdentityTT x) =
      IdentityTT (f <*> x)

instance
  ( Monad m, MonadTrans t
  ) => Monad (IdentityTT t m)
  where
    return
      :: a
      -> IdentityTT t m a
    return = IdentityTT . return

    (>>=)
      :: IdentityTT t m a
      -> (a -> IdentityTT t m b)
      -> IdentityTT t m b
    (IdentityTT x) >>= f =
      IdentityTT (x >>= (unIdentityTT . f))

instance
  ( MonadTrans t
  ) => MonadTrans (IdentityTT t)
  where
    lift
      :: ( Monad m )
      => m a
      -> IdentityTT t m a
    lift = IdentityTT . lift

instance
  ( MonadFunctor t
  ) => MonadFunctor (IdentityTT t)
  where
    hoist
      :: ( Monad m, Monad n )
      => (forall u. m u -> n u)
      -> IdentityTT t m a
      -> IdentityTT t n a
    hoist f = IdentityTT . hoist f . unIdentityTT

instance
  MonadTransTrans IdentityTT
  where
    liftT
      :: ( Monad m, MonadTrans t )
      => t m a
      -> IdentityTT t m a
    liftT = IdentityTT

instance
  MonadTransFunctor IdentityTT
  where
    hoistT
      :: ( Monad m, MonadFunctor t1, MonadFunctor t2 )
      => (forall n x. (Monad n) => t1 n x -> t2 n x)
      -> IdentityTT t1 m a
      -> IdentityTT t2 m a
    hoistT f = IdentityTT . f . unIdentityTT

    raiseT
      :: ( Monad m1, Monad m2, MonadFunctor t )
      => (forall x. m1 x -> m2 x)
      -> IdentityTT t m1 x
      -> IdentityTT t m2 x
    raiseT f = IdentityTT . hoist f . unIdentityTT

instance
  RunMonadTransTrans Unit IdentityTT Identity
  where
    runTT
      :: (Monad m, MonadTrans t)
      => Unit m
      -> IdentityTT t m a
      -> t m (Identity a)
    runTT Unit (IdentityTT x) = fmap Identity x

-- | Helper type for running @IdentityTT@
data Unit (a :: * -> *)
  = Unit
  deriving (Eq, Show, Typeable)
