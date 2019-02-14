{-#
  LANGUAGE
    InstanceSigs,
    KindSignatures,
    FlexibleInstances,
    QuantifiedConstraints,
    MultiParamTypeClasses
#-}

module Control.FX.Monad.Trans.Trans.IdentityTT (
    IdentityTT(..)
  , runIdentityTT
) where

import Data.Typeable (Typeable)

import Control.FX.Monad
import Control.FX.Monad.Trans
import Control.FX.Monad.Trans.Trans.Class

data IdentityTT
  (t :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = IdentityTT
        { unIdentityTT :: t m a
        } deriving (Show, Typeable)

instance
  ( Monad m, MonadTrans t
  ) => Functor (IdentityTT t m)
  where
    fmap f = IdentityTT . fmap f . unIdentityTT

instance
  ( Monad m, MonadTrans t
  ) => Applicative (IdentityTT t m)
  where
    pure = IdentityTT . pure

    (IdentityTT f) <*> (IdentityTT x) =
      IdentityTT (f <*> x)

instance
  ( Monad m, MonadTrans t
  ) => Monad (IdentityTT t m)
  where
    return = IdentityTT . return

    (IdentityTT x) >>= f =
      IdentityTT (x >>= (unIdentityTT . f))

instance
  ( MonadTrans t
  ) => MonadTrans (IdentityTT t)
  where
    lift = IdentityTT . lift

instance
  ( MonadFunctor t
  ) => MonadFunctor (IdentityTT t)
  where
    hoist f = IdentityTT . hoist f . unIdentityTT

instance MonadTransTrans IdentityTT where
  liftT = IdentityTT

instance MonadTransFunctor IdentityTT where
  hoistT f = IdentityTT . f . unIdentityTT

  raiseT f = IdentityTT . hoist f . unIdentityTT

instance RunMonadTransTrans Unit IdentityTT Identity where
  runTT
    :: (Monad m, MonadTrans t)
    => Unit m -> IdentityTT t m a -> t m (Identity a)
  runTT Unit (IdentityTT x) = fmap Identity x

runIdentityTT
  :: (Monad m, MonadTrans t)
  => IdentityTT t m a -> t m a
runIdentityTT = fmap unIdentity . runTT Unit
