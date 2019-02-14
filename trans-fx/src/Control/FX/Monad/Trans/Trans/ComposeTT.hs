{-#
  LANGUAGE
    InstanceSigs,
    KindSignatures,
    FlexibleInstances,
    UndecidableInstances,
    QuantifiedConstraints,
    MultiParamTypeClasses
#-}

module Control.FX.Monad.Trans.Trans.ComposeTT (
    ComposeTT(..)
  , runComposeTT
) where

import Data.Typeable (Typeable)

import Control.FX.Monad
import Control.FX.Monad.Trans
import Control.FX.Monad.Trans.Trans.Class
import Control.FX.Monad.Trans.Trans.ApplyTT

data ComposeTT
  (u1 :: ((* -> *) -> (* -> *)) -> (* -> *) -> * -> *)
  (u2 :: ((* -> *) -> (* -> *)) -> (* -> *) -> * -> *)
  (t :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = ComposeTT
        { unComposeTT :: u1 (u2 t) m a
        } deriving (Typeable)

instance
  ( Monad m, MonadTrans t, MonadTransTrans u1, MonadTransTrans u2
  ) => Functor (ComposeTT u1 u2 t m)
  where
    fmap f = ComposeTT . fmap f . unComposeTT

instance
  ( Monad m, MonadTrans t, MonadTransTrans u1, MonadTransTrans u2
  ) => Applicative (ComposeTT u1 u2 t m)
  where
    pure = ComposeTT . pure

    (ComposeTT f) <*> (ComposeTT x) =
      ComposeTT (f <*> x)

instance
  ( Monad m, MonadTrans t, MonadTransTrans u1, MonadTransTrans u2
  ) => Monad (ComposeTT u1 u2 t m)
  where
    return = ComposeTT . return

    (ComposeTT x) >>= f =
      ComposeTT (x >>= (unComposeTT . f))

instance
  ( MonadTrans t, MonadTransTrans u1, MonadTransTrans u2
  ) => MonadTrans (ComposeTT u1 u2 t)
  where
    lift = ComposeTT . lift

instance
  ( MonadFunctor t, MonadTransFunctor u1, MonadTransFunctor u2
  ) => MonadFunctor (ComposeTT u1 u2 t)
  where
    hoist f = ComposeTT . raiseT f . unComposeTT

instance
  ( MonadTransTrans u1, MonadTransTrans u2
  ) => MonadTransTrans (ComposeTT u1 u2)
  where
    liftT = ComposeTT . liftT . liftT

instance
  ( RunMonadTransTrans z u1 f1, RunMonadTransTrans z u2 f2
  ) => RunMonadTransTrans (Dub z) (ComposeTT u1 u2) (Compose f2 f1)
  where
    runTT
      :: (Monad m, MonadTrans t)
      => Dub z m -> ComposeTT u1 u2 t m a -> t m (Compose f2 f1 a)
    runTT (Dub z1 z2) =
      fmap Compose . runTT z2 . runTT z1 . unComposeTT

runComposeTT
  :: ( Monad m, MonadTrans t
     , RunMonadTransTrans z u1 f1, RunMonadTransTrans z u2 f2 )
  => z m -> z m -> ComposeTT u1 u2 t m a -> t m (f2 (f1 a))
runComposeTT z1 z2 = fmap unCompose . runTT (Dub z1 z2)
