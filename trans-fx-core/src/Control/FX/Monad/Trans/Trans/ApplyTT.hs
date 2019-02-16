{-#
  LANGUAGE
    InstanceSigs,
    KindSignatures,
    FlexibleInstances,
    UndecidableInstances,
    QuantifiedConstraints,
    MultiParamTypeClasses
#-}

module Control.FX.Monad.Trans.Trans.ApplyTT (
    ApplyTT(..)
  , runApplyTT
) where

import Data.Typeable (Typeable)

import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans
import Control.FX.Monad.Trans.Trans.Class

data ApplyTT
  (u :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *)
  (t :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = ApplyTT
        { unApplyTT :: u t m a
        } deriving (Typeable)

instance
  ( Monad m, MonadTrans t, MonadTransTrans u
  ) => Functor (ApplyTT u t m)
  where
    fmap :: (a -> b) -> ApplyTT u t m a -> ApplyTT u t m b
    fmap f = ApplyTT . fmap f . unApplyTT

instance
  ( Monad m, MonadTrans t, MonadTransTrans u
  ) => Applicative (ApplyTT u t m)
  where
    pure :: a -> ApplyTT u t m a
    pure = ApplyTT . pure

    (ApplyTT f) <*> (ApplyTT x) =
      ApplyTT (f <*> x)

instance
  ( Monad m, MonadTrans t, MonadTransTrans u
  ) => Monad (ApplyTT u t m)
  where
    return = ApplyTT . return

    (ApplyTT x) >>= f =
      ApplyTT (x >>= (unApplyTT . f))

instance
  ( MonadTrans t, MonadTransTrans u
  ) => MonadTrans (ApplyTT u t)
  where
    lift = ApplyTT . lift

instance
  ( MonadFunctor t, MonadTransFunctor u
  ) => MonadFunctor (ApplyTT u t)
  where
    hoist f = ApplyTT . raiseT f . unApplyTT

instance
  ( MonadTransTrans u
  ) => MonadTransTrans (ApplyTT u)
  where
    liftT = ApplyTT . liftT

instance
  ( MonadTransFunctor u
  ) => MonadTransFunctor (ApplyTT u)
  where
    hoistT f = ApplyTT . hoistT f . unApplyTT

    raiseT f = ApplyTT . raiseT f . unApplyTT

instance
  ( MonadTransTrans u, RunMonadTransTrans z u f
  ) => RunMonadTransTrans z (ApplyTT u) f
  where
    runTT
      :: (Monad m, MonadTrans t)
      => z m -> ApplyTT u t m a -> t m (f a)
    runTT z = runTT z . unApplyTT

runApplyTT :: ApplyTT u t m a -> u t m a
runApplyTT = unApplyTT
