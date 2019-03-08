-- | Module      : Control.FX.Monad.Trans.Trans.ApplyTT
--   Description : Concrete application monad transformer transformer
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.Trans.Trans.ApplyTT (
    ApplyTT(..)
  , InputTT(..)
  , OutputTT(..)
) where



import Data.Typeable (Typeable)

import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans
import Control.FX.Monad.Trans.Trans.Class



-- | Concrete application monad transformer transformer
data ApplyTT
  (u :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *)
  (t :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = ApplyTT
        { unApplyTT :: u t m a
        } deriving (Typeable)

deriving instance
  ( Show (u t m a)
  ) => Show (ApplyTT u t m a)

instance
  ( Monad m, MonadTrans t, MonadTransTrans u
  ) => Functor (ApplyTT u t m)
  where
    fmap
      :: (a -> b)
      -> ApplyTT u t m a
      -> ApplyTT u t m b
    fmap f = ApplyTT . fmap f . unApplyTT

instance
  ( Monad m, MonadTrans t, MonadTransTrans u
  ) => Applicative (ApplyTT u t m)
  where
    pure
      :: a
      -> ApplyTT u t m a
    pure = ApplyTT . pure

    (<*>)
      :: ApplyTT u t m (a -> b)
      -> ApplyTT u t m a
      -> ApplyTT u t m b
    (ApplyTT f) <*> (ApplyTT x) =
      ApplyTT (f <*> x)

instance
  ( Monad m, MonadTrans t, MonadTransTrans u
  ) => Monad (ApplyTT u t m)
  where
    return
      :: a
      -> ApplyTT u t m a
    return = ApplyTT . return

    (>>=)
      :: ApplyTT u t m a
      -> (a -> ApplyTT u t m b)
      -> ApplyTT u t m b
    (ApplyTT x) >>= f =
      ApplyTT (x >>= (unApplyTT . f))

instance
  ( MonadTrans t, MonadTransTrans u
  ) => MonadTrans (ApplyTT u t)
  where
    lift
      :: ( Monad m )
      => m a
      -> ApplyTT u t m a
    lift = ApplyTT . lift

instance
  ( MonadFunctor t, MonadTransFunctor u
  ) => MonadFunctor (ApplyTT u t)
  where
    hoist
      :: ( Monad m, Monad n )
      => (forall u. m u -> n u)
      -> ApplyTT u t m a
      -> ApplyTT u t n a
    hoist f = ApplyTT . raiseT f . unApplyTT

instance
  ( MonadTransTrans u
  ) => MonadTransTrans (ApplyTT u)
  where
    liftT
      :: ( Monad m, MonadTrans t )
      => t m a
      -> ApplyTT u t m a
    liftT = ApplyTT . liftT

instance
  ( MonadTransFunctor u
  ) => MonadTransFunctor (ApplyTT u)
  where
    hoistT
      :: ( Monad m, MonadFunctor t1, MonadFunctor t2 )
      => (forall n x. (Monad n) => t1 n x -> t2 n x)
      -> ApplyTT u t1 m a
      -> ApplyTT u t2 m a
    hoistT f = ApplyTT . hoistT f . unApplyTT

    raiseT
      :: ( Monad m1, Monad m2, MonadFunctor t )
      => (forall x. m1 x -> m2 x)
      -> ApplyTT u t m1 x
      -> ApplyTT u t m2 x
    raiseT f = ApplyTT . raiseT f . unApplyTT





instance
  ( MonadTransTrans u, RunMonadTransTrans u
  ) => RunMonadTransTrans (ApplyTT u)
  where
    newtype InputTT (ApplyTT u) m
      = ApplyTTIn
          { unApplyTTIn :: InputTT u m
          } deriving (Typeable)

    newtype OutputTT (ApplyTT u) a
      = ApplyTTOut
          { unApplyTTOut :: OutputTT u a
          } deriving (Typeable)

    runTT
      :: (Monad m, MonadTrans t)
      => InputTT (ApplyTT u) m
      -> ApplyTT u t m a
      -> t m (OutputTT (ApplyTT u) a)
    runTT (ApplyTTIn z) = fmap ApplyTTOut . runTT z . unApplyTT
