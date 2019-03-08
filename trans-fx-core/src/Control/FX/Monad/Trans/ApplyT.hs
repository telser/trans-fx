-- | Module      : Control.FX.Monad.Trans.ApplyT
--   Description : Concrete application monad transformer
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

module Control.FX.Monad.Trans.ApplyT (
    ApplyT(..)
  , InputT(..)
  , OutputT(..)
) where



import Data.Typeable (Typeable)

import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans.Class
import Control.FX.Monad.Trans.IdentityT



-- | Concrete application monad transformer
newtype ApplyT
  (t :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = ApplyT
        { unApplyT :: t m a
        } deriving (Typeable)

deriving instance
  ( Show (t m a)
  ) => Show (ApplyT t m a)

instance
  ( Monad m, MonadTrans t
  ) => Functor (ApplyT t m)
  where
    fmap
      :: (a -> b)
      -> ApplyT t m a
      -> ApplyT t m b
    fmap f = ApplyT . fmap f . unApplyT

instance
  ( Monad m, MonadTrans t
  ) => Applicative (ApplyT t m)
  where
    pure
      :: a
      -> ApplyT t m a
    pure = ApplyT . pure

    (<*>)
      :: ApplyT t m (a -> b)
      -> ApplyT t m a
      -> ApplyT t m b
    (ApplyT f) <*> (ApplyT x) =
      ApplyT (f <*> x)

instance
  ( Monad m, MonadTrans t
  ) => Monad (ApplyT t m)
  where
    return
      :: a
      -> ApplyT t m a
    return = ApplyT . return

    (>>=)
      :: ApplyT t m a
      -> (a -> ApplyT t m b)
      -> ApplyT t m b
    (ApplyT x) >>= f =
      ApplyT (x >>= (unApplyT . f))

instance
  ( Central c, MonadTrans t
  , forall m. (Central m) => Central (t m)
  ) => Commutant (ApplyT t c)
  where
    commute
      :: ( Applicative f )
      => ApplyT t c (f a)
      -> f (ApplyT t c a)
    commute = fmap ApplyT . commute . unApplyT

instance
  ( Central c, MonadTrans t
  , forall m. (Central m) => Central (t m)
  ) => Central (ApplyT t c)

instance
  ( Monad m, MonadTrans t, MonadIdentity (t m), Eq a
  ) => Eq (ApplyT t m a)
  where
    (==)
      :: ApplyT t m a
      -> ApplyT t m a
      -> Bool
    (ApplyT x) == (ApplyT y) =
      (unwrap x) == (unwrap y)

instance
  ( Monad m, MonadTrans t, MonadIdentity (t m), Semigroup a
  ) => Semigroup (ApplyT t m a)
  where
    (<>)
      :: ApplyT t m a
      -> ApplyT t m a
      -> ApplyT t m a
    (ApplyT a) <> (ApplyT b) =
      ApplyT (a <> b)

instance
  ( Monad m, MonadTrans t, MonadIdentity (t m), Monoid a
  ) => Monoid (ApplyT t m a)
  where
    mempty
      :: ApplyT t m a
    mempty = ApplyT mempty

    mappend
      :: ApplyT t m a
      -> ApplyT t m a
      -> ApplyT t m a
    mappend = (<>)

instance
  ( MonadTrans t
  ) => MonadTrans (ApplyT t)
  where
    lift
      :: ( Monad m )
      => m a
      -> ApplyT t m a
    lift = ApplyT . lift

instance
  ( MonadFunctor t
  ) => MonadFunctor (ApplyT t)
  where
    hoist
      :: ( Monad m, Monad n )
      => (forall u. m u -> n u)
      -> ApplyT t m a
      -> ApplyT t n a
    hoist f = ApplyT . hoist f . unApplyT





instance
  ( RunMonadTrans t
  ) => RunMonadTrans (ApplyT t)
  where
    newtype InputT (ApplyT t)
      = ApplyTIn
          { unApplyTIn :: InputT t
          } deriving (Typeable)

    newtype OutputT (ApplyT t) a
      = ApplyTOut
          { unApplyTOut :: OutputT t a
          } deriving (Typeable)

    runT
      :: ( Monad m )
      => InputT (ApplyT t)
      -> ApplyT t m a
      -> m (OutputT (ApplyT t) a)
    runT (ApplyTIn z) (ApplyT x) = fmap ApplyTOut $ runT z x

deriving instance
  ( Eq (InputT t)
  ) => Eq (InputT (ApplyT t))

deriving instance
  ( Show (InputT t)
  ) => Show (InputT (ApplyT t))

deriving instance
  ( Eq (OutputT t a)
  ) => Eq (OutputT (ApplyT t) a)

deriving instance
  ( Show (OutputT t a)
  ) => Show (OutputT (ApplyT t) a)





{- Effect Classes -}

instance
  ( Monad m, MonadTrans t
  , forall x. (Monad x) => MonadIdentity (t x)
  ) => MonadIdentity (ApplyT t m)
  where
    unwrap
      :: ApplyT t m a
      -> a
    unwrap = unwrap . ApplyT

instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , forall x. (Monad x) => MonadExcept mark e (t x)
  ) => MonadExcept mark e (ApplyT t m)
  where
    throw
      :: mark e
      -> ApplyT t m a
    throw = ApplyT . throw

    catch
      :: ApplyT t m a
      -> (mark e -> ApplyT t m a)
      -> ApplyT t m a
    catch x h = ApplyT $ catch (unApplyT x) (unApplyT . h)

instance
  ( Monad m, Monoid w, MonadTrans t, MonadIdentity mark
  , forall x. (Monad x) => MonadWriteOnly mark w (t x)
  ) => MonadWriteOnly mark w (ApplyT t m)
  where
    draft
      :: ApplyT t m a
      -> ApplyT t m (Pair (mark w) a)
    draft = ApplyT . draft . unApplyT

    tell
      :: mark w
      -> ApplyT t m ()
    tell = ApplyT . tell

instance
  ( Monad m, Monoid w, MonadTrans t, MonadIdentity mark
  , forall x. (Monad x) => MonadAppendOnly mark w (t x)
  ) => MonadAppendOnly mark w (ApplyT t m)
  where
    look
      :: ApplyT t m (mark w)
    look = ApplyT look

    jot
      :: mark w
      -> ApplyT t m ()
    jot = ApplyT . jot

instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , forall x. (Monad x) => MonadState mark s (t x)
  ) => MonadState mark s (ApplyT t m)
  where
    get
      :: ApplyT t m (mark s)
    get = ApplyT get

    put
      :: mark s
      -> ApplyT t m ()
    put = ApplyT . put

instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , forall x. (Monad x) => MonadReadOnly mark r (t x)
  ) => MonadReadOnly mark r (ApplyT t m)
  where
    ask
      :: ApplyT t m (mark r)
    ask = ApplyT ask

    local
      :: (mark r -> mark r)
      -> ApplyT t m a
      -> ApplyT t m a
    local f = ApplyT . local f . unApplyT

instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , forall x. (Monad x) => MonadHalt mark (t x)
  ) => MonadHalt mark (ApplyT t m)
  where
    halt
      :: mark ()
      -> ApplyT t m a
    halt = ApplyT . halt
