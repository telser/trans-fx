{-#
  LANGUAGE
    InstanceSigs,
    KindSignatures,
    FlexibleInstances,
    UndecidableInstances,
    QuantifiedConstraints,
    MultiParamTypeClasses
#-}

module Control.FX.Monad.Trans.ApplyT (
    ApplyT(..)
  , runApplyT
) where

import Data.Typeable (Typeable)

import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans.Class
import Control.FX.Monad.Trans.IdentityT

newtype ApplyT
  (t :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = ApplyT
        { unApplyT :: t m a
        } deriving (Typeable)

instance
  ( Show a
  , forall u. (Show u) => Show (m u)
  , forall u. (Show u, Show (m u)) => Show (t m u)
  ) => Show (ApplyT t m a)
  where
    show (ApplyT x) =
      "ApplyT " ++ show x

instance
  ( Monad m, MonadTrans t
  ) => Functor (ApplyT t m)
  where
    fmap f = ApplyT . fmap f . unApplyT

instance
  ( Monad m, MonadTrans t
  ) => Applicative (ApplyT t m)
  where
    pure = ApplyT . pure

    (ApplyT f) <*> (ApplyT x) =
      ApplyT (f <*> x)

instance
  ( Monad m, MonadTrans t
  ) => Monad (ApplyT t m)
  where
    return = ApplyT . return

    (ApplyT x) >>= f =
      ApplyT (x >>= (unApplyT . f))

instance
  ( Monad c, Central c, MonadTrans t
  , forall m. (Monad m, Central m) => Central (t m)
  ) => Central (ApplyT t c)
  where
    commute = fmap ApplyT . commute . unApplyT

instance
  ( MonadTrans t
  ) => MonadTrans (ApplyT t)
  where
    lift = ApplyT . lift

instance
  ( MonadFunctor t
  ) => MonadFunctor (ApplyT t)
  where
    hoist f = ApplyT . hoist f . unApplyT

instance
  ( RunMonadTrans z t f
  ) => RunMonadTrans z (ApplyT t) f
  where
    runT z (ApplyT x) = runT z x

runApplyT
  :: ( Monad m, MonadTrans t, RunMonadTrans z t f )
  => z -> ApplyT t m a -> m (f a)
runApplyT = runT



{- Effect Classes -}

instance
  ( Monad m, MonadTrans t
  , forall x. (Monad x) => MonadIdentity (t x)
  ) => MonadIdentity (ApplyT t m)
  where
    unwrap = unwrap . ApplyT

instance
  ( Monad m, MonadTrans t, MonadIdentity wrap
  , forall x. (Monad x) => MonadExcept wrap e (t x)
  ) => MonadExcept wrap e (ApplyT t m)
  where
    throw = ApplyT . throw

    catch x h = ApplyT $ catch (unApplyT x) (unApplyT . h)

instance
  ( Monad m, Monoid w, MonadTrans t, MonadIdentity mark
  , forall x. (Monad x) => MonadWriter mark w (t x)
  ) => MonadWriter mark w (ApplyT t m)
  where
    draft = ApplyT . draft . unApplyT

    tell :: mark w -> ApplyT t m ()
    tell = ApplyT . tell

instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , forall x. (Monad x) => MonadState mark s (t x)
  ) => MonadState mark s (ApplyT t m)
  where
    get :: ApplyT t m (mark s)
    get = ApplyT get

    put :: mark s -> ApplyT t m ()
    put = ApplyT . put

instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , forall x. (Monad x) => MonadReader mark r (t x)
  ) => MonadReader mark r (ApplyT t m)
  where
    ask = ApplyT ask

    local f = ApplyT . local f . unApplyT

instance
  ( Monad m, MonadTrans t
  , forall x. (Monad x) => MonadMaybe (t x)
  ) => MonadMaybe (ApplyT t m)
  where
    bail :: ApplyT t m a
    bail = ApplyT bail
