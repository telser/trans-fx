{-#
  LANGUAGE
    Rank2Types,
    InstanceSigs,
    KindSignatures,
    FlexibleInstances,
    UndecidableInstances,
    QuantifiedConstraints,
    MultiParamTypeClasses
#-}

module Control.FX.Monad.Trans.Trans.OverTT (
    OverTT(..)
  , runOverTT
) where

import Data.Typeable (Typeable)

import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans
import Control.FX.Monad.Trans.Trans.Class

data OverTT
  (u :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *)
  (w :: (* -> *) -> * -> *)
  (t :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = OverTT
        { unOverTT :: w (u t m) a
        } deriving (Typeable)



instance
  ( Monad m, MonadTrans t, MonadFunctor w, MonadTransTrans u
  ) => Functor (OverTT u w t m)
  where
    fmap f = OverTT . fmap f . unOverTT

instance
  ( Monad m, MonadTrans t, MonadFunctor w, MonadTransTrans u
  ) => Applicative (OverTT u w t m)
  where
    pure = OverTT . pure

    (OverTT f) <*> (OverTT x) =
      OverTT (f <*> x)

instance
  ( Monad m, MonadTrans t, MonadFunctor w, MonadTransTrans u
  ) => Monad (OverTT u w t m)
  where
    return = OverTT . return

    (OverTT x) >>= f =
      OverTT (x >>= (unOverTT . f))

instance
  ( MonadTrans t, MonadFunctor w, MonadTransTrans u
  ) => MonadTrans (OverTT u w t)
  where
    lift = OverTT . lift . lift

instance
  ( MonadFunctor t, MonadFunctor w, MonadTransFunctor u
  ) => MonadFunctor (OverTT u w t)
  where
    hoist f = OverTT . hoist (hoist f) . unOverTT

instance
  ( MonadFunctor w, MonadTransTrans u
  ) => MonadTransTrans (OverTT u w)
  where
    liftT = OverTT . lift . liftT

instance
  ( RunMonadTransTrans z1 u f1, RunMonadTrans z2 w f2, MonadFunctor w
  ) => RunMonadTransTrans (Sing z1 z2) (OverTT u w) (Compose f1 f2)
  where
    runTT (Sing z1 z2) =
      fmap Compose . runTT z1 . runT z2 . unOverTT

runOverTT
  :: ( Monad m, MonadTrans t, MonadFunctor w
     , RunMonadTransTrans z1 u f1, RunMonadTrans z2 w f2 )
  => z1 m -> z2 -> OverTT u w t m a -> t m (f1 (f2 a))
runOverTT z1 z2 = fmap unCompose . runTT (Sing z1 z2)



{- Effect Instances -}

instance
  ( Monad m, MonadTrans t, MonadFunctor w
  , MonadTransTrans u, MonadIdentity mark
  , forall x. (Monad x) => MonadExcept mark e (w x)
  ) => MonadExcept mark e (OverTT u w t m)
  where
    throw = OverTT . throw

    catch x h = OverTT $ catch (unOverTT x) (unOverTT . h)

instance
  ( Monad m, MonadTrans t, MonadFunctor w, Monoid w'
  , MonadTransTrans u, MonadIdentity mark
  , forall x. (Monad x) => MonadWriteOnly mark w' (w x)
  ) => MonadWriteOnly mark w' (OverTT u w t m)
  where
    tell = OverTT . tell

    draft = OverTT . draft . unOverTT

instance
  ( Monad m, MonadTrans t, MonadFunctor w
  , MonadTransTrans u
  , forall x. (Monad x) => MonadMaybe (w x)
  ) => MonadMaybe (OverTT u w t m)
  where
    bail = OverTT bail

instance
  ( Monad m, MonadTrans t, MonadFunctor w
  , MonadTransTrans u, MonadIdentity mark
  , forall x. (Monad x) => MonadReadOnly mark r (w x)
  ) => MonadReadOnly mark r (OverTT u w t m)
  where
    ask = OverTT ask

    local f = OverTT . local f . unOverTT

instance
  ( Monad m, MonadTrans t, MonadFunctor w
  , MonadTransTrans u, MonadIdentity mark
  , forall x. (Monad x) => MonadState mark s (w x)
  ) => MonadState mark s (OverTT u w t m)
  where
    get = OverTT get

    put = OverTT . put
