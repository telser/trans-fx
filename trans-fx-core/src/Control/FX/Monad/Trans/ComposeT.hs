{-#
  LANGUAGE
    InstanceSigs,
    KindSignatures,
    FlexibleInstances,
    UndecidableInstances,
    QuantifiedConstraints,
    MultiParamTypeClasses
#-}

module Control.FX.Monad.Trans.ComposeT (
    ComposeT(..)
  , runComposeT
) where

import Data.Typeable (Typeable)

import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans.Class
import Control.FX.Monad.Trans.ExceptT
import Control.FX.Monad.Trans.WriteOnlyT
import Control.FX.Monad.Trans.StateT
import Control.FX.Monad.Trans.ReadOnlyT
import Control.FX.Monad.Trans.MaybeT

newtype ComposeT
  (t1 :: (* -> *) -> * -> *)
  (t2 :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = ComposeT
        { unComposeT :: t1 (t2 m) a
        } deriving (Typeable)

instance
  ( Monad m, MonadTrans t1, MonadTrans t2
  ) => Functor (ComposeT t1 t2 m)
  where
    fmap f = ComposeT . fmap f . unComposeT
      

instance
  ( Monad m, MonadTrans t1, MonadTrans t2
  ) => Applicative (ComposeT t1 t2 m)
  where
    pure = ComposeT . pure

    (ComposeT f) <*> (ComposeT x) =
      ComposeT (f <*> x)

instance
  ( Monad m, MonadTrans t1, MonadTrans t2
  ) => Monad (ComposeT t1 t2 m)
  where
    return = ComposeT . return

    (ComposeT x) >>= f =
      ComposeT (x >>= (unComposeT . f))

instance
  ( MonadTrans t1, MonadTrans t2
  ) => MonadTrans (ComposeT t1 t2)
  where
    lift = ComposeT . lift . lift

instance
  ( MonadFunctor t1, MonadFunctor t2
  ) => MonadFunctor (ComposeT t1 t2)
  where
    hoist f = ComposeT . hoist (hoist f) . unComposeT

instance
  ( RunMonadTrans z1 t1 f1, RunMonadTrans z2 t2 f2
  ) => RunMonadTrans (z1,z2) (ComposeT t1 t2) (Compose f2 f1)
  where
    runT (z1,z2) = fmap Compose . runT z2 . runT z1 . unComposeT

runComposeT
  :: ( Monad m, RunMonadTrans z1 t1 f1, RunMonadTrans z2 t2 f2 )
  => z1 -> z2 -> ComposeT t1 t2 m a -> m (Compose f2 f1 a)
runComposeT z1 z2 = runT (z1,z2)



{- Effect Instances -}

instance
  ( Monad m, MonadTrans t1, MonadTrans t2
  , forall x. (Monad x) => MonadIdentity (t1 x)
  , forall x. (Monad x) => MonadIdentity (t2 x)
  ) => MonadIdentity (ComposeT t1 t2 m)
  where
    unwrap = unwrap . unComposeT



instance  {-# OVERLAPS #-}
  ( Monad m, MonadTrans t2, MonadIdentity mark
  ) => MonadExcept mark e (ComposeT (ExceptT mark e) t2 m)
  where
    throw = ComposeT . throw

    catch x h = ComposeT $ catch (unComposeT x) (unComposeT . h)

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t1, MonadTrans t2
  , MonadIdentity mark, LiftCatch z1 t1 f1
  , forall x. (Monad x) => MonadExcept mark e (t2 x)
  ) => MonadExcept mark e (ComposeT t1 t2 m)
  where
    throw = ComposeT . lift . throw

    catch x h = ComposeT $ liftCatch catch (unComposeT x) (unComposeT . h)



instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t2, Monoid w, MonadIdentity mark
  ) => MonadWriteOnly mark w (ComposeT (WriteOnlyT mark w) t2 m)
  where
    draft = ComposeT . draft . unComposeT

    tell = ComposeT . tell

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t1, MonadTrans t2, Monoid w
  , LiftDraft z1 t1 f1, MonadIdentity mark
  , forall x. (Monad x) => MonadWriteOnly mark w (t2 x)
  ) => MonadWriteOnly mark w (ComposeT t1 t2 m)
  where
    draft = ComposeT . liftDraft draft . unComposeT

    tell = ComposeT . lift . tell



instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t2, MonadIdentity mark
  ) => MonadState mark s (ComposeT (StateT mark s) t2 m)
  where
    get :: ComposeT (StateT mark s) t2 m (mark s)
    get = ComposeT get

    put :: mark s -> ComposeT (StateT mark s) t2 m ()
    put = ComposeT . put

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t1, MonadTrans t2, MonadIdentity mark
  , forall x. (Monad x) => MonadState mark s (t2 x)
  ) => MonadState mark s (ComposeT t1 t2 m)
  where
    get :: ComposeT t1 t2 m (mark s)
    get = ComposeT $ lift get

    put :: mark s -> ComposeT t1 t2 m ()
    put = ComposeT . lift . put



instance  {-# OVERLAPS #-}
  ( Monad m, MonadTrans t2, MonadIdentity mark
  ) => MonadReadOnly mark r (ComposeT (ReadOnlyT mark r) t2 m)
  where
    ask :: ComposeT (ReadOnlyT mark r) t2 m (mark r)
    ask = ComposeT ask

    local f = ComposeT . local f . unComposeT

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t1, MonadTrans t2
  , LiftLocal z1 t1 f1, MonadIdentity mark
  , forall x. (Monad x) => MonadReadOnly mark r (t2 x)
  ) => MonadReadOnly mark r (ComposeT t1 t2 m)
  where
    ask :: ComposeT t1 t2 m (mark r)
    ask = ComposeT $ lift ask

    local :: (mark r -> mark r) -> ComposeT t1 t2 m a -> ComposeT t1 t2 m a
    local f = ComposeT . liftLocal local f . unComposeT



instance  {-# OVERLAPS #-}
  ( Monad m, MonadTrans t2
  ) => MonadMaybe (ComposeT MaybeT t2 m)
  where
    bail :: ComposeT MaybeT t2 m r
    bail = ComposeT bail

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t1, MonadTrans t2
  , LiftLocal z1 t1 f1
  , forall x. (Monad x) => MonadMaybe (t2 x)
  ) => MonadMaybe (ComposeT t1 t2 m)
  where
    bail :: ComposeT t1 t2 m r
    bail = ComposeT $ lift bail
