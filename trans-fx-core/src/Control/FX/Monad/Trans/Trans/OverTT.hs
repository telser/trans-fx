-- | Module      : Control.FX.Monad.Trans.Trans.OverTT
--   Description : Concrete monad functor application monad transformer transformer
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
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.Trans.Trans.OverTT (
    OverTT(..)
  , Sing(..)
  , runOverTT
) where



import Data.Typeable (Typeable, typeOf)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans
import Control.FX.Monad.Trans.Trans.Class
import Control.FX.Monad.Trans.Trans.ApplyTT



-- | Concrete monad transformer transformer which applies a monad functor
data OverTT
  (u :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *)
  (v :: (* -> *) -> * -> *)
  (t :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = OverTT
        { unOverTT :: v (u t m) a
        } deriving (Typeable)

deriving instance
  ( Show (v (u t m) a)
  ) => Show (OverTT u v t m a)

instance
  ( Monad m, MonadTrans t, MonadFunctor v, MonadTransTrans u
  ) => Functor (OverTT u v t m)
  where
    fmap
      :: (a -> b)
      -> OverTT u v t m a
      -> OverTT u v t m b
    fmap f = OverTT . fmap f . unOverTT

instance
  ( Monad m, MonadTrans t, MonadFunctor v, MonadTransTrans u
  ) => Applicative (OverTT u v t m)
  where
    pure
      :: a
      -> OverTT u v t m a
    pure = OverTT . pure

    (<*>)
      :: OverTT u v t m (a -> b)
      -> OverTT u v t m a
      -> OverTT u v t m b
    (OverTT f) <*> (OverTT x) =
      OverTT (f <*> x)

instance
  ( Monad m, MonadTrans t, MonadFunctor v, MonadTransTrans u
  ) => Monad (OverTT u v t m)
  where
    return
      :: a
      -> OverTT u v t m a
    return = OverTT . return

    (>>=)
      :: OverTT u v t m a
      -> (a -> OverTT u v t m b)
      -> OverTT u v t m b
    (OverTT x) >>= f =
      OverTT (x >>= (unOverTT . f))

instance
  ( MonadIdentity (v (u t m)), Eq a
  ) => Eq (OverTT u v t m a)
  where
    (==)
      :: OverTT u v t m a
      -> OverTT u v t m a
      -> Bool
    (OverTT x) == (OverTT y) =
      (unwrap x) == (unwrap y)

instance
  ( MonadIdentity (v (u t m)), Semigroup a
  ) => Semigroup (OverTT u v t m a)
  where
    (<>)
      :: OverTT u v t m a
      -> OverTT u v t m a
      -> OverTT u v t m a
    (OverTT x) <> (OverTT y) =
      OverTT $ pure $ (unwrap x) <> (unwrap y)

instance
  ( MonadIdentity (v (u t m)), Monoid a
  ) => Monoid (OverTT u v t m a)
  where
    mempty
      :: OverTT u v t m a
    mempty = OverTT $ pure mempty

instance
  ( MonadTrans t, MonadFunctor v, MonadTransTrans u
  ) => MonadTrans (OverTT u v t)
  where
    lift
      :: ( Monad m )
      => m a
      -> OverTT u v t m a
    lift = OverTT . lift . lift

instance
  ( MonadFunctor t, MonadFunctor v, MonadTransFunctor u
  ) => MonadFunctor (OverTT u v t)
  where
    hoist
      :: ( Monad m, Monad n )
      => (forall x. m x -> n x)
      -> OverTT u v t m a
      -> OverTT u v t n a
    hoist f = OverTT . hoist (hoist f) . unOverTT

instance
  ( MonadFunctor v, MonadTransTrans u
  ) => MonadTransTrans (OverTT u v)
  where
    liftT
      :: ( Monad m, MonadTrans t )
      => t m a
      -> OverTT u v t m a
    liftT = OverTT . lift . liftT

instance
  ( RunMonadTransTrans z1 u f1, RunMonadTrans z2 v f2, MonadFunctor v
  ) => RunMonadTransTrans (Sing z1 z2) (OverTT u v) (Compose f1 f2)
  where
    runTT
      :: ( Monad m, MonadTrans t )
      => Sing z1 z2 m
      -> OverTT u v t m a
      -> t m (Compose f1 f2 a)
    runTT (Sing z1 z2) =
      fmap Compose . runTT z1 . runT z2 . unOverTT

runOverTT
  :: ( RunMonadTransTrans z1 u f1, RunMonadTrans z2 v f2
     , Monad m, MonadTrans t, MonadFunctor v )
  => z1 m
  -> z2
  -> OverTT u v t m a
  -> t m (Compose f1 f2 a)
runOverTT z1 z2 = runTT (Sing z1 z2)

-- | Helper type for running @OverTT@
data Sing
  (z :: (* -> *) -> *)
  (y :: *)
  (m :: * -> *)
    = Sing (z m) y

instance
  ( Typeable z, Typeable y, Typeable m
  ) => Show (Sing z y m)
  where
    show
      :: Sing z y m
      -> String
    show = show . typeOf

instance
  ( Monad m, MonadTrans t, MonadFunctor v, MonadTransTrans u
  , Eq a, RunMonadTransTrans z1 u f1, RunMonadTrans z2 v f2
  , forall x. (Eq x) => Eq (f1 (f2 x))
  , forall x. (Eq x) => EqIn h (t m x)
  ) => EqIn (Sing z1 z2 m, h) (OverTT u v t m a)
  where
    eqIn
      :: (Sing z1 z2 m, h)
      -> OverTT u v t m a
      -> OverTT u v t m a
      -> Bool
    eqIn (k,h) x y =
      eqIn h (runTT k x) (runTT k y)





{- Effect Instances -}

instance
  ( Monad m, MonadTrans t, MonadFunctor v
  , MonadTransTrans u, MonadIdentity (v (u t m))
  ) => MonadIdentity (OverTT u v t m)
  where
    unwrap
      :: OverTT u v t m a
      -> a
    unwrap = unwrap . unOverTT

instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t, MonadFunctor v
  , MonadTransTrans u, MonadIdentity mark, LiftCatch z v f
  , forall x y. (Monad x, MonadTrans y) => MonadExcept mark e (u y x)
  ) => MonadExcept mark e (OverTT (ApplyTT u) v t m)
  where
    throw
      :: mark e
      -> OverTT (ApplyTT u) v t m a
    throw = OverTT . hoist ApplyTT . lift . throw

    catch
      :: OverTT (ApplyTT u) v t m a
      -> (mark e -> OverTT (ApplyTT u) v t m a)
      -> OverTT (ApplyTT u) v t m a
    catch x h = OverTT $ hoist ApplyTT $
      liftCatch catch
        (hoist unApplyTT $ unOverTT x)
        (hoist unApplyTT . unOverTT . h)

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadFunctor v
  , MonadTransTrans u, MonadIdentity mark
  , forall x. (Monad x) => MonadExcept mark e (v x)
  ) => MonadExcept mark e (OverTT u v t m)
  where
    throw
      :: mark e
      -> OverTT u v t m a
    throw = OverTT . throw

    catch
      :: OverTT u v t m a
      -> (mark e -> OverTT u v t m a)
      -> OverTT u v t m a
    catch x h = OverTT $ catch (unOverTT x) (unOverTT . h)



instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t, MonadFunctor v, Monoid w
  , MonadTransTrans u, MonadIdentity mark, LiftDraft z v f
  , forall x y. (Monad x, MonadTrans y) => MonadWriteOnly mark w (u y x)
  ) => MonadWriteOnly mark w (OverTT (ApplyTT u) v t m)
  where
    tell
      :: mark w
      -> OverTT (ApplyTT u) v t m ()
    tell = OverTT . lift . ApplyTT . tell

    draft
      :: OverTT (ApplyTT u) v t m a
      -> OverTT (ApplyTT u) v t m (Pair (mark w) a)
    draft x = OverTT $ hoist ApplyTT $
      liftDraft draft (hoist unApplyTT $ unOverTT x)

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadFunctor v, Monoid w
  , MonadTransTrans u, MonadIdentity mark
  , forall x. (Monad x) => MonadWriteOnly mark w (v x)
  ) => MonadWriteOnly mark w (OverTT u v t m)
  where
    tell
      :: mark w
      -> OverTT u v t m ()
    tell = OverTT . tell

    draft
      :: OverTT u v t m a
      -> OverTT u v t m (Pair (mark w) a)
    draft = OverTT . draft . unOverTT



instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t, MonadFunctor v, Monoid w
  , MonadTransTrans u, MonadIdentity mark, LiftDraft z v f
  , forall x y. (Monad x, MonadTrans y) => MonadAppendOnly mark w (u y x)
  ) => MonadAppendOnly mark w (OverTT (ApplyTT u) v t m)
  where
    jot
      :: mark w
      -> OverTT (ApplyTT u) v t m ()
    jot = OverTT . lift . ApplyTT . jot

    look
      :: OverTT (ApplyTT u) v t m (mark w)
    look = OverTT $ hoist ApplyTT $ lift look



instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadFunctor v, Monoid w
  , MonadTransTrans u, MonadIdentity mark
  , forall x. (Monad x) => MonadAppendOnly mark w (v x)
  ) => MonadAppendOnly mark w (OverTT u v t m)
  where
    jot
      :: mark w
      -> OverTT u v t m ()
    jot = OverTT . jot

    look
      :: OverTT u v t m (mark w)
    look = OverTT look





instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t, MonadFunctor v, MonadTransTrans u, MonadIdentity mark
  , forall x y. (Monad x, MonadTrans y) => MonadHalt mark (u y x)
  ) => MonadHalt mark (OverTT (ApplyTT u) v t m)
  where
    halt
      :: mark ()
      -> OverTT (ApplyTT u) v t m a
    halt = OverTT . hoist ApplyTT . lift . halt

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadFunctor v, MonadTransTrans u, MonadIdentity mark
  , forall x. (Monad x) => MonadHalt mark (v x)
  ) => MonadHalt mark (OverTT u v t m)
  where
    halt
      :: mark ()
      -> OverTT u v t m a
    halt = OverTT . halt



instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t, MonadFunctor v
  , MonadTransTrans u, MonadIdentity mark, LiftLocal z v f
  , forall x y. (Monad x, MonadTrans y) => MonadReadOnly mark r (u y x)
  ) => MonadReadOnly mark r (OverTT (ApplyTT u) v t m)
  where
    ask
      :: OverTT (ApplyTT u) v t m (mark r)
    ask = OverTT $ hoist ApplyTT $ lift ask

    local
      :: (mark r -> mark r)
      -> OverTT (ApplyTT u) v t m a
      -> OverTT (ApplyTT u) v t m a
    local f =
      OverTT . hoist ApplyTT . liftLocal local f . hoist unApplyTT . unOverTT

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadFunctor v
  , MonadTransTrans u, MonadIdentity mark
  , forall x. (Monad x) => MonadReadOnly mark r (v x)
  ) => MonadReadOnly mark r (OverTT u v t m)
  where
    ask
      :: OverTT u v t m (mark r)
    ask = OverTT ask

    local
      :: (mark r -> mark r)
      -> OverTT u v t m a
      -> OverTT u v t m a
    local f = OverTT . local f . unOverTT



instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t, MonadFunctor v
  , MonadTransTrans u, MonadIdentity mark
  , forall x y. (Monad x, MonadTrans y) => MonadState mark s (u y x)
  ) => MonadState mark s (OverTT (ApplyTT u) v t m)
  where
    get
      :: OverTT (ApplyTT u) v t m (mark s)
    get = OverTT $ hoist ApplyTT $ lift get

    put
      :: mark s
      -> OverTT (ApplyTT u) v t m ()
    put = OverTT . lift . ApplyTT . put

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadFunctor v
  , MonadTransTrans u, MonadIdentity mark
  , forall x. (Monad x) => MonadState mark s (v x)
  ) => MonadState mark s (OverTT u v t m)
  where
    get
      :: OverTT u v t m (mark s)
    get = OverTT get

    put
      :: mark s
      -> OverTT u v t m ()
    put = OverTT . put



instance
  ( Monad m, MonadTrans t, MonadFunctor v
  , MonadTransTrans u, MonadPrompt mark p (u t m)
  ) => MonadPrompt mark p (OverTT u v t m)
  where
    prompt
      :: mark (p a)
      -> OverTT u v t m (mark a)
    prompt = OverTT . lift . prompt
