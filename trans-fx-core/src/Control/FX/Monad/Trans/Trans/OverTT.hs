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
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.Trans.Trans.OverTT (
    OverTT(..)
  , Sing(..)
) where



import Data.Typeable (Typeable, typeOf)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans
import Control.FX.Monad.Trans.Trans.Class



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

instance
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

instance
  ( Monad m, MonadTrans t, MonadFunctor v
  , MonadTransTrans u
  , forall x. (Monad x) => MonadMaybe (v x)
  ) => MonadMaybe (OverTT u v t m)
  where
    bail
      :: OverTT u v t m a
    bail = OverTT bail

instance
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

instance
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
