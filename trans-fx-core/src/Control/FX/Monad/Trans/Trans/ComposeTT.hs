-- | Module      : Control.FX.Monad.Trans.Trans.ComposeTT
--   Description : Concrete composite monad transformer transformer
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

module Control.FX.Monad.Trans.Trans.ComposeTT (
    ComposeTT(..)
  , Dub(..)
) where

import Data.Typeable (Typeable, typeOf)



import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans
import Control.FX.Monad.Trans.Trans.Class
import Control.FX.Monad.Trans.Trans.ApplyTT



-- | Concrete composite monad transformer transformer
data ComposeTT
  (u1 :: ((* -> *) -> (* -> *)) -> (* -> *) -> * -> *)
  (u2 :: ((* -> *) -> (* -> *)) -> (* -> *) -> * -> *)
  (t :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = ComposeTT
        { unComposeTT :: u1 (u2 t) m a
        } deriving (Typeable)

deriving instance
  ( Show (u1 (u2 t) m a)
  ) => Show (ComposeTT u1 u2 t m a)

instance
  ( Monad m, MonadTrans t, MonadTransTrans u1, MonadTransTrans u2
  ) => Functor (ComposeTT u1 u2 t m)
  where
    fmap
      :: (a -> b)
      -> ComposeTT u1 u2 t m a
      -> ComposeTT u1 u2 t m b
    fmap f = ComposeTT . fmap f . unComposeTT

instance
  ( Monad m, MonadTrans t, MonadTransTrans u1, MonadTransTrans u2
  ) => Applicative (ComposeTT u1 u2 t m)
  where
    pure
      :: a
      -> ComposeTT u1 u2 t m a
    pure = ComposeTT . pure

    (<*>)
      :: ComposeTT u1 u2 t m (a -> b)
      -> ComposeTT u1 u2 t m a
      -> ComposeTT u1 u2 t m b
    (ComposeTT f) <*> (ComposeTT x) =
      ComposeTT (f <*> x)

instance
  ( Monad m, MonadTrans t, MonadTransTrans u1, MonadTransTrans u2
  ) => Monad (ComposeTT u1 u2 t m)
  where
    return
      :: a
      -> ComposeTT u1 u2 t m a
    return = ComposeTT . return

    (>>=)
      :: ComposeTT u1 u2 t m a
      -> (a -> ComposeTT u1 u2 t m b)
      -> ComposeTT u1 u2 t m b
    (ComposeTT x) >>= f =
      ComposeTT (x >>= (unComposeTT . f))

instance
  ( Monad m, MonadTrans t, MonadTransTrans u1, MonadTransTrans u2
  , Semigroup a, MonadIdentity (u1 (u2 t) m)
  ) => Semigroup (ComposeTT u1 u2 t m a)
  where
    (<>)
      :: ComposeTT u1 u2 t m a
      -> ComposeTT u1 u2 t m a
      -> ComposeTT u1 u2 t m a
    (ComposeTT x) <> (ComposeTT y) =
      ComposeTT (x <> y)

instance
  ( Monad m, MonadTrans t, MonadTransTrans u1, MonadTransTrans u2
  , Monoid a, MonadIdentity (u1 (u2 t) m)
  ) => Monoid (ComposeTT u1 u2 t m a)
  where
    mempty
      :: ComposeTT u1 u2 t m a
    mempty = ComposeTT mempty

    mappend
      :: ComposeTT u1 u2 t m a
      -> ComposeTT u1 u2 t m a
      -> ComposeTT u1 u2 t m a
    mappend = (<>)

instance
  ( MonadTrans t, MonadTransTrans u1, MonadTransTrans u2
  ) => MonadTrans (ComposeTT u1 u2 t)
  where
    lift
      :: ( Monad m )
      => m a
      -> ComposeTT u1 u2 t m a
    lift = ComposeTT . lift

instance
  ( MonadTransTrans u1, MonadTransTrans u2
  ) => MonadTransTrans (ComposeTT u1 u2)
  where
    liftT
      :: ( Monad m, MonadTrans t )
      => t m a
      -> ComposeTT u1 u2 t m a
    liftT = ComposeTT . liftT . liftT

instance
  ( RunMonadTransTrans z1 u1 f1, RunMonadTransTrans z2 u2 f2
  ) => RunMonadTransTrans (Dub z1 z2) (ComposeTT u1 u2) (Compose f2 f1)
  where
    runTT
      :: ( Monad m, MonadTrans t )
      => Dub z1 z2 m
      -> ComposeTT u1 u2 t m a
      -> t m (Compose f2 f1 a)
    runTT (Dub z1 z2) =
      fmap Compose . runTT z2 . runTT z1 . unComposeTT

-- | Helper type for running @CompositeTT@
data Dub
  (z1 :: (* -> *) -> *)
  (z2 :: (* -> *) -> *)
  (m :: * -> *)
    = Dub (z1 m) (z2 m)

instance
  ( Typeable z1, Typeable z2, Typeable m
  ) => Show (Dub z1 z2 m)
  where
    show
      :: Dub z1 z2 m
      -> String
    show = show . typeOf





{- Effect Instances -}

instance
  ( Monad m, MonadTrans t, MonadTransTrans u1, MonadTransTrans u2
  , MonadIdentity (u1 (u2 t) m)
  ) => MonadIdentity (ComposeTT u1 u2 t m)
  where
    unwrap
      :: ComposeTT u1 u2 t m a
      -> a
    unwrap = unwrap . unComposeTT



instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t, MonadTransTrans u1, MonadTransTrans u2
  , MonadIdentity mark
  , forall x y. (Monad x, MonadTrans y) => MonadExcept mark e (u1 y x)
  ) => MonadExcept mark e (ComposeTT (ApplyTT u1) u2 t m)
  where
    throw
      :: mark e
      -> ComposeTT (ApplyTT u1) u2 t m a
    throw = ComposeTT . ApplyTT . throw

    catch
      :: ComposeTT (ApplyTT u1) u2 t m a
      -> (mark e -> ComposeTT (ApplyTT u1) u2 t m a)
      -> ComposeTT (ApplyTT u1) u2 t m a
    catch x h = ComposeTT $ ApplyTT $
      catch
        (unApplyTT $ unComposeTT x)
        (unApplyTT . unComposeTT . h)

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadTransTrans u1, MonadTransTrans u2
  , MonadIdentity mark, LiftCatchT z1 u1 f1
  , forall x y. (Monad x, MonadTrans y) => MonadExcept mark e (u2 y x)
  ) => MonadExcept mark e (ComposeTT u1 u2 t m)
  where
    throw
      :: mark e
      -> ComposeTT u1 u2 t m a
    throw = ComposeTT . liftT . throw

    catch
      :: ComposeTT u1 u2 t m a
      -> (mark e -> ComposeTT u1 u2 t m a)
      -> ComposeTT u1 u2 t m a
    catch x h = ComposeTT $
      liftCatchT catch (unComposeTT x) (unComposeTT . h)



instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t, MonadTransTrans u1, MonadTransTrans u2
  , MonadIdentity mark, Monoid w
  , forall x y. (Monad x, MonadTrans y) => MonadWriteOnly mark w (u1 y x)
  ) => MonadWriteOnly mark w (ComposeTT (ApplyTT u1) u2 t m)
  where
    tell
      :: mark w
      -> ComposeTT (ApplyTT u1) u2 t m ()
    tell = ComposeTT . ApplyTT . tell

    draft
      :: ComposeTT (ApplyTT u1) u2 t m a
      -> ComposeTT (ApplyTT u1) u2 t m (Pair (mark w) a)
    draft = ComposeTT . ApplyTT . draft . unApplyTT . unComposeTT

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadTransTrans u1, MonadTransTrans u2
  , MonadIdentity mark, Monoid w, LiftDraftT z1 u1 f1
  , forall x y. (Monad x, MonadTrans y) => MonadWriteOnly mark w (u2 y x)
  ) => MonadWriteOnly mark w (ComposeTT u1 u2 t m)
  where
    tell
      :: mark w
      -> ComposeTT u1 u2 t m ()
    tell = ComposeTT . liftT . tell

    draft
      :: ComposeTT u1 u2 t m a
      -> ComposeTT u1 u2 t m (Pair (mark w) a)
    draft = ComposeTT . liftDraftT draft . unComposeTT



instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t, MonadTransTrans u1, MonadTransTrans u2
  , MonadIdentity mark
  , forall x y. (Monad x, MonadTrans y) => MonadReadOnly mark r (u1 y x)
  ) => MonadReadOnly mark r (ComposeTT (ApplyTT u1) u2 t m)
  where
    ask
      :: ComposeTT (ApplyTT u1) u2 t m (mark r)
    ask = ComposeTT $ ApplyTT ask

    local
      :: (mark r -> mark r)
      -> ComposeTT (ApplyTT u1) u2 t m a
      -> ComposeTT (ApplyTT u1) u2 t m a
    local f = ComposeTT . ApplyTT . local f . unApplyTT . unComposeTT

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadTransTrans u1, MonadTransTrans u2
  , MonadIdentity mark, LiftLocalT z1 u1 f1
  , forall x y. (Monad x, MonadTrans y) => MonadReadOnly mark r (u2 y x)
  ) => MonadReadOnly mark r (ComposeTT u1 u2 t m)
  where
    ask
      :: ComposeTT u1 u2 t m (mark r)
    ask = ComposeTT $ liftT ask

    local
      :: (mark r -> mark r)
      -> ComposeTT u1 u2 t m a
      -> ComposeTT u1 u2 t m a
    local f = ComposeTT . liftLocalT local f . unComposeTT



instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t, MonadTransTrans u1, MonadTransTrans u2
  , MonadIdentity mark
  , forall x y. (Monad x, MonadTrans y) => MonadState mark s (u1 y x)
  ) => MonadState mark s (ComposeTT (ApplyTT u1) u2 t m)
  where
    get
      :: ComposeTT (ApplyTT u1) u2 t m (mark s)
    get = ComposeTT $ ApplyTT get

    put
      :: mark s
      -> ComposeTT (ApplyTT u1) u2 t m ()
    put = ComposeTT . ApplyTT . put

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadTransTrans u1, MonadTransTrans u2
  , MonadIdentity mark
  , forall x y. (Monad x, MonadTrans y) => MonadState mark s (u2 y x)
  ) => MonadState mark s (ComposeTT u1 u2 t m)
  where
    get
      :: ComposeTT u1 u2 t m (mark s)
    get = ComposeTT $ liftT get

    put
      :: mark s
      -> ComposeTT u1 u2 t m ()
    put = ComposeTT . liftT . put



instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t, MonadTransTrans u1, MonadTransTrans u2
  , forall x y. (Monad x, MonadTrans y) => MonadMaybe (u1 y x)
  ) => MonadMaybe (ComposeTT (ApplyTT u1) u2 t m)
  where
    bail
      :: ComposeTT (ApplyTT u1) u2 t m a
    bail = ComposeTT $ ApplyTT bail

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadTransTrans u1, MonadTransTrans u2
  , forall x y. (Monad x, MonadTrans y) => MonadMaybe (u2 y x)
  ) => MonadMaybe (ComposeTT u1 u2 t m)
  where
    bail
      :: ComposeTT u1 u2 t m a
    bail = ComposeTT $ liftT bail



instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t, MonadTransTrans u1, MonadTransTrans u2
  , MonadIdentity mark
  , forall x y. (Monad x, MonadTrans y) => MonadPrompt mark p (u1 y x)
  ) => MonadPrompt mark p (ComposeTT (ApplyTT u1) u2 t m)
  where
    prompt
      :: mark (p a)
      -> ComposeTT (ApplyTT u1) u2 t m (mark a)
    prompt = ComposeTT . ApplyTT . prompt

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadTransTrans u1, MonadTransTrans u2
  , MonadIdentity mark
  , forall x y. (Monad x, MonadTrans y) => MonadPrompt mark p (u2 y x)
  ) => MonadPrompt mark p (ComposeTT u1 u2 t m)
  where
    prompt
      :: mark (p a)
      -> ComposeTT u1 u2 t m (mark a)
    prompt = ComposeTT . liftT . prompt
