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

import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans
import Control.FX.Monad.Trans.Trans.Class



-- | Concrete monad transformer transformer which applies a monad functor
data OverTT
  (u :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *)
  (w :: (* -> *) -> * -> *)
  (t :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = OverTT
        { unOverTT :: w (u t m) a
        } deriving (Typeable)

deriving instance
  ( Show (w (u t m) a)
  ) => Show (OverTT u w t m a)

instance
  ( Monad m, MonadTrans t, MonadFunctor w, MonadTransTrans u
  ) => Functor (OverTT u w t m)
  where
    fmap
      :: (a -> b)
      -> OverTT u w t m a
      -> OverTT u w t m b
    fmap f = OverTT . fmap f . unOverTT

instance
  ( Monad m, MonadTrans t, MonadFunctor w, MonadTransTrans u
  ) => Applicative (OverTT u w t m)
  where
    pure
      :: a
      -> OverTT u w t m a
    pure = OverTT . pure

    (<*>)
      :: OverTT u w t m (a -> b)
      -> OverTT u w t m a
      -> OverTT u w t m b
    (OverTT f) <*> (OverTT x) =
      OverTT (f <*> x)

instance
  ( Monad m, MonadTrans t, MonadFunctor w, MonadTransTrans u
  ) => Monad (OverTT u w t m)
  where
    return
      :: a
      -> OverTT u w t m a
    return = OverTT . return

    (>>=)
      :: OverTT u w t m a
      -> (a -> OverTT u w t m b)
      -> OverTT u w t m b
    (OverTT x) >>= f =
      OverTT (x >>= (unOverTT . f))

instance
  ( MonadTrans t, MonadFunctor w, MonadTransTrans u
  ) => MonadTrans (OverTT u w t)
  where
    lift
      :: ( Monad m )
      => m a
      -> OverTT u w t m a
    lift = OverTT . lift . lift

instance
  ( MonadFunctor t, MonadFunctor w, MonadTransFunctor u
  ) => MonadFunctor (OverTT u w t)
  where
    hoist
      :: ( Monad m, Monad n )
      => (forall x. m x -> n x)
      -> OverTT u w t m a
      -> OverTT u w t n a
    hoist f = OverTT . hoist (hoist f) . unOverTT

instance
  ( MonadFunctor w, MonadTransTrans u
  ) => MonadTransTrans (OverTT u w)
  where
    liftT
      :: ( Monad m, MonadTrans t )
      => t m a
      -> OverTT u w t m a
    liftT = OverTT . lift . liftT

instance
  ( RunMonadTransTrans z1 u f1, RunMonadTrans z2 w f2, MonadFunctor w
  ) => RunMonadTransTrans (Sing z1 z2) (OverTT u w) (Compose f1 f2)
  where
    runTT
      :: ( Monad m, MonadTrans t )
      => Sing z1 z2 m
      -> OverTT u w t m a
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



{- Effect Instances -}

instance
  ( Monad m, MonadTrans t, MonadFunctor w
  , MonadTransTrans u, MonadIdentity mark
  , forall x. (Monad x) => MonadExcept mark e (w x)
  ) => MonadExcept mark e (OverTT u w t m)
  where
    throw
      :: mark e
      -> OverTT u w t m a
    throw = OverTT . throw

    catch
      :: OverTT u w t m a
      -> (mark e -> OverTT u w t m a)
      -> OverTT u w t m a
    catch x h = OverTT $ catch (unOverTT x) (unOverTT . h)

instance
  ( Monad m, MonadTrans t, MonadFunctor w, Monoid w'
  , MonadTransTrans u, MonadIdentity mark
  , forall x. (Monad x) => MonadWriteOnly mark w' (w x)
  ) => MonadWriteOnly mark w' (OverTT u w t m)
  where
    tell
      :: mark w'
      -> OverTT u w t m ()
    tell = OverTT . tell

    draft
      :: OverTT u w t m a
      -> OverTT u w t m (Pair (mark w') a)
    draft = OverTT . draft . unOverTT

instance
  ( Monad m, MonadTrans t, MonadFunctor w
  , MonadTransTrans u
  , forall x. (Monad x) => MonadMaybe (w x)
  ) => MonadMaybe (OverTT u w t m)
  where
    bail
      :: OverTT u w t m a
    bail = OverTT bail

instance
  ( Monad m, MonadTrans t, MonadFunctor w
  , MonadTransTrans u, MonadIdentity mark
  , forall x. (Monad x) => MonadReadOnly mark r (w x)
  ) => MonadReadOnly mark r (OverTT u w t m)
  where
    ask
      :: OverTT u w t m (mark r)
    ask = OverTT ask

    local
      :: (mark r -> mark r)
      -> OverTT u w t m a
      -> OverTT u w t m a
    local f = OverTT . local f . unOverTT

instance
  ( Monad m, MonadTrans t, MonadFunctor w
  , MonadTransTrans u, MonadIdentity mark
  , forall x. (Monad x) => MonadState mark s (w x)
  ) => MonadState mark s (OverTT u w t m)
  where
    get
      :: OverTT u w t m (mark s)
    get = OverTT get

    put
      :: mark s
      -> OverTT u w t m ()
    put = OverTT . put

instance
  ( Monad m, MonadTrans t, MonadFunctor w
  , MonadTransTrans u, MonadPrompt mark p (u t m)
  ) => MonadPrompt mark p (OverTT u w t m)
  where
    prompt
      :: mark (p a)
      -> OverTT u w t m (mark a)
    prompt = OverTT . lift . prompt
