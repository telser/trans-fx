-- | Module      : Control.FX.Monad.Trans.ComposeT
--   Description : Concrete composite monad transformer
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

module Control.FX.Monad.Trans.ComposeT (
    ComposeT(..)
) where



import Data.Typeable (Typeable)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans.Class
import Control.FX.Monad.Trans.ExceptT
import Control.FX.Monad.Trans.WriteOnlyT
import Control.FX.Monad.Trans.StateT
import Control.FX.Monad.Trans.ReadOnlyT
import Control.FX.Monad.Trans.MaybeT



-- | Concrete composite monad transformer
newtype ComposeT
  (t1 :: (* -> *) -> * -> *)
  (t2 :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = ComposeT
        { unComposeT :: t1 (t2 m) a
        } deriving (Typeable)

instance
  ( EqIn (h1,(h2,h3)) (t1 (t2 m) a)
  ) => EqIn ((h1,h2),h3) (ComposeT t1 t2 m a)
  where
    eqIn
      :: ((h1,h2),h3)
      -> ComposeT t1 t2 m a
      -> ComposeT t1 t2 m a
      -> Bool
    eqIn ((h1,h2),h3) (ComposeT x) (ComposeT y) =
      eqIn (h1,(h2,h3)) x y

deriving instance
  ( Show (t1 (t2 m) a)
  ) => Show (ComposeT t1 t2 m a)

instance
  ( Monad m, MonadTrans t1, MonadTrans t2
  ) => Functor (ComposeT t1 t2 m)
  where
    fmap
      :: (a -> b)
      -> ComposeT t1 t2 m a
      -> ComposeT t1 t2 m b
    fmap f = ComposeT . fmap f . unComposeT
      

instance
  ( Monad m, MonadTrans t1, MonadTrans t2
  ) => Applicative (ComposeT t1 t2 m)
  where
    pure
      :: a
      -> ComposeT t1 t2 m a
    pure = ComposeT . pure

    (<*>)
      :: ComposeT t1 t2 m (a -> b)
      -> ComposeT t1 t2 m a
      -> ComposeT t1 t2 m b
    (ComposeT f) <*> (ComposeT x) =
      ComposeT (f <*> x)

instance
  ( Monad m, MonadTrans t1, MonadTrans t2
  ) => Monad (ComposeT t1 t2 m)
  where
    return
      :: a
      -> ComposeT t1 t2 m a
    return = ComposeT . return

    (>>=)
      :: ComposeT t1 t2 m a
      -> (a -> ComposeT t1 t2 m b)
      -> ComposeT t1 t2 m b
    (ComposeT x) >>= f =
      ComposeT (x >>= (unComposeT . f))

instance
  ( MonadTrans t1, MonadTrans t2
  ) => MonadTrans (ComposeT t1 t2)
  where
    lift
      :: ( Monad m )
      => m a
      -> ComposeT t1 t2 m a
    lift = ComposeT . lift . lift

instance
  ( MonadFunctor t1, MonadFunctor t2
  ) => MonadFunctor (ComposeT t1 t2)
  where
    hoist
      :: ( Monad m, Monad n )
      => (forall u. m u -> n u)
      -> ComposeT t1 t2 m a
      -> ComposeT t1 t2 n a
    hoist f = ComposeT . hoist (hoist f) . unComposeT

instance
  ( RunMonadTrans z1 t1 f1, RunMonadTrans z2 t2 f2
  ) => RunMonadTrans (z1,z2) (ComposeT t1 t2) (Compose f2 f1)
  where
    runT
      :: ( Monad m )
      => (z1,z2)
      -> ComposeT t1 t2 m a
      -> m (Compose f2 f1 a)
    runT (z1,z2) = fmap Compose . runT z2 . runT z1 . unComposeT



{- Effect Classes -}

instance
  ( Monad m, MonadTrans t1, MonadTrans t2
  , forall x. (Monad x) => MonadIdentity (t1 x)
  , forall x. (Monad x) => MonadIdentity (t2 x)
  ) => MonadIdentity (ComposeT t1 t2 m)
  where
    unwrap
      :: ComposeT t1 t2 m a
      -> a
    unwrap = unwrap . unComposeT



instance  {-# OVERLAPS #-}
  ( Monad m, MonadTrans t2, MonadIdentity mark
  ) => MonadExcept mark e (ComposeT (ExceptT mark e) t2 m)
  where
    throw
      :: mark e
      -> ComposeT (ExceptT mark e) t2 m a
    throw = ComposeT . throw

    catch
      :: ComposeT (ExceptT mark e) t2 m a
      -> (mark e -> ComposeT (ExceptT mark e) t2 m a)
      -> ComposeT (ExceptT mark e) t2 m a
    catch x h = ComposeT $ catch (unComposeT x) (unComposeT . h)

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t1, MonadTrans t2
  , MonadIdentity mark, LiftCatch z1 t1 f1
  , forall x. (Monad x) => MonadExcept mark e (t2 x)
  ) => MonadExcept mark e (ComposeT t1 t2 m)
  where
    throw
      :: mark e
      -> ComposeT t1 t2 m a
    throw = ComposeT . lift . throw

    catch
      :: ComposeT t1 t2 m a
      -> (mark e -> ComposeT t1 t2 m a)
      -> ComposeT t1 t2 m a
    catch x h = ComposeT $ liftCatch catch (unComposeT x) (unComposeT . h)



instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t2, Monoid w, MonadIdentity mark
  ) => MonadWriteOnly mark w (ComposeT (WriteOnlyT mark w) t2 m)
  where
    draft
      :: ComposeT (WriteOnlyT mark w) t2 m a
      -> ComposeT (WriteOnlyT mark w) t2 m (Pair (mark w) a)
    draft = ComposeT . draft . unComposeT

    tell
      :: mark w
      -> ComposeT (WriteOnlyT mark w) t2 m ()
    tell = ComposeT . tell

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t1, MonadTrans t2, Monoid w
  , LiftDraft z1 t1 f1, MonadIdentity mark
  , forall x. (Monad x) => MonadWriteOnly mark w (t2 x)
  ) => MonadWriteOnly mark w (ComposeT t1 t2 m)
  where
    draft
      :: ComposeT t1 t2 m a
      -> ComposeT t1 t2 m (Pair (mark w) a)
    draft = ComposeT . liftDraft draft . unComposeT

    tell
      :: mark w
      -> ComposeT t1 t2 m ()
    tell = ComposeT . lift . tell



instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t2, MonadIdentity mark
  ) => MonadState mark s (ComposeT (StateT mark s) t2 m)
  where
    get
      :: ComposeT (StateT mark s) t2 m (mark s)
    get = ComposeT get

    put
      :: mark s -> ComposeT (StateT mark s) t2 m ()
    put = ComposeT . put

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t1, MonadTrans t2, MonadIdentity mark
  , forall x. (Monad x) => MonadState mark s (t2 x)
  ) => MonadState mark s (ComposeT t1 t2 m)
  where
    get
      :: ComposeT t1 t2 m (mark s)
    get = ComposeT $ lift get

    put
      :: mark s -> ComposeT t1 t2 m ()
    put = ComposeT . lift . put



instance  {-# OVERLAPS #-}
  ( Monad m, MonadTrans t2, MonadIdentity mark
  ) => MonadReadOnly mark r (ComposeT (ReadOnlyT mark r) t2 m)
  where
    ask
      :: ComposeT (ReadOnlyT mark r) t2 m (mark r)
    ask = ComposeT ask

    local
      :: (mark r -> mark r)
      -> ComposeT (ReadOnlyT mark r) t2 m a
      -> ComposeT (ReadOnlyT mark r) t2 m a
    local f = ComposeT . local f . unComposeT

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t1, MonadTrans t2
  , LiftLocal z1 t1 f1, MonadIdentity mark
  , forall x. (Monad x) => MonadReadOnly mark r (t2 x)
  ) => MonadReadOnly mark r (ComposeT t1 t2 m)
  where
    ask
      :: ComposeT t1 t2 m (mark r)
    ask = ComposeT $ lift ask

    local
      :: (mark r -> mark r)
      -> ComposeT t1 t2 m a
      -> ComposeT t1 t2 m a
    local f = ComposeT . liftLocal local f . unComposeT



instance  {-# OVERLAPS #-}
  ( Monad m, MonadTrans t2
  ) => MonadMaybe (ComposeT MaybeT t2 m)
  where
    bail
      :: ComposeT MaybeT t2 m r
    bail = ComposeT bail

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t1, MonadTrans t2
  , LiftLocal z1 t1 f1
  , forall x. (Monad x) => MonadMaybe (t2 x)
  ) => MonadMaybe (ComposeT t1 t2 m)
  where
    bail
      :: ComposeT t1 t2 m r
    bail = ComposeT $ lift bail
