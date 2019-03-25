-- | Module      : Control.FX.Monad.Trans.Trans.IdentityTT
--   Description : Concrete identity monad transformer transformer
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.Trans.Trans.IdentityTT (
    IdentityTT(..)
  , Context(..)
  , InputTT(..)
  , OutputTT(..)
) where



import Data.Typeable (Typeable, typeOf)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans
import Control.FX.Monad.Trans.Trans.Class



-- | Concrete identity monad transformer transformer
data IdentityTT
  (t :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = IdentityTT
        { unIdentityTT :: t m a
        } deriving (Eq, Show, Typeable)

instance
  ( Monad m, MonadTrans t
  ) => Functor (IdentityTT t m)
  where
    fmap
      :: (a -> b)
      -> IdentityTT t m a
      -> IdentityTT t m b
    fmap f = IdentityTT . fmap f . unIdentityTT

instance
  ( Monad m, MonadTrans t
  ) => Applicative (IdentityTT t m)
  where
    pure
      :: a
      -> IdentityTT t m a
    pure = IdentityTT . pure

    (<*>)
      :: IdentityTT t m (a -> b)
      -> IdentityTT t m a
      -> IdentityTT t m b
    (IdentityTT f) <*> (IdentityTT x) =
      IdentityTT (f <*> x)

instance
  ( Monad m, MonadTrans t
  ) => Monad (IdentityTT t m)
  where
    return
      :: a
      -> IdentityTT t m a
    return = IdentityTT . return

    (>>=)
      :: IdentityTT t m a
      -> (a -> IdentityTT t m b)
      -> IdentityTT t m b
    (IdentityTT x) >>= f =
      IdentityTT (x >>= (unIdentityTT . f))

instance
  ( Monad m, Semigroup a, MonadIdentity (t m)
  ) => Semigroup (IdentityTT t m a)
  where
    (<>)
      :: IdentityTT t m a
      -> IdentityTT t m a
      -> IdentityTT t m a
    (IdentityTT x) <> (IdentityTT y) =
      IdentityTT (x <> y)

instance
  ( Monad m, Monoid a, MonadIdentity (t m)
  ) => Monoid (IdentityTT t m a)
  where
    mempty
      :: IdentityTT t m a
    mempty = IdentityTT mempty

instance
  ( MonadTrans t
  ) => MonadTrans (IdentityTT t)
  where
    lift
      :: ( Monad m )
      => m a
      -> IdentityTT t m a
    lift = IdentityTT . lift

instance
  ( MonadFunctor t
  ) => MonadFunctor (IdentityTT t)
  where
    hoist
      :: ( Monad m, Monad n )
      => (forall u. m u -> n u)
      -> IdentityTT t m a
      -> IdentityTT t n a
    hoist f = IdentityTT . hoist f . unIdentityTT

instance
  MonadTransTrans IdentityTT
  where
    liftT
      :: ( Monad m, MonadTrans t )
      => t m a
      -> IdentityTT t m a
    liftT = IdentityTT





instance
  ( EqIn (t m)
  ) => EqIn (IdentityTT t m)
  where
    newtype Context (IdentityTT t m)
      = IdentityTTCtx
          { unIdentityTTCtx :: Context (t m)
          } deriving (Typeable)

    eqIn
      :: (Eq a)
      => Context (IdentityTT t m)
      -> IdentityTT t m a
      -> IdentityTT t m a
      -> Bool
    eqIn (IdentityTTCtx h) (IdentityTT x) (IdentityTT y) =
      eqIn h x y

deriving instance
  ( Eq (Context (t m))
  ) => Eq (Context (IdentityTT t m))

deriving instance
  ( Show (Context (t m))
  ) => Show (Context (IdentityTT t m))



instance
  RunMonadTransTrans IdentityTT
  where
    newtype InputTT IdentityTT m
      = IdentityTTIn
          { unIdentityTTIn :: ()
          } deriving (Eq, Show, Typeable)

    newtype OutputTT IdentityTT a
      = IdentityTTOut
          { unIdentityTTOut :: Identity a
          } deriving (Eq, Show, Typeable)

    runTT
      :: (Monad m, MonadTrans t)
      => InputTT IdentityTT m
      -> IdentityTT t m a
      -> t m (OutputTT IdentityTT a)
    runTT _ (IdentityTT x) = fmap (IdentityTTOut . Identity) x





{- Specialized Lifts -}

instance
  LiftCatchT IdentityTT
  where
    liftCatchT
      :: ( Monad m, MonadTrans t )
      => (forall x. Catch e (t m) (OutputTT IdentityTT x))
      -> (forall x. Catch e (IdentityTT t m) x)
    liftCatchT catch x h =
      IdentityTT $ fmap (unIdentity . unIdentityTTOut) $ catch
        (fmap (IdentityTTOut . Identity) $ unIdentityTT x)
        (fmap (IdentityTTOut . Identity) . unIdentityTT . h)

instance
  LiftDraftT IdentityTT
  where
    liftDraftT
      :: ( Monad m, MonadTrans t, Monoid w )
      => (forall x. Draft w (t m) (OutputTT IdentityTT x))
      -> (forall x. Draft w (IdentityTT t m) x)
    liftDraftT draft x = IdentityTT $
      fmap (bimap2 (unIdentity . unIdentityTTOut)) $ draft
        (fmap (IdentityTTOut . Identity) $ unIdentityTT x)

instance
  LiftLocalT IdentityTT
  where
    liftLocalT
      :: ( Monad m, MonadTrans t )
      => (forall x. Local r (t m) (OutputTT IdentityTT x))
      -> (forall x. Local r (IdentityTT t m) x)
    liftLocalT local f =
      IdentityTT . fmap (unIdentity . unIdentityTTOut) . local f . fmap (IdentityTTOut . Identity) . unIdentityTT





{- Effect Classes -}

instance
  ( Monad m, MonadTrans t, MonadIdentity (t m)
  ) => MonadIdentity (IdentityTT t m)
  where
    unwrap
      :: IdentityTT t m a
      -> a
    unwrap = unwrap . unIdentityTT

instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadState mark s (t m)
  ) => MonadState mark s (IdentityTT t m)
  where
    get
      :: IdentityTT t m (mark s)
    get = IdentityTT get

    put
      :: mark s
      -> IdentityTT t m ()
    put = IdentityTT . put

instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadReadOnly mark r (t m)
  ) => MonadReadOnly mark r (IdentityTT t m)
  where
    ask
      :: IdentityTT t m (mark r)
    ask = IdentityTT ask

    local
      :: (mark r -> mark r)
      -> IdentityTT t m a
      -> IdentityTT t m a
    local f = IdentityTT . local f . unIdentityTT

instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadWriteOnly mark w (t m), Monoid w
  ) => MonadWriteOnly mark w (IdentityTT t m)
  where
    tell
      :: mark w
      -> IdentityTT t m ()
    tell = IdentityTT . tell

    draft
      :: IdentityTT t m a
      -> IdentityTT t m (Pair (mark w) a)
    draft = IdentityTT . draft . unIdentityTT

instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadAppendOnly mark w (t m), Monoid w
  ) => MonadAppendOnly mark w (IdentityTT t m)
  where
    jot
      :: mark w
      -> IdentityTT t m ()
    jot = IdentityTT . jot

    look
      :: IdentityTT t m (mark w)
    look = IdentityTT look

instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadWriteOnce mark w (t m)
  ) => MonadWriteOnce mark w (IdentityTT t m)
  where
    etch
      :: mark w
      -> IdentityTT t m Bool
    etch = IdentityTT . etch

    press
      :: IdentityTT t m (Maybe (mark w))
    press = IdentityTT press

instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadExcept mark e (t m)
  ) => MonadExcept mark e (IdentityTT t m)
  where
    throw
      :: mark e
      -> IdentityTT t m a
    throw = IdentityTT . throw

    catch
      :: IdentityTT t m a
      -> (mark e -> IdentityTT t m a)
      -> IdentityTT t m a
    catch x h = IdentityTT $ catch
      (unIdentityTT x) (unIdentityTT . h)

instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadHalt mark (t m)
  ) => MonadHalt mark (IdentityTT t m)
  where
    halt
      :: mark ()
      -> IdentityTT t m a
    halt = IdentityTT . halt

instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadPrompt mark p (t m)
  ) => MonadPrompt mark p (IdentityTT t m)
  where
    prompt
      :: mark (p a)
      -> IdentityTT t m (mark a)
    prompt = IdentityTT . prompt
