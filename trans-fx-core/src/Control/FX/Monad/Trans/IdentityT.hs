-- | Module      : Control.FX.Monad.Trans.IdentityT
--   Description : Concrete identity monad transformer
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

module Control.FX.Monad.Trans.IdentityT (
    IdentityT(..)
  , Context(..)
  , InputT(..)
  , OutputT(..)
) where



import Data.Typeable (Typeable)
import Control.Applicative (liftA2)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans.Class



-- | Concrete identity monad transformer
newtype IdentityT
  (m :: * -> *)
  (a :: *)
  = IdentityT
      { unIdentityT :: m a
      } deriving (Typeable)



deriving instance
  ( Show (m a)
  ) => Show (IdentityT m a)

instance
  ( Functor m
  ) => Functor (IdentityT m)
  where
    fmap
      :: (a -> b)
      -> IdentityT m a
      -> IdentityT m b
    fmap f = IdentityT . fmap f . unIdentityT

instance
  ( Applicative m
  ) => Applicative (IdentityT m)
  where
    pure
      :: a
      -> IdentityT m a
    pure = IdentityT . pure

    (<*>)
      :: IdentityT m (a -> b)
      -> IdentityT m a
      -> IdentityT m b
    (IdentityT f) <*> (IdentityT x) =
      IdentityT (f <*> x)

instance
  ( Monad m
  ) => Monad (IdentityT m)
  where
    return
      :: a
      -> IdentityT m a
    return = IdentityT . return

    (>>=)
      :: IdentityT m a
      -> (a -> IdentityT m b)
      -> IdentityT m b
    (IdentityT x) >>= f =
      IdentityT (x >>= (unIdentityT . f))

instance
  ( Central c
  ) => Commutant (IdentityT c)
  where
    commute
      :: ( Applicative f )
      => IdentityT c (f a)
      -> f (IdentityT c a)
    commute = fmap IdentityT . commute . unIdentityT

instance
  ( Central c
  ) => Central (IdentityT c)

instance
  ( MonadIdentity m, Eq a
  ) => Eq (IdentityT m a)
  where
    (==)
      :: IdentityT m a
      -> IdentityT m a
      -> Bool
    (IdentityT x) == (IdentityT y) =
      (unwrap x) == (unwrap y)

instance
  ( MonadIdentity m, Semigroup a
  ) => Semigroup (IdentityT m a)
  where
    (<>)
      :: IdentityT m a
      -> IdentityT m a
      -> IdentityT m a
    (IdentityT a) <> (IdentityT b) =
      IdentityT (a <> b)

instance
  ( MonadIdentity m, Monoid a
  ) => Monoid (IdentityT m a)
  where
    mempty
      :: IdentityT m a
    mempty = IdentityT mempty

    mappend
      :: IdentityT m a
      -> IdentityT m a
      -> IdentityT m a
    mappend = (<>)

instance
  MonadTrans IdentityT
  where
    lift
      :: ( Monad m )
      => m a
      -> IdentityT m a
    lift = IdentityT

instance
  MonadFunctor IdentityT
  where
    hoist
      :: ( Monad m, Monad n )
      => (forall u. m u -> n u)
      -> IdentityT m a
      -> IdentityT n a
    hoist f = IdentityT . f . unIdentityT





instance
  ( EqIn m, Functor m
  ) => EqIn (IdentityT m)
  where
    data Context (IdentityT m)
      = IdentityTCtx
          { unIdentityTCtx :: Context m
          } deriving (Typeable)

    eqIn
      :: (Eq a)
      => Context (IdentityT m)
      -> IdentityT m a
      -> IdentityT m a
      -> Bool
    eqIn (IdentityTCtx h) (IdentityT x) (IdentityT y) =
      eqIn h x y

deriving instance
  ( Eq (Context m)
  ) => Eq (Context (IdentityT m))

deriving instance
  ( Show (Context m)
  ) => Show (Context (IdentityT m))



instance
  RunMonadTrans IdentityT
  where
    data instance InputT IdentityT
      = IdentityTIn
          { unIdentityTIn :: ()
          } deriving (Eq, Show, Typeable)

    data instance OutputT IdentityT a
      = IdentityTOut
          { unIdentityTOut :: Identity a
          } deriving (Eq, Show, Typeable)

    runT
      :: ( Monad m )
      => InputT IdentityT
      -> IdentityT m a
      -> m (OutputT IdentityT a)
    runT _ (IdentityT x) =
      fmap (IdentityTOut . Identity) x





{- Specialized Lifts -}

instance
  LiftCatch IdentityT
  where
    liftCatch
      :: ( Monad m )
      => Catch e m (OutputT IdentityT a)
      -> Catch e (IdentityT m) a
    liftCatch catch m h =
      IdentityT $ fmap (unIdentity . unIdentityTOut) $ catch
        (unIdentityT $ fmap (IdentityTOut . Identity) m)
        (unIdentityT . fmap (IdentityTOut . Identity) . h)

instance
  LiftDraft IdentityT
  where
    liftDraft
      :: ( Monad m )
      => Draft w m (OutputT IdentityT a)
      -> Draft w (IdentityT m) a
    liftDraft draft =
      IdentityT . fmap (fmap (unIdentity . unIdentityTOut))
        . draft . fmap (IdentityTOut . Identity) . unIdentityT

instance
  LiftLocal IdentityT
  where
    liftLocal
      :: ( Monad m )
      => Local r m (OutputT IdentityT a)
      -> Local r (IdentityT m) a
    liftLocal local f =
      IdentityT . fmap (unIdentity . unIdentityTOut)
        . local f . fmap (IdentityTOut . Identity) . unIdentityT





{- Effect Classes -}

instance
  ( MonadIdentity m, Central m
  ) => MonadIdentity (IdentityT m)
  where
    unwrap
      :: IdentityT m a
      -> a
    unwrap = unwrap . unIdentityT

instance
  ( MonadHalt mark m
  ) => MonadHalt mark (IdentityT m)

instance
  ( MonadReadOnly mark r m
  ) => MonadReadOnly mark r (IdentityT m)

instance
  ( MonadWriteOnly mark w m, Monoid w
  ) => MonadWriteOnly mark w (IdentityT m)

instance
  ( MonadState mark s m
  ) => MonadState mark s (IdentityT m)

instance
  ( MonadPrompt mark p m
  ) => MonadPrompt mark p (IdentityT m)

instance
  ( MonadExcept mark e m
  ) => MonadExcept mark e (IdentityT m)

instance
  ( MonadAppendOnly mark w m
  ) => MonadAppendOnly mark w (IdentityT m)
