-- | Module      : Control.FX.Monad.Trans.IdentityT
--   Description : Concrete identity monad transformer
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.Trans.IdentityT (
    IdentityT(..)
  , runIdentityT
) where



import Data.Typeable (Typeable)
import Control.Applicative (liftA2)

import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans.Class



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
  MonadTrans IdentityT
  where
    lift
      :: m a
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
  RunMonadTrans () IdentityT Identity
  where
    runT
      :: ( Monad m )
      => ()
      -> IdentityT m a
      -> m (Identity a)
    runT () (IdentityT x) = fmap Identity x

runIdentityT
  :: ( Monad m )
  => IdentityT m a
  -> m a
runIdentityT (IdentityT x) = x



{- Effect Class -}

instance
  ( MonadIdentity m
  ) => MonadIdentity (IdentityT m)
  where
    unwrap
      :: IdentityT m a
      -> a
    unwrap = unwrap . unIdentityT



{- Specialized Lifts -}

instance LiftCatch () IdentityT Identity where
  liftCatch
    :: ( Monad m )
    => Catch e m (Identity a)
    -> Catch e (IdentityT m) a
  liftCatch catch m h = IdentityT $ fmap unIdentity $ catch
    (unIdentityT $ fmap Identity m)
    (unIdentityT . fmap Identity . h)

instance LiftDraft () IdentityT Identity where
  liftDraft
    :: ( Monad m )
    => Draft w m (Identity a)
    -> Draft w (IdentityT m) a
  liftDraft draft =
    IdentityT . fmap (fmap unIdentity) . draft . (fmap Identity . unIdentityT)

instance LiftLocal () IdentityT Identity where
  liftLocal
    :: ( Monad m )
    => Local r m (Identity a)
    -> Local r (IdentityT m) a
  liftLocal local f =
    IdentityT . fmap unIdentity . local f . fmap Identity . unIdentityT
