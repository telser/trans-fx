-- | Module      : Control.FX.Monad.Trans.MaybeT
--   Description : Concrete maybe monad transformer
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
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.Trans.MaybeT (
    MaybeT(..)
  , runMaybeT
) where



import Data.Typeable (Typeable)

import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans.Class



-- | Concrete @Maybe@ monad transformer
newtype MaybeT
  (m :: * -> *)
  (a :: *)
    = MaybeT
        { unMaybeT :: m (Maybe a)
        } deriving (Typeable)

deriving instance
  ( Show (m (Maybe a))
  ) => Show (MaybeT m a)

instance
  ( Monad m
  ) => Functor (MaybeT m)
  where
    fmap
      :: (a -> b)
      -> MaybeT m a
      -> MaybeT m b
    fmap f =
      MaybeT . fmap (fmap f) . unMaybeT

instance
  ( Monad m
  ) => Applicative (MaybeT m)
  where
    pure
      :: a
      -> MaybeT m a
    pure = MaybeT . return . Just

    (<*>)
      :: MaybeT m (a -> b)
      -> MaybeT m a
      -> MaybeT m b
    (MaybeT mf) <*> (MaybeT mx) =
      MaybeT $ do
        f' <- mf
        case f' of
          Nothing -> return Nothing
          Just f -> do
            x' <- mx
            case x' of
              Nothing -> return Nothing
              Just x  -> return (Just (f x))

instance
  ( Monad m
  ) => Monad (MaybeT m)
  where
    return
      :: a
      -> MaybeT m a
    return = MaybeT . return . Just

    (>>=)
      :: MaybeT m a
      -> (a -> MaybeT m b)
      -> MaybeT m b
    (MaybeT x) >>= f =
      MaybeT $ do
        a' <- x
        case a' of
          Nothing -> return Nothing
          Just a  -> unMaybeT $ f a

instance
  ( Central c
  ) => Commutant (MaybeT c)
  where
    commute
      :: ( Applicative f )
      => MaybeT c (f a)
      -> f (MaybeT c a)
    commute = fmap MaybeT . commute . fmap commute . unMaybeT

instance
  ( Central c
  ) => Central (MaybeT c)

instance MonadTrans MaybeT where
  lift
    :: ( Monad m )
    => m a
    -> MaybeT m a
  lift x = MaybeT (x >>= (return . Just))

instance
  MonadFunctor MaybeT
  where
    hoist
      :: ( Monad m, Monad n )
      => ( forall u. m u -> n u )
      -> MaybeT m a
      -> MaybeT n a
    hoist f = MaybeT . f . unMaybeT

instance
  RunMonadTrans () MaybeT Maybe
  where
    runT
      :: ( Monad m )
      => ()
      -> MaybeT m a
      -> m (Maybe a)
    runT () (MaybeT x) = x

-- | Run a @MaybeT@ computation
runMaybeT
  :: ( Monad m )
  => MaybeT m a
  -> m (Maybe a)
runMaybeT = runT ()



{- Effect Class -}

instance
  ( Monad m
  ) => MonadMaybe (MaybeT m)
  where
    bail
      :: MaybeT m a
    bail = MaybeT $ return Nothing



{- Specialized Lifts -}

instance
  LiftCatch () MaybeT Maybe
  where
    liftCatch catch x h = MaybeT $
      catch (runMaybeT x) (runMaybeT . h)

instance
  LiftDraft () MaybeT Maybe
  where
    liftDraft
      :: ( Monad m )
      => Draft w m (Maybe a)
      -> Draft w (MaybeT m) a
    liftDraft draft =
      MaybeT . fmap commute . draft . unMaybeT

instance
  LiftLocal () MaybeT Maybe
  where
    liftLocal
      :: ( Monad m )
      => Local r m (Maybe a)
      -> Local r (MaybeT m) a
    liftLocal local f =
      MaybeT . local f . unMaybeT
