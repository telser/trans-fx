{-#
  LANGUAGE
    InstanceSigs,
    KindSignatures,
    FlexibleInstances,
    MultiParamTypeClasses
#-}

module Control.FX.Monad.Trans.MaybeT (
    MaybeT(..)
  , runMaybeT
) where

import Data.Typeable (Typeable)

import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans.Class

newtype MaybeT
  (m :: * -> *)
  (a :: *)
    = MaybeT
        { unMaybeT :: m (Maybe a)
        } deriving (Typeable)

instance
  ( Monad m
  ) => Functor (MaybeT m)
  where
    fmap f =
      MaybeT . fmap (fmap f) . unMaybeT

instance
  ( Monad m
  ) => Applicative (MaybeT m)
  where
    pure = MaybeT . return . Just

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
    return = MaybeT . return . Just

    (MaybeT x) >>= f =
      MaybeT $ do
        a' <- x
        case a' of
          Nothing -> return Nothing
          Just a  -> unMaybeT $ f a

instance
  ( Monad m, Central m
  ) => Central (MaybeT m)
  where
    commute :: (Applicative f) => MaybeT m (f a) -> f (MaybeT m a)
    commute = fmap MaybeT . commute . fmap commute . unMaybeT

instance MonadTrans MaybeT where
  lift x = MaybeT (x >>= (return . Just))

instance MonadFunctor MaybeT where
  hoist f = MaybeT . f . unMaybeT

instance RunMonadTrans () MaybeT Maybe where
  runT :: (Monad m) => () -> MaybeT m a -> m (Maybe a)
  runT () (MaybeT x) = x

runMaybeT :: (Monad m) => MaybeT m a -> m (Maybe a)
runMaybeT = runT ()



{- Effect Class -}

instance
  ( Monad m
  ) => MonadMaybe (MaybeT m)
  where
    bail :: MaybeT m a
    bail = MaybeT $ return Nothing



{- Specialized Lifts -}

instance LiftCatch () MaybeT Maybe where
  liftCatch catch x h = MaybeT $
    catch (runMaybeT x) (runMaybeT . h)

instance LiftDraft () MaybeT Maybe where
  liftDraft
    :: (Monad m)
    => Draft w m (Maybe a) -> Draft w (MaybeT m) a
  liftDraft draft =
    MaybeT . fmap commute . draft . unMaybeT

instance LiftLocal () MaybeT Maybe where
  liftLocal
    :: (Monad m)
    => Local r m (Maybe a) -> Local r (MaybeT m) a
  liftLocal local f =
    MaybeT . local f . unMaybeT
