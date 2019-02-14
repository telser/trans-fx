{-#
  LANGUAGE
    InstanceSigs,
    KindSignatures,
    FlexibleInstances,
    MultiParamTypeClasses
#-}

module Control.FX.Monad.Trans.ReaderT (
    ReaderT(..)
  , runReaderT
) where

import Data.Typeable (Typeable)

import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans.Class

newtype ReaderT
  (k :: * -> *)
  (r :: *)
  (m :: * -> *)
  (a :: *)
    = ReaderT
        { unReaderT :: Reader k r (m a)
        } deriving (Typeable)

instance
  ( Functor m
  ) => Functor (ReaderT mark r m)
  where
    fmap f (ReaderT x) =
      ReaderT $ fmap (fmap f) x

instance
  ( Applicative m
  ) => Applicative (ReaderT mark r m)
  where
    pure :: a -> ReaderT mark r m a
    pure x =
      ReaderT $ Reader $ \_ -> pure x

    (ReaderT f) <*> (ReaderT x) =
      ReaderT $ Reader $ \r ->
        (unReader f r) <*> (unReader x r)

instance
  ( Monad m
  ) => Monad (ReaderT mark r m)
  where
    return :: a -> ReaderT mark r m a
    return x =
      ReaderT $ Reader $ \_ -> return x

    (ReaderT x) >>= f =
      ReaderT $ Reader $ \r ->
        (unReader x r >>= (($ r) . unReader . unReaderT . f))

instance MonadTrans (ReaderT mark r) where
  lift x = ReaderT $ Reader $ \_ -> x

instance MonadFunctor (ReaderT mark r) where
  hoist f (ReaderT x) =
    ReaderT $ Reader $ \r -> f (unReader x r)

instance RunMonadTrans r (ReaderT mark r) Identity where
  runT :: (Monad m) => r -> ReaderT mark r m a -> m (Identity a)
  runT r = fmap Identity . runReaderT r

runReaderT :: (Functor m) => r -> ReaderT mark r m a -> m a
runReaderT r (ReaderT x) = unReader x r



{- Effect Class -}

instance
  ( Monad m, MonadIdentity mark
  ) => MonadReader mark r (ReaderT mark r m)
  where
    ask :: ReaderT mark r m (mark r)
    ask = ReaderT $ Reader $ \r -> return (pure r)

    local :: (mark r -> mark r) -> ReaderT mark r m a -> ReaderT mark r m a
    local f (ReaderT (Reader x)) =
      ReaderT $ Reader $ x . unwrap . f . pure



{- Specialized Lifts -}

instance LiftCatch r (ReaderT mark r) Identity where
  liftCatch
    :: (Monad m)
    => Catch e m (Identity a) -> Catch e (ReaderT mark r m) a
  liftCatch catch x h = ReaderT $ Reader $ \r -> fmap unIdentity $ catch
    (fmap Identity $ runReaderT r x)
    (\e -> fmap Identity $ runReaderT r $ h e)

instance LiftDraft r (ReaderT mark r) Identity where
  liftDraft
    :: (Monad m)
    => Draft w m (Identity a) -> Draft w (ReaderT mark r m) a
  liftDraft draft =
    ReaderT . fmap (fmap (fmap unIdentity) . draft . fmap Identity) . unReaderT
