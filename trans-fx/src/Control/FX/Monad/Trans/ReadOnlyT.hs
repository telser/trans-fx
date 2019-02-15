{-#
  LANGUAGE
    InstanceSigs,
    KindSignatures,
    FlexibleInstances,
    MultiParamTypeClasses
#-}

module Control.FX.Monad.Trans.ReadOnlyT (
    ReadOnlyT(..)
  , runReadOnlyT
) where

import Data.Typeable (Typeable)

import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans.Class

newtype ReadOnlyT
  (k :: * -> *)
  (r :: *)
  (m :: * -> *)
  (a :: *)
    = ReadOnlyT
        { unReadOnlyT :: ReadOnly k r (m a)
        } deriving (Typeable)

instance
  ( Functor m
  ) => Functor (ReadOnlyT mark r m)
  where
    fmap f (ReadOnlyT x) =
      ReadOnlyT $ fmap (fmap f) x

instance
  ( Applicative m
  ) => Applicative (ReadOnlyT mark r m)
  where
    pure :: a -> ReadOnlyT mark r m a
    pure x =
      ReadOnlyT $ ReadOnly $ \_ -> pure x

    (ReadOnlyT f) <*> (ReadOnlyT x) =
      ReadOnlyT $ ReadOnly $ \r ->
        (unReadOnly f r) <*> (unReadOnly x r)

instance
  ( Monad m
  ) => Monad (ReadOnlyT mark r m)
  where
    return :: a -> ReadOnlyT mark r m a
    return x =
      ReadOnlyT $ ReadOnly $ \_ -> return x

    (ReadOnlyT x) >>= f =
      ReadOnlyT $ ReadOnly $ \r ->
        (unReadOnly x r >>= (($ r) . unReadOnly . unReadOnlyT . f))

instance MonadTrans (ReadOnlyT mark r) where
  lift x = ReadOnlyT $ ReadOnly $ \_ -> x

instance MonadFunctor (ReadOnlyT mark r) where
  hoist f (ReadOnlyT x) =
    ReadOnlyT $ ReadOnly $ \r -> f (unReadOnly x r)

instance RunMonadTrans r (ReadOnlyT mark r) Identity where
  runT :: (Monad m) => r -> ReadOnlyT mark r m a -> m (Identity a)
  runT r = fmap Identity . runReadOnlyT r

runReadOnlyT :: (Functor m) => r -> ReadOnlyT mark r m a -> m a
runReadOnlyT r (ReadOnlyT x) = unReadOnly x r



{- Effect Class -}

instance
  ( Monad m, MonadIdentity mark
  ) => MonadReadOnly mark r (ReadOnlyT mark r m)
  where
    ask :: ReadOnlyT mark r m (mark r)
    ask = ReadOnlyT $ ReadOnly $ \r -> return (pure r)

    local :: (mark r -> mark r) -> ReadOnlyT mark r m a -> ReadOnlyT mark r m a
    local f (ReadOnlyT (ReadOnly x)) =
      ReadOnlyT $ ReadOnly $ x . unwrap . f . pure



{- Specialized Lifts -}

instance LiftCatch r (ReadOnlyT mark r) Identity where
  liftCatch
    :: (Monad m)
    => Catch e m (Identity a) -> Catch e (ReadOnlyT mark r m) a
  liftCatch catch x h = ReadOnlyT $ ReadOnly $ \r -> fmap unIdentity $ catch
    (fmap Identity $ runReadOnlyT r x)
    (\e -> fmap Identity $ runReadOnlyT r $ h e)

instance LiftDraft r (ReadOnlyT mark r) Identity where
  liftDraft
    :: (Monad m)
    => Draft w m (Identity a) -> Draft w (ReadOnlyT mark r m) a
  liftDraft draft =
    ReadOnlyT . fmap (fmap (fmap unIdentity) . draft . fmap Identity) . unReadOnlyT
