{-#
  LANGUAGE
    InstanceSigs,
    KindSignatures,
    FlexibleInstances,
    MultiParamTypeClasses
#-}

module Control.FX.Monad.Trans.StateT (
    StateT(..)
  , runStateT
) where

import Data.Typeable (Typeable)

import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans.Class

newtype StateT
  (k :: * -> *)
  (s :: *)
  (m :: * -> *)
  (a :: *)
    = StateT
        { unStateT :: s -> m (Pair s a)
        } deriving (Typeable)

instance
  ( Monad m
  ) => Functor (StateT mark s m)
  where
    fmap :: (a -> b) -> StateT mark s m a -> StateT mark s m b
    fmap f (StateT x) =
      StateT $ \s1 -> do
        Pair s2 a <- x s1
        return $ Pair s2 (f a)

instance
  ( Monad m
  ) => Applicative (StateT mark s m)
  where
    pure x =
      StateT $ \s -> pure $ Pair s x

    (StateT f) <*> (StateT x) =
      StateT $ \s1 -> do
        Pair s2 g <- f s1
        Pair s3 a <- x s2
        return $ Pair s3 (g a)

instance
  ( Monad m
  ) => Monad (StateT mark s m)
  where
    return x =
      StateT $ \s -> return $ Pair s x

    (StateT x) >>= f =
      StateT $ \s1 -> do
        Pair s2 a <- x s1
        unStateT (f a) s2

instance MonadTrans (StateT mark s) where
  lift x = StateT $ \s -> fmap (\a -> Pair s a) x

instance MonadFunctor (StateT mark s) where
  hoist f (StateT x) =
    StateT $ \s -> do
      a <- f $ fmap slot2 (x s)
      return $ Pair s a

instance RunMonadTrans s (StateT mark s) (Pair s) where
  runT :: (Monad m) => s -> StateT mark s m a -> m (Pair s a)
  runT s (StateT x) = x s

runStateT :: s -> StateT mark s m a -> m (Pair s a)
runStateT s (StateT x) = x s



{- Effect Class -}

instance
  ( Monad m, MonadIdentity mark
  ) => MonadState mark s (StateT mark s m)
  where
    get :: StateT mark s m (mark s)
    get = StateT $ \s -> return (Pair s (pure s))

    put :: mark s -> StateT mark s m ()
    put s = StateT $ \_ -> return (Pair (unwrap s) ())



{- Specialized Lifts -}

instance LiftCatch s (StateT mark s) (Pair s) where
  liftCatch catch x h = StateT $ \s ->
    catch (runStateT s x) (\e -> runStateT s (h e))

instance LiftDraft s (StateT mark s) (Pair s) where
  liftDraft
    :: (Monad m)
    => Draft w m (Pair s a) -> Draft w (StateT mark s m) a
  liftDraft draft x =
    StateT $ \s -> do
      Pair s (Pair w a) <- draft $ runStateT s x
      return $ Pair w (Pair s a)

instance LiftLocal s (StateT mark s) (Pair s) where
  liftLocal
    :: (Monad m)
    => Local r m (Pair s a) -> Local r (StateT mark s m) a
  liftLocal local f x =
    StateT $ \s -> do
      local f $ runStateT s x
