-- | Module      : Control.FX.Monad.Trans.AppendOnlyT
--   Description : Concrete mutable state monad transformer
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.Trans.AppendOnlyT (
    AppendOnlyT(..)
) where



import Data.Typeable (Typeable, typeOf)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans.Class



-- | Concrete @State@ monad transformer
newtype AppendOnlyT
  (mark :: * -> *)
  (w :: *)
  (m :: * -> *)
  (a :: *)
    = AppendOnlyT
        { unAppendOnlyT :: w -> m (Pair w a)
        } deriving (Typeable)

instance
  ( EqIn h (m (Pair w a)), MonadIdentity mark
  ) => EqIn (mark w, h) (AppendOnlyT mark w m a)
  where
    eqIn
      :: (mark w, h)
      -> AppendOnlyT mark w m a
      -> AppendOnlyT mark w m a
      -> Bool
    eqIn (w,h) (AppendOnlyT x) (AppendOnlyT y) =
      eqIn h (x $ unwrap w) (y $ unwrap w)

instance
  ( Typeable w, Typeable m, Typeable a, Typeable mark
  ) => Show (AppendOnlyT mark w m a)
  where
    show
      :: AppendOnlyT mark w m a
      -> String
    show = show . typeOf

instance
  ( Monad m, MonadIdentity mark, Monoid w
  ) => Functor (AppendOnlyT mark w m)
  where
    fmap
      :: (a -> b)
      -> AppendOnlyT mark w m a
      -> AppendOnlyT mark w m b
    fmap f (AppendOnlyT x) =
      AppendOnlyT $ \s1 -> do
        Pair s2 a <- x s1
        return $ Pair s2 (f a)

instance
  ( Monad m, MonadIdentity mark, Monoid w
  ) => Applicative (AppendOnlyT mark w m)
  where
    pure
      :: a
      -> AppendOnlyT mark w m a
    pure x =
      AppendOnlyT $ \w -> pure $ Pair w x

    (<*>)
      :: AppendOnlyT mark w m (a -> b)
      -> AppendOnlyT mark w m a
      -> AppendOnlyT mark w m b
    (AppendOnlyT f) <*> (AppendOnlyT x) =
      AppendOnlyT $ \w1 -> do
        Pair w2 g <- f w1
        Pair w3 a <- x w2
        return $ Pair w3 (g a)

instance
  ( Monad m, MonadIdentity mark, Monoid w
  ) => Monad (AppendOnlyT mark w m)
  where
    return
      :: a
      -> AppendOnlyT mark w m a
    return x =
      AppendOnlyT $ \w -> return $ Pair w x

    (>>=)
      :: AppendOnlyT mark w m a
      -> (a -> AppendOnlyT mark w m b)
      -> AppendOnlyT mark w m b
    (AppendOnlyT x) >>= f =
      AppendOnlyT $ \w1 -> do
        Pair w2 a <- x w1
        unAppendOnlyT (f a) w2

instance
  ( MonadIdentity mark, Monoid w
  ) => MonadTrans (AppendOnlyT mark w)
  where
    lift
      :: ( Monad m )
      => m a
      -> AppendOnlyT mark w m a
    lift x = AppendOnlyT $ \w -> fmap (\a -> Pair w a) x

instance
  ( MonadIdentity mark, Monoid w
  ) => MonadFunctor (AppendOnlyT mark w)
  where
    hoist
      :: ( Monad m, Monad n )
      => (forall u. m u -> n u)
      -> AppendOnlyT mark w m a
      -> AppendOnlyT mark w n a
    hoist f (AppendOnlyT x) =
      AppendOnlyT $ \w -> do
        a <- f $ fmap slot2 (x w)
        return $ Pair w a

instance
  ( MonadIdentity mark, Monoid w
  ) => RunMonadTrans (mark ()) (AppendOnlyT mark w) (Pair (mark w))
  where
    runT
      :: ( Monad m )
      => mark ()
      -> AppendOnlyT mark w m a
      -> m (Pair (mark w) a)
    runT _ (AppendOnlyT x) = do
      Pair w a <- x mempty
      return $ Pair (pure w) a





{- Specialized Lifts -}

instance
  ( MonadIdentity mark, Monoid w
  ) => LiftCatch (mark ()) (AppendOnlyT mark w) (Pair (mark w))
  where
    liftCatch
      :: ( Monad m )
      => Catch e m (Pair (mark w) a)
      -> Catch e (AppendOnlyT mark w m) a
    liftCatch catch x h = AppendOnlyT $ \w ->
      fmap (bimap1 unwrap) $ catch
        (fmap (bimap1 pure) $ unAppendOnlyT x w)
        (\e -> fmap (bimap1 pure) $ unAppendOnlyT (h e) w)

instance
  ( MonadIdentity mark, Monoid w
  ) => LiftDraft (mark ()) (AppendOnlyT mark w) (Pair (mark w))
  where
    liftDraft
      :: ( Monad m )
      => Draft w1 m (Pair (mark w) a)
      -> Draft w1 (AppendOnlyT mark w m) a
    liftDraft draft x =
      AppendOnlyT $ \w -> do
        Pair w_ (Pair w1 a) <- draft $ fmap (bimap1 pure) $ unAppendOnlyT x w
        return $ Pair (unwrap w1) (Pair w_ a)

instance
  ( MonadIdentity mark, Monoid w
  ) => LiftLocal (mark ()) (AppendOnlyT mark w) (Pair (mark w))
  where
    liftLocal
      :: ( Monad m )
      => Local r m (Pair (mark w) a)
      -> Local r (AppendOnlyT mark w m) a
    liftLocal local f x =
      AppendOnlyT $ \w1 -> do
        Pair w2 a <- local f $ fmap (bimap1 pure) $ unAppendOnlyT x w1
        return $ Pair (unwrap w2) a





{- Effect Class -}

instance {-# OVERLAPPING #-}
  ( Monad m, MonadIdentity mark, Monoid w
  ) => MonadAppendOnly mark w (AppendOnlyT mark w m)
  where
    look
      :: AppendOnlyT mark w m (mark w)
    look = AppendOnlyT $ \w -> return (Pair w (pure w))

    jot
      :: mark w
      -> AppendOnlyT mark w m ()
    jot w2 = AppendOnlyT $ \w1 -> return (Pair (w1 <> (unwrap w2)) ())

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadIdentity mark, MonadIdentity mark1, Monoid w, Monoid w1
  , MonadAppendOnly mark w m
  ) => MonadAppendOnly mark w (AppendOnlyT mark1 w1 m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1, Monoid w
  , MonadState mark s m
  ) => MonadState mark s (AppendOnlyT mark1 w m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1, Monoid w
  , MonadReadOnly mark r m
  ) => MonadReadOnly mark r (AppendOnlyT mark1 w m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadWriteOnly mark w m, Monoid w, Monoid w1
  ) => MonadWriteOnly mark w (AppendOnlyT mark1 w1 m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1, Monoid w
  , MonadExcept mark e m
  ) => MonadExcept mark e (AppendOnlyT mark1 w m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1, Monoid w
  , MonadPrompt mark p m
  ) => MonadPrompt mark p (AppendOnlyT mark1 w m)

instance
  ( Monad m, MonadIdentity mark1, MonadIdentity mark, Monoid w
  , MonadHalt mark m
  ) => MonadHalt mark (AppendOnlyT mark1 w m)
