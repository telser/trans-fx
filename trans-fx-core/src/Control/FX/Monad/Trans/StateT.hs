-- | Module      : Control.FX.Monad.Trans.StateT
--   Description : Concrete mutable state monad transformer
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.Trans.StateT (
    StateT(..)
  , Context(..)
  , InputT(..)
  , OutputT(..)
) where



import Data.Typeable (Typeable, typeOf)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans.Class



-- | Concrete @State@ monad transformer
newtype StateT
  (mark :: * -> *)
  (s :: *)
  (m :: * -> *)
  (a :: *)
    = StateT
        { unStateT :: s -> m (Pair s a)
        } deriving (Typeable)



instance
  ( Typeable s, Typeable m, Typeable a, Typeable mark
  ) => Show (StateT mark s m a)
  where
    show
      :: StateT mark s m a
      -> String
    show = show . typeOf

instance
  ( Monad m, MonadIdentity mark
  ) => Functor (StateT mark s m)
  where
    fmap
      :: (a -> b)
      -> StateT mark s m a
      -> StateT mark s m b
    fmap f (StateT x) =
      StateT $ \s1 -> do
        Pair s2 a <- x s1
        return $ Pair s2 (f a)

instance
  ( Monad m, MonadIdentity mark
  ) => Applicative (StateT mark s m)
  where
    pure
      :: a
      -> StateT mark s m a
    pure x =
      StateT $ \s -> pure $ Pair s x

    (<*>)
      :: StateT mark s m (a -> b)
      -> StateT mark s m a
      -> StateT mark s m b
    (StateT f) <*> (StateT x) =
      StateT $ \s1 -> do
        Pair s2 g <- f s1
        Pair s3 a <- x s2
        return $ Pair s3 (g a)

instance
  ( Monad m, MonadIdentity mark
  ) => Monad (StateT mark s m)
  where
    return
      :: a
      -> StateT mark s m a
    return x =
      StateT $ \s -> return $ Pair s x

    (>>=)
      :: StateT mark s m a
      -> (a -> StateT mark s m b)
      -> StateT mark s m b
    (StateT x) >>= f =
      StateT $ \s1 -> do
        Pair s2 a <- x s1
        unStateT (f a) s2

instance
  ( MonadIdentity mark
  ) => MonadTrans (StateT mark s)
  where
    lift
      :: ( Monad m )
      => m a
      -> StateT mark s m a
    lift x = StateT $ \s -> fmap (\a -> Pair s a) x

instance
  ( MonadIdentity mark
  ) => MonadFunctor (StateT mark s)
  where
    hoist
      :: ( Monad m, Monad n )
      => (forall u. m u -> n u)
      -> StateT mark s m a
      -> StateT mark s n a
    hoist f (StateT x) =
      StateT $ \s -> do
        a <- f $ fmap slot2 (x s)
        return $ Pair s a





instance
  ( EqIn m, MonadIdentity mark, Eq s
  ) => EqIn (StateT mark s m)
  where
    newtype Context (StateT mark s m)
      = StateTCtx
          { unStateTCtx :: (mark s, Context m)
          } deriving (Typeable)

    eqIn
      :: (Eq a)
      => Context (StateT mark s m)
      -> StateT mark s m a
      -> StateT mark s m a
      -> Bool
    eqIn (StateTCtx (s,h)) (StateT x) (StateT y) =
      eqIn h (x $ unwrap s) (y $ unwrap s)

deriving instance
  ( Eq (mark s), Eq (Context m)
  ) => Eq (Context (StateT mark s m))

deriving instance
  ( Show (mark s), Show (Context m)
  ) => Show (Context (StateT mark s m))



instance
  ( MonadIdentity mark
  ) => RunMonadTrans (StateT mark s)
  where
    newtype InputT (StateT mark s)
      = StateTIn
          { unStateTIn :: mark s
          } deriving (Typeable)

    newtype OutputT (StateT mark s) a
      = StateTOut
          { unStateTOut :: Pair (mark s) a
          } deriving (Typeable)

    runT
      :: ( Monad m )
      => InputT (StateT mark s)
      -> StateT mark s m a
      -> m (OutputT (StateT mark s) a)
    runT (StateTIn s) (StateT x) = do
      Pair s1 a <- x (unwrap s)
      return $ StateTOut $ Pair (pure s1) a

deriving instance
  ( Eq (mark s)
  ) => Eq (InputT (StateT mark s))

deriving instance
  ( Show (mark s)
  ) => Show (InputT (StateT mark s))

deriving instance
  ( Eq (mark s), Eq a
  ) => Eq (OutputT (StateT mark s) a)

deriving instance
  ( Show (mark s), Show a
  ) => Show (OutputT (StateT mark s) a)





{- Specialized Lifts -}

instance
  ( MonadIdentity mark
  ) => LiftCatch (StateT mark s)
  where
    liftCatch
      :: ( Monad m )
      => Catch e m (OutputT (StateT mark s) a)
      -> Catch e (StateT mark s m) a
    liftCatch catch x h = StateT $ \s ->
      fmap (bimap1 unwrap . unStateTOut) $ catch
        (fmap (StateTOut . bimap1 pure) $ unStateT x s)
        (\e -> fmap (StateTOut . bimap1 pure) $ unStateT (h e) s)

instance
  ( MonadIdentity mark
  ) => LiftDraft (StateT mark s)
  where
    liftDraft
      :: ( Monad m )
      => Draft w m (OutputT (StateT mark s) a)
      -> Draft w (StateT mark s m) a
    liftDraft draft x =
      StateT $ \s -> do
        Pair w (StateTOut (Pair s a)) <- draft $ fmap (StateTOut . bimap1 pure) $ unStateT x s
        return $ Pair (unwrap s) (Pair w a)

instance
  ( MonadIdentity mark
  ) => LiftLocal (StateT mark s)
  where
    liftLocal
      :: ( Monad m )
      => Local r m (OutputT (StateT mark s) a)
      -> Local r (StateT mark s m) a
    liftLocal local f x =
      StateT $ \s -> do
        StateTOut (Pair s1 a) <- local f $ fmap (StateTOut . bimap1 pure) $ unStateT x s
        return $ Pair (unwrap s1) a





{- Effect Classes -}

instance {-# OVERLAPPING #-}
  ( Monad m, MonadIdentity mark
  ) => MonadState mark s (StateT mark s m)
  where
    get
      :: StateT mark s m (mark s)
    get = StateT $ \s -> return (Pair s (pure s))

    put
      :: mark s
      -> StateT mark s m ()
    put s = StateT $ \_ -> return (Pair (unwrap s) ())

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadState mark s m
  ) => MonadState mark s (StateT mark1 s1 m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadReadOnly mark r m
  ) => MonadReadOnly mark r (StateT mark1 s m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadWriteOnly mark w m, Monoid w
  ) => MonadWriteOnly mark w (StateT mark1 s m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadExcept mark e m
  ) => MonadExcept mark e (StateT mark1 s m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadPrompt mark p m
  ) => MonadPrompt mark p (StateT mark1 s m)

instance
  ( Monad m, MonadIdentity mark1, MonadIdentity mark
  , MonadHalt mark m
  ) => MonadHalt mark (StateT mark1 s m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadAppendOnly mark w m, Monoid w
  ) => MonadAppendOnly mark w (StateT mark1 s m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadWriteOnce mark w m
  ) => MonadWriteOnce mark w (StateT mark1 s m)
