-- | Module      : Control.FX.Monad.Trans.WriteOnceT
--   Description : Concrete write-once state monad transformer
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
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.Trans.WriteOnceT (
    WriteOnceT(..)
  , Context(..)
  , InputT(..)
  , OutputT(..)
) where



import Data.Typeable (Typeable, typeOf)
import Control.Monad (ap)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans.Class



-- | Concrete @State@ monad transformer
newtype WriteOnceT
  (mark :: * -> *)
  (w :: *)
  (m :: * -> *)
  (a :: *)
    = WriteOnceT
        { unWriteOnceT :: LeftZero w -> m (Pair (LeftZero w) a)
        } deriving (Typeable)



instance
  ( Typeable w, Typeable m, Typeable a, Typeable mark
  ) => Show (WriteOnceT mark w m a)
  where
    show
      :: WriteOnceT mark w m a
      -> String
    show = show . typeOf

instance
  ( Monad m, MonadIdentity mark
  ) => Functor (WriteOnceT mark w m)
  where
    fmap
      :: (a -> b)
      -> WriteOnceT mark w m a
      -> WriteOnceT mark w m b
    fmap f x =
      x >>= (return . f)

instance
  ( Monad m, MonadIdentity mark
  ) => Applicative (WriteOnceT mark w m)
  where
    pure
      :: a
      -> WriteOnceT mark w m a
    pure = return

    (<*>)
      :: WriteOnceT mark w m (a -> b)
      -> WriteOnceT mark w m a
      -> WriteOnceT mark w m b
    (<*>) = ap

instance
  ( Monad m, MonadIdentity mark
  ) => Monad (WriteOnceT mark w m)
  where
    return
      :: a
      -> WriteOnceT mark w m a
    return x =
      WriteOnceT $ \_ ->
        return $ Pair mempty x

    (>>=)
      :: WriteOnceT mark w m a
      -> (a -> WriteOnceT mark w m b)
      -> WriteOnceT mark w m b
    (WriteOnceT x) >>= f =
      WriteOnceT $ \w1 -> do
        Pair w2 a <- x w1
        Pair w3 b <- unWriteOnceT (f a) (w1 <> w2)
        return $ Pair (w2 <> w3) b

instance
  ( MonadIdentity mark
  ) => MonadTrans (WriteOnceT mark w)
  where
    lift
      :: ( Monad m )
      => m a
      -> WriteOnceT mark w m a
    lift x = WriteOnceT $ \_ ->
      fmap (\a -> Pair mempty a) x

instance
  ( MonadIdentity mark
  ) => MonadFunctor (WriteOnceT mark w)
  where
    hoist
      :: ( Monad m, Monad n )
      => (forall u. m u -> n u)
      -> WriteOnceT mark w m a
      -> WriteOnceT mark w n a
    hoist f (WriteOnceT x) =
      WriteOnceT $ \w -> do
        a <- f $ fmap slot2 (x w)
        return $ Pair mempty a





instance
  ( EqIn m, MonadIdentity mark, Eq w
  ) => EqIn (WriteOnceT mark w m)
  where
    newtype Context (WriteOnceT mark w m)
      = WriteOnceTCtx
          { unWriteOnceTCtx :: (mark (), Context m)
          } deriving (Typeable)

    eqIn
      :: (Eq a)
      => Context (WriteOnceT mark w m)
      -> WriteOnceT mark w m a
      -> WriteOnceT mark w m a
      -> Bool
    eqIn (WriteOnceTCtx (w,h)) (WriteOnceT x) (WriteOnceT y) =
      eqIn h (x mempty) (y mempty)

deriving instance
  ( Eq (mark ()), Eq (Context m)
  ) => Eq (Context (WriteOnceT mark w m))

deriving instance
  ( Show (mark ()), Show (Context m)
  ) => Show (Context (WriteOnceT mark w m))



instance
  ( MonadIdentity mark
  ) => RunMonadTrans (WriteOnceT mark w)
  where
    newtype InputT (WriteOnceT mark w)
      = WriteOnceTIn
          { unWriteOnceTIn :: mark ()
          } deriving (Typeable)

    newtype OutputT (WriteOnceT mark w) a
      = WriteOnceTOut
          { unWriteOnceTOut :: Pair (mark (Maybe w)) a
          } deriving (Typeable)

    runT
      :: ( Monad m )
      => InputT (WriteOnceT mark w)
      -> WriteOnceT mark w m a
      -> m (OutputT (WriteOnceT mark w) a)
    runT _ (WriteOnceT x) = do
      Pair w a <- x mempty
      return $ WriteOnceTOut $ Pair (pure $ toMaybe w) a

deriving instance
  ( Eq (mark ())
  ) => Eq (InputT (WriteOnceT mark w))

deriving instance
  ( Show (mark ())
  ) => Show (InputT (WriteOnceT mark w))

deriving instance
  ( Eq (mark (Maybe w)), Eq a
  ) => Eq (OutputT (WriteOnceT mark w) a)

deriving instance
  ( Show (mark (Maybe w)), Show a
  ) => Show (OutputT (WriteOnceT mark w) a)





{- Specialized Lifts -}

instance
  ( MonadIdentity mark
  ) => LiftCatch (WriteOnceT mark w)
  where
    liftCatch
      :: ( Monad m )
      => Catch e m (OutputT (WriteOnceT mark w) a)
      -> Catch e (WriteOnceT mark w m) a
    liftCatch catch x h = WriteOnceT $ \w ->
      fmap (bimap1 (fromMaybe . unwrap) . unWriteOnceTOut) $ catch
        (fmap (WriteOnceTOut . bimap1 (pure . toMaybe)) $ unWriteOnceT x w)
        (\e -> fmap (WriteOnceTOut . bimap1 (pure . toMaybe)) $ unWriteOnceT (h e) w)

instance
  ( MonadIdentity mark
  ) => LiftDraft (WriteOnceT mark w)
  where
    liftDraft
      :: ( Monad m )
      => Draft w1 m (OutputT (WriteOnceT mark w) a)
      -> Draft w1 (WriteOnceT mark w m) a
    liftDraft draft x =
      WriteOnceT $ \w -> do
        Pair w_ (WriteOnceTOut (Pair w1 a)) <-
          draft $ fmap (WriteOnceTOut . bimap1 (pure . toMaybe)) $ unWriteOnceT x w
        return $ Pair (fromMaybe $ unwrap w1) (Pair w_ a)

instance
  ( MonadIdentity mark
  ) => LiftLocal (WriteOnceT mark w)
  where
    liftLocal
      :: ( Monad m )
      => Local r m (OutputT (WriteOnceT mark w) a)
      -> Local r (WriteOnceT mark w m) a
    liftLocal local f x =
      WriteOnceT $ \w1 -> do
        WriteOnceTOut (Pair w2 a) <-
          local f $ fmap (WriteOnceTOut . bimap1 (pure . toMaybe)) $ unWriteOnceT x w1
        return $ Pair (fromMaybe $ unwrap w2) a





{- Effect Class -}

instance {-# OVERLAPPING #-}
  ( Monad m, MonadIdentity mark
  ) => MonadWriteOnce mark w (WriteOnceT mark w m)
  where
    press
      :: WriteOnceT mark w m (Maybe (mark w))
    press = WriteOnceT $ \w ->
      return (Pair mempty (fmap pure $ toMaybe w))

    etch
      :: mark w
      -> WriteOnceT mark w m Bool
    etch w = WriteOnceT $ \w1 ->
      case w1 of
        LeftUnit   -> return $ Pair (LeftZero $ unwrap w) True
        LeftZero _ -> return $ Pair mempty False

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadWriteOnce mark w m
  ) => MonadWriteOnce mark w (WriteOnceT mark1 w1 m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadState mark s m
  ) => MonadState mark s (WriteOnceT mark1 w m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadReadOnly mark r m
  ) => MonadReadOnly mark r (WriteOnceT mark1 w m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadAppendOnly mark w m
  ) => MonadAppendOnly mark w (WriteOnceT mark1 w1 m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadWriteOnly mark w m
  ) => MonadWriteOnly mark w (WriteOnceT mark1 w1 m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadExcept mark e m
  ) => MonadExcept mark e (WriteOnceT mark1 w m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadPrompt mark p m
  ) => MonadPrompt mark p (WriteOnceT mark1 w m)

instance
  ( Monad m, MonadIdentity mark1, MonadIdentity mark
  , MonadHalt mark m
  ) => MonadHalt mark (WriteOnceT mark1 w m)
