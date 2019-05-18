-- | Module      : Control.FX.Monad.Trans.AppendOnlyT
--   Description : Concrete append-only state monad transformer
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

module Control.FX.Monad.Trans.AppendOnlyT (
    AppendOnlyT(..)
  , runAppendOnlyT
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
newtype AppendOnlyT
  (mark :: * -> *)
  (w :: *)
  (m :: * -> *)
  (a :: *)
    = AppendOnlyT
        { unAppendOnlyT :: w -> m (Pair w a)
        } deriving (Typeable)



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
    fmap f x =
      x >>= (return . f)

instance
  ( Monad m, MonadIdentity mark, Monoid w
  ) => Applicative (AppendOnlyT mark w m)
  where
    pure
      :: a
      -> AppendOnlyT mark w m a
    pure = return

    (<*>)
      :: AppendOnlyT mark w m (a -> b)
      -> AppendOnlyT mark w m a
      -> AppendOnlyT mark w m b
    (<*>) = ap

instance
  ( Monad m, MonadIdentity mark, Monoid w
  ) => Monad (AppendOnlyT mark w m)
  where
    return
      :: a
      -> AppendOnlyT mark w m a
    return x =
      AppendOnlyT $ \_ ->
        return $ Pair mempty x

    (>>=)
      :: AppendOnlyT mark w m a
      -> (a -> AppendOnlyT mark w m b)
      -> AppendOnlyT mark w m b
    (AppendOnlyT x) >>= f =
      AppendOnlyT $ \w1 -> do
        Pair w2 a <- x w1
        Pair w3 b <- unAppendOnlyT (f a) (w1 <> w2)
        return $ Pair (w2 <> w3) b

instance
  ( MonadIdentity mark, Monoid w
  ) => MonadTrans (AppendOnlyT mark w)
  where
    lift
      :: ( Monad m )
      => m a
      -> AppendOnlyT mark w m a
    lift x = AppendOnlyT $ \_ ->
      fmap (\a -> Pair mempty a) x

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
        return $ Pair mempty a





instance
  ( EqIn m, MonadIdentity mark, Eq w
  ) => EqIn (AppendOnlyT mark w m)
  where
    newtype Context (AppendOnlyT mark w m)
      = AppendOnlyTCtx
          { unAppendOnlyTCtx :: (mark w, Context m)
          } deriving (Typeable)

    eqIn
      :: (Eq a)
      => Context (AppendOnlyT mark w m)
      -> AppendOnlyT mark w m a
      -> AppendOnlyT mark w m a
      -> Bool
    eqIn (AppendOnlyTCtx (w,h)) (AppendOnlyT x) (AppendOnlyT y) =
      eqIn h (x $ unwrap w) (y $ unwrap w)

deriving instance
  ( Eq (mark w), Eq (Context m)
  ) => Eq (Context (AppendOnlyT mark w m))

deriving instance
  ( Show (mark w), Show (Context m)
  ) => Show (Context (AppendOnlyT mark w m))



instance
  ( MonadIdentity mark, Monoid w
  ) => RunMonadTrans (AppendOnlyT mark w)
  where
    newtype InputT (AppendOnlyT mark w)
      = AppendOnlyTIn
          { unAppendOnlyTIn :: mark ()
          } deriving (Typeable)

    newtype OutputT (AppendOnlyT mark w) a
      = AppendOnlyTOut
          { unAppendOnlyTOut :: Pair (mark w) a
          } deriving (Typeable)

    runT
      :: ( Monad m )
      => InputT (AppendOnlyT mark w)
      -> AppendOnlyT mark w m a
      -> m (OutputT (AppendOnlyT mark w) a)
    runT _ (AppendOnlyT x) = do
      Pair w a <- x mempty
      return $ AppendOnlyTOut $ Pair (pure w) a

runAppendOnlyT
  :: ( Monad m, MonadIdentity mark, Monoid w )
  => AppendOnlyT mark w m a
  -> m (Pair (mark w) a)
runAppendOnlyT =
  fmap unAppendOnlyTOut . runT (AppendOnlyTIn $ pure ())

deriving instance
  ( Eq (mark ())
  ) => Eq (InputT (AppendOnlyT mark w))

deriving instance
  ( Show (mark ())
  ) => Show (InputT (AppendOnlyT mark w))

deriving instance
  ( Eq (mark w), Eq a
  ) => Eq (OutputT (AppendOnlyT mark w) a)

deriving instance
  ( Show (mark w), Show a
  ) => Show (OutputT (AppendOnlyT mark w) a)





{- Specialized Lifts -}

instance
  ( MonadIdentity mark, Monoid w
  ) => LiftCatch (AppendOnlyT mark w)
  where
    liftCatch
      :: ( Monad m )
      => Catch e m (OutputT (AppendOnlyT mark w) a)
      -> Catch e (AppendOnlyT mark w m) a
    liftCatch catch x h = AppendOnlyT $ \w ->
      fmap (bimap1 unwrap . unAppendOnlyTOut) $ catch
        (fmap (AppendOnlyTOut . bimap1 pure) $ unAppendOnlyT x w)
        (\e -> fmap (AppendOnlyTOut . bimap1 pure) $ unAppendOnlyT (h e) w)

instance
  ( MonadIdentity mark, Monoid w
  ) => LiftDraft (AppendOnlyT mark w)
  where
    liftDraft
      :: ( Monad m )
      => Draft w1 m (OutputT (AppendOnlyT mark w) a)
      -> Draft w1 (AppendOnlyT mark w m) a
    liftDraft draft x =
      AppendOnlyT $ \w -> do
        Pair w_ (AppendOnlyTOut (Pair w1 a)) <-
          draft $ fmap (AppendOnlyTOut . bimap1 pure) $ unAppendOnlyT x w
        return $ Pair (unwrap w1) (Pair w_ a)

instance
  ( MonadIdentity mark, Monoid w
  ) => LiftLocal (AppendOnlyT mark w)
  where
    liftLocal
      :: ( Monad m )
      => Local r m (OutputT (AppendOnlyT mark w) a)
      -> Local r (AppendOnlyT mark w m) a
    liftLocal local f x =
      AppendOnlyT $ \w1 -> do
        AppendOnlyTOut (Pair w2 a) <-
          local f $ fmap (AppendOnlyTOut . bimap1 pure) $ unAppendOnlyT x w1
        return $ Pair (unwrap w2) a





{- Effect Class -}

instance {-# OVERLAPPING #-}
  ( Monad m, MonadIdentity mark, Monoid w
  ) => MonadAppendOnly mark w (AppendOnlyT mark w m)
  where
    look
      :: AppendOnlyT mark w m (mark w)
    look = AppendOnlyT $ \w ->
      return (Pair mempty (pure w))

    jot
      :: mark w
      -> AppendOnlyT mark w m ()
    jot w = AppendOnlyT $ \_ ->
      return $ Pair (unwrap w) ()

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadIdentity mark, MonadIdentity mark1, Monoid w, Monoid w1
  , MonadAppendOnly mark w m
  ) => MonadAppendOnly mark w (AppendOnlyT mark1 w1 m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1, Monoid w, Monoid w1
  , MonadWriteOnce mark w m
  ) => MonadWriteOnce mark w (AppendOnlyT mark1 w1 m)

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
