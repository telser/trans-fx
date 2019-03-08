-- | Module      : Control.FX.Monad.Trans.HaltT
--   Description : Concrete halt monad transformer
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

module Control.FX.Monad.Trans.HaltT (
    HaltT(..)
  , Context(..)
  , InputT(..)
  , OutputT(..)
) where



import Data.Typeable (Typeable)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans.Class



-- | Concrete @Maybe@ monad transformer
newtype HaltT
  (mark :: * -> *)
  (m :: * -> *)
  (a :: *)
    = HaltT
        { unHaltT :: m (Halt mark a)
        } deriving (Typeable)



deriving instance
  ( Show (m (Halt mark a))
  ) => Show (HaltT mark m a)

instance
  ( Monad m, MonadIdentity mark
  ) => Functor (HaltT mark m)
  where
    fmap
      :: (a -> b)
      -> HaltT mark m a
      -> HaltT mark m b
    fmap f =
      HaltT . fmap (fmap f) . unHaltT

instance
  ( Monad m, MonadIdentity mark
  ) => Applicative (HaltT mark m)
  where
    pure
      :: a
      -> HaltT mark m a
    pure = HaltT . return . Step

    (<*>)
      :: HaltT mark m (a -> b)
      -> HaltT mark m a
      -> HaltT mark m b
    (HaltT mf) <*> (HaltT mx) =
      HaltT $ do
        f' <- mf
        case f' of
          Halt -> return Halt
          Step f -> do
            x' <- mx
            case x' of
              Halt -> return Halt
              Step x  -> return (Step (f x))

instance
  ( Monad m, MonadIdentity mark
  ) => Monad (HaltT mark m)
  where
    return
      :: a
      -> HaltT mark m a
    return = HaltT . return . Step

    (>>=)
      :: HaltT mark m a
      -> (a -> HaltT mark m b)
      -> HaltT mark m b
    (HaltT x) >>= f =
      HaltT $ do
        a' <- x
        case a' of
          Halt -> return Halt
          Step a  -> unHaltT $ f a

instance
  ( Central c, MonadIdentity mark
  ) => Commutant (HaltT mark c)
  where
    commute
      :: ( Applicative f )
      => HaltT mark c (f a)
      -> f (HaltT mark c a)
    commute = fmap HaltT . commute . fmap commute . unHaltT

instance
  ( Central c, MonadIdentity mark
  ) => Central (HaltT mark c)

instance
  ( MonadIdentity mark
  ) => MonadTrans (HaltT mark)
  where
    lift
      :: ( Monad m )
      => m a
      -> HaltT mark m a
    lift x = HaltT (x >>= (return . Step))

instance
  ( MonadIdentity mark
  ) => MonadFunctor (HaltT mark)
  where
    hoist
      :: ( Monad m, Monad n )
      => ( forall u. m u -> n u )
      -> HaltT mark m a
      -> HaltT mark n a
    hoist f = HaltT . f . unHaltT





instance
  ( EqIn m
  ) => EqIn (HaltT mark m)
  where
    newtype Context (HaltT mark m)
      = HaltTCtx
          { unHaltTCtx :: (mark (), Context m)
          } deriving (Typeable)

    eqIn
      :: (Eq a)
      => Context (HaltT mark m)
      -> HaltT mark m a
      -> HaltT mark m a
      -> Bool
    eqIn (HaltTCtx (_,h)) (HaltT x) (HaltT y) =
      eqIn h x y

deriving instance
  ( Eq (mark ()), Eq (Context m)
  ) => Eq (Context (HaltT mark m))

deriving instance
  ( Show (mark ()), Show (Context m)
  ) => Show (Context (HaltT mark m))



instance
  ( MonadIdentity mark
  ) => RunMonadTrans (HaltT mark)
  where
    newtype InputT (HaltT mark)
      = HaltTIn
          { unHaltTIn :: mark ()
          } deriving (Typeable)

    newtype OutputT (HaltT mark) a
      = HaltTOut
          { unHaltTOut :: Halt mark a
          } deriving (Eq, Show, Typeable)

    runT
      :: ( Monad m )
      => InputT (HaltT mark)
      -> HaltT mark m a
      -> m (OutputT (HaltT mark) a)
    runT _ (HaltT x) = fmap HaltTOut x

deriving instance
  ( Eq (mark ())
  ) => Eq (InputT (HaltT mark))

deriving instance
  ( Show (mark ())
  ) => Show (InputT (HaltT mark))

instance
  ( MonadIdentity mark
  ) => Functor (OutputT (HaltT mark))
  where
    fmap f (HaltTOut x) = HaltTOut (fmap f x)

instance
  ( MonadIdentity mark
  ) => Applicative (OutputT (HaltT mark))
  where
    pure = HaltTOut . pure

    (HaltTOut f) <*> (HaltTOut x) =
      HaltTOut (f <*> x)





{- Specialized Lifts -}

instance
  ( MonadIdentity mark
  ) => LiftCatch (HaltT mark)
  where
    liftCatch
      :: ( Monad m )
      => Catch e m (OutputT (HaltT mark) a)
      -> Catch e (HaltT mark m) a
    liftCatch catch x h = HaltT $
      fmap unHaltTOut $ catch
        (fmap HaltTOut $ unHaltT x)
        (fmap HaltTOut . unHaltT . h)

instance
  ( MonadIdentity mark
  ) => LiftDraft (HaltT mark)
  where
    liftDraft
      :: ( Monad m )
      => Draft w m (OutputT (HaltT mark) a)
      -> Draft w (HaltT mark m) a
    liftDraft draft =
      HaltT . fmap unHaltTOut . fmap commute . draft . fmap HaltTOut . unHaltT

instance
  ( MonadIdentity mark
  ) => LiftLocal (HaltT mark)
  where
    liftLocal
      :: ( Monad m )
      => Local r m (OutputT (HaltT mark) a)
      -> Local r (HaltT mark m) a
    liftLocal local f =
      HaltT . fmap unHaltTOut . local f . fmap HaltTOut . unHaltT





{- Effect Class -}

instance {-# OVERLAPPING #-}
  ( Monad m, MonadIdentity mark
  ) => MonadHalt mark (HaltT mark m)
  where
    halt
      :: mark ()
      -> HaltT mark m a
    halt = HaltT . return . halt

instance {-# OVERLAPPABLE #-}
  ( MonadHalt mark m, MonadIdentity mark, MonadIdentity mark1
  ) => MonadHalt mark (HaltT mark1 m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadReadOnly mark r m
  ) => MonadReadOnly mark r (HaltT mark1 m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadWriteOnly mark w m, Monoid w
  ) => MonadWriteOnly mark w (HaltT mark1 m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadState mark s m
  ) => MonadState mark s (HaltT mark1 m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadExcept mark e m
  ) => MonadExcept mark e (HaltT mark1 m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadPrompt mark p m
  ) => MonadPrompt mark p (HaltT mark1 m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadAppendOnly mark w m, Monoid w
  ) => MonadAppendOnly mark w (HaltT mark1 m)
