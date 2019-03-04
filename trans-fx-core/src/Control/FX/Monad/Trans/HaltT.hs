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

type instance Context (HaltT mark m) =
  (mark (), Context m)

instance
  ( EqIn m
  ) => EqIn (HaltT mark m)
  where
    eqIn
      :: (Eq a)
      => (mark (), Context m)
      -> HaltT mark m a
      -> HaltT mark m a
      -> Bool
    eqIn (_,h) (HaltT x) (HaltT y) =
      eqIn h x y



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
  ( MonadIdentity mark
  ) => RunMonadTrans (mark ()) (HaltT mark) (Halt mark)
  where
    runT
      :: ( Monad m )
      => mark ()
      -> HaltT mark m a
      -> m (Halt mark a)
    runT _ (HaltT x) = x





{- Specialized Lifts -}

instance
  ( MonadIdentity mark
  ) => LiftCatch (mark ()) (HaltT mark) (Halt mark)
  where
    liftCatch
      :: ( Monad m )
      => Catch e m (Halt mark a)
      -> Catch e (HaltT mark m) a
    liftCatch catch x h = HaltT $
      catch (unHaltT x) (unHaltT . h)

instance
  ( MonadIdentity mark
  ) => LiftDraft (mark ()) (HaltT mark) (Halt mark)
  where
    liftDraft
      :: ( Monad m )
      => Draft w m (Halt mark a)
      -> Draft w (HaltT mark m) a
    liftDraft draft =
      HaltT . fmap commute . draft . unHaltT

instance
  ( MonadIdentity mark
  ) => LiftLocal (mark ()) (HaltT mark) (Halt mark)
  where
    liftLocal
      :: ( Monad m )
      => Local r m (Halt mark a)
      -> Local r (HaltT mark m) a
    liftLocal local f =
      HaltT . local f . unHaltT





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
