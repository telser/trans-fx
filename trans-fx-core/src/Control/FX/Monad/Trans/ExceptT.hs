-- | Module      : Control.FX.Monad.Trans.ExceptT
--   Description : Concrete exception monad transformer
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
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.Trans.ExceptT (
    ExceptT(..)
  , runExceptT
  , Context(..)
  , InputT(..)
  , OutputT(..)
) where



import Data.Typeable (Typeable, typeOf)
import Control.Applicative (liftA2)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans.Class



-- | Concrete exception monad transformer
newtype ExceptT
  (mark :: * -> *)
  (e :: *)
  (m :: * -> *)
  (a :: *)
    = ExceptT
        { unExceptT :: m (Except mark e a)
        } deriving (Typeable)



deriving instance
  ( Show (m (Except mark e a))
  ) => Show (ExceptT mark e m a)

instance
  ( Monad m, MonadIdentity mark
  ) => Functor (ExceptT mark e m)
  where
    fmap
      :: (a -> b)
      -> ExceptT mark e m a
      -> ExceptT mark e m b
    fmap f = ExceptT . fmap (fmap f) . unExceptT

instance
  ( Monad m, MonadIdentity mark
  ) => Applicative (ExceptT mark e m)
  where
    pure
      :: a
      -> ExceptT mark e m a
    pure = ExceptT . pure . pure

    (<*>)
      :: ExceptT mark e m (a -> b)
      -> ExceptT mark e m a
      -> ExceptT mark e m b
    (ExceptT f) <*> (ExceptT x) =
      ExceptT $ liftA2 (<*>) f x

instance
  ( Monad m, MonadIdentity mark
  ) => Monad (ExceptT mark e m)
  where
    return
      :: a
      -> ExceptT mark e m a
    return = ExceptT . return . Accept

    (>>=)
      :: ExceptT mark e m a
      -> (a -> ExceptT mark e m b)
      -> ExceptT mark e m b
    (ExceptT x) >>= f =
      ExceptT $ do
        a' <- x
        case a' of
          Except e -> return (Except e)
          Accept a -> unExceptT $ f a

instance
  ( Central c, MonadIdentity mark
  ) => Commutant (ExceptT mark e c)
  where
    commute
      :: ( Applicative f )
      => ExceptT mark e c (f a)
      -> f (ExceptT mark e c a)
    commute = fmap ExceptT . commute . fmap commute . unExceptT

instance
  ( Central c, MonadIdentity mark
  ) => Central (ExceptT mark e c)

instance
  ( MonadIdentity mark
  ) => MonadTrans (ExceptT mark e) where
  lift
    :: ( Monad m )
    => m a
    -> ExceptT mark e m a
  lift x = ExceptT (x >>= (return . pure))

instance
  ( MonadIdentity mark
  ) => MonadFunctor (ExceptT mark e) where
  hoist
    :: ( Monad m, Monad n )
    => ( forall u. m u -> n u )
    -> ExceptT mark e m a
    -> ExceptT mark e n a
  hoist f = ExceptT . f . unExceptT





instance
  ( EqIn m, MonadIdentity mark, Eq e
  ) => EqIn (ExceptT mark e m)
  where
    newtype Context (ExceptT mark e m)
      = ExceptTCtx
          { unExceptTCtx :: (mark (), Context m)
          } deriving (Typeable)

    eqIn
      :: (Eq a)
      => Context (ExceptT mark e m)
      -> ExceptT mark e m a
      -> ExceptT mark e m a
      -> Bool
    eqIn (ExceptTCtx (_,h)) x y =
      eqIn h (unExceptT x) (unExceptT y)

deriving instance
  ( Eq (mark ()), Eq (Context m)
  ) => Eq (Context (ExceptT mark e m))

deriving instance
  ( Show (mark ()), Show (Context m)
  ) => Show (Context (ExceptT mark e m))



instance
  ( MonadIdentity mark
  ) => RunMonadTrans (ExceptT mark e)
  where
    newtype InputT (ExceptT mark e)
      = ExceptTIn
          { unExceptTIn :: mark ()
          } deriving (Typeable)

    newtype OutputT (ExceptT mark e) a
      = ExceptTOut
          { unExceptTOut :: Except mark e a
          } deriving (Eq, Show, Typeable)

    runT
      :: ( Monad m )
      => InputT (ExceptT mark e)
      -> ExceptT mark e m a
      -> m (OutputT (ExceptT mark e) a)
    runT _ (ExceptT x) = fmap ExceptTOut x

runExceptT
  :: ( Monad m, MonadIdentity mark )
  => ExceptT mark e m a
  -> m (Except mark e a)
runExceptT =
  fmap unExceptTOut . runT (ExceptTIn $ pure ())

deriving instance
  ( Eq (mark ())
  ) => Eq (InputT (ExceptT mark e))

deriving instance
  ( Show (mark ())
  ) => Show (InputT (ExceptT mark e))

instance
  ( MonadIdentity mark
  ) => Functor (OutputT (ExceptT mark e))
  where
    fmap f (ExceptTOut x) = ExceptTOut (fmap f x)

instance
  ( MonadIdentity mark
  ) => Applicative (OutputT (ExceptT mark e))
  where
    pure = ExceptTOut . pure

    (ExceptTOut f) <*> (ExceptTOut x) =
      ExceptTOut (f <*> x)





{- Specialized Lifts -}

instance
  ( MonadIdentity mark
  ) => LiftCatch (ExceptT mark e)
  where
    liftCatch
      :: ( Monad m )
      => Catch e2 m (OutputT (ExceptT mark e) a)
      -> Catch e2 (ExceptT mark e m) a
    liftCatch catch x h = ExceptT $
      fmap unExceptTOut $ catch
        (fmap ExceptTOut $ unExceptT x)
        (fmap ExceptTOut . unExceptT . h)

instance
  ( MonadIdentity mark
  ) => LiftDraft (ExceptT mark e)
  where
    liftDraft
      :: ( Monad m )
      => Draft w m (OutputT (ExceptT mark e) a)
      -> Draft w (ExceptT mark e m) a
    liftDraft draft =
      ExceptT . fmap unExceptTOut . fmap commute . draft . fmap ExceptTOut . unExceptT

instance
  ( MonadIdentity mark
  ) => LiftLocal (ExceptT mark e)
  where
    liftLocal
      :: ( Monad m )
      => Local r m (OutputT (ExceptT mark e) a)
      -> Local r (ExceptT mark e m) a
    liftLocal local f =
      ExceptT . fmap unExceptTOut . local f . fmap ExceptTOut . unExceptT





{- Effect Classes -}

instance {-# OVERLAPPING #-}
  ( Monad m, MonadIdentity mark
  ) => MonadExcept mark e (ExceptT mark e m)
  where
    throw
      :: mark e
      -> ExceptT mark e m a
    throw = ExceptT . return . Except . unwrap

    catch
      :: ExceptT mark e m a
      -> (mark e -> ExceptT mark e m a)
      -> ExceptT mark e m a
    catch (ExceptT x) h = ExceptT $ do
      a <- x
      case a of
        Except e -> unExceptT $ h (pure e)
        Accept z -> return (Accept z)

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadExcept mark e m
  ) => MonadExcept mark e (ExceptT mark1 e1 m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1, Monoid w
  , MonadWriteOnly mark w m
  ) => MonadWriteOnly mark w (ExceptT mark1 e m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadState mark s m
  ) => MonadState mark s (ExceptT mark1 e m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadReadOnly mark r m
  ) => MonadReadOnly mark r (ExceptT mark1 e m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadHalt mark m
  ) => MonadHalt mark (ExceptT mark1 e m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadPrompt mark p m
  ) => MonadPrompt mark p (ExceptT mark1 e m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1, Monoid w
  , MonadAppendOnly mark w m
  ) => MonadAppendOnly mark w (ExceptT mark1 e m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadWriteOnce mark w m
  ) => MonadWriteOnce mark w (ExceptT mark1 e m)
