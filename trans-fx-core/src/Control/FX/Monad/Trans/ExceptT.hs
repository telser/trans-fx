{-#
  LANGUAGE
    InstanceSigs,
    KindSignatures,
    FlexibleInstances,
    QuantifiedConstraints,
    MultiParamTypeClasses
#-}

module Control.FX.Monad.Trans.ExceptT (
    ExceptT(..)
  , runExceptT
) where

import Data.Typeable (Typeable)
import Control.Applicative (liftA2)

import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans.Class

newtype ExceptT
  (k :: * -> *)
  (e :: *)
  (m :: * -> *)
  (a :: *)
    = ExceptT
        { unExceptT :: m (Except k e a)
        } deriving (Typeable)

instance
  ( Show e, Show a
  , forall u. (Show u) => Show (m u)
  ) => Show (ExceptT mark e m a)
  where
    show (ExceptT x) = "(ExceptT " ++ show x ++ ")"

instance
  ( Monad m
  ) => Functor (ExceptT mark e m)
  where
    fmap f = ExceptT . fmap (fmap f) . unExceptT

instance
  ( Monad m
  ) => Applicative (ExceptT mark e m)
  where
    pure :: a -> ExceptT mark e m a
    pure = ExceptT . pure . pure

    (ExceptT f) <*> (ExceptT x) =
      ExceptT $ liftA2 (<*>) f x

instance
  ( Monad m
  ) => Monad (ExceptT mark e m)
  where
    return :: a -> ExceptT mark e m a
    return = ExceptT . return . Accept

    (ExceptT x) >>= f =
      ExceptT $ do
        a' <- x
        case a' of
          Except e -> return (Except e)
          Accept a -> unExceptT $ f a

instance
  ( Central c
  ) => Commutant (ExceptT mark e c)
  where
    commute
      :: ( Applicative f )
      => ExceptT mark e c (f a) -> f (ExceptT mark e c a)
    commute = fmap ExceptT . commute . fmap commute . unExceptT

instance
  ( Central c
  ) => Central (ExceptT mark e c)

instance MonadTrans (ExceptT mark e) where
  lift x = ExceptT (x >>= (return . pure))

instance MonadFunctor (ExceptT mark e) where
  hoist f = ExceptT . f . unExceptT

instance RunMonadTrans () (ExceptT mark e) (Except mark e) where
  runT :: () -> ExceptT mark e m a -> m (Except mark e a)
  runT () (ExceptT x) = x

runExceptT :: ExceptT mark e m a -> m (Except mark e a)
runExceptT = unExceptT



{- Effect Instance -}

instance
  ( Monad m, MonadIdentity mark
  ) => MonadExcept mark e (ExceptT mark e m)
  where
    throw = ExceptT . return . Except . unwrap
    catch (ExceptT x) h = ExceptT $ do
      a <- x
      case a of
        Except e -> unExceptT $ h (pure e)
        Accept z -> return (Accept z)



{- Specialized Lifts -}

instance LiftDraft () (ExceptT mark e) (Except mark e) where
  liftDraft
    :: (Monad m)
    => Draft w m (Except mark e a) -> Draft w (ExceptT mark e m) a
  liftDraft draft =
    ExceptT . fmap commute . draft . unExceptT

instance LiftLocal () (ExceptT mark e) (Except mark e) where
  liftLocal
    :: (Monad m)
    => Local r m (Except mark e a) -> Local r (ExceptT mark e m) a
  liftLocal local f =
    ExceptT . local f . unExceptT
