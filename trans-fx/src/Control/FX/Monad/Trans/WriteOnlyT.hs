{-#
  LANGUAGE
    InstanceSigs,
    KindSignatures,
    FlexibleInstances,
    QuantifiedConstraints,
    MultiParamTypeClasses
#-}

module Control.FX.Monad.Trans.WriteOnlyT where

import Data.Typeable (Typeable)
import Control.Applicative (liftA2)

import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans.Class

newtype WriteOnlyT
  (k :: * -> *)
  (w :: *)
  (m :: * -> *)
  (a :: *)
    = WriteOnlyT
        { unWriteOnlyT :: m (WriteOnly k w a)
        } deriving (Typeable)

instance
  ( Show w, Show a
  , forall u. (Show u) => Show (m u)
  ) => Show (WriteOnlyT mark w m a)
  where
    show (WriteOnlyT x) = "(WriteOnlyT " ++ show x ++ ")"

instance
  ( Monoid w, Monad m
  ) => Functor (WriteOnlyT mark w m)
  where
    fmap f = WriteOnlyT . fmap (fmap f) . unWriteOnlyT

instance
  ( Monoid w, Monad m
  ) => Applicative (WriteOnlyT mark w m)
  where
    pure :: a -> WriteOnlyT mark w m a
    pure = WriteOnlyT . pure . pure

    (WriteOnlyT f) <*> (WriteOnlyT x) =
      WriteOnlyT $ (liftA2 (<*>) f x)

instance
  ( Monoid w, Monad m
  ) => Monad (WriteOnlyT mark w m)
  where
    return = WriteOnlyT . return . return

    (WriteOnlyT x) >>= f =
      WriteOnlyT $ do
        WriteOnly (Pair w1 a) <- x
        WriteOnly (Pair w2 b) <- unWriteOnlyT $ f a
        return $ WriteOnly $ Pair (w1 <> w2) b

instance
  ( Monoid w, Central c
  ) => Commutant (WriteOnlyT mark w c)
  where
    commute
      :: ( Applicative f )
      => WriteOnlyT mark w c (f a) -> f (WriteOnlyT mark w c a)
    commute = fmap (WriteOnlyT) . commute . fmap commute . unWriteOnlyT

instance
  ( Monoid w, Central c
  ) => Central (WriteOnlyT mark w c)

instance
  ( Monoid w
  ) => MonadTrans (WriteOnlyT mark w)
  where
    lift :: (Monad m) => m a -> WriteOnlyT mark w m a
    lift x = WriteOnlyT $ (x >>= (return . pure))

instance
  ( Monoid w
  ) => MonadFunctor (WriteOnlyT mark w)
  where
    hoist f = WriteOnlyT . f . unWriteOnlyT

instance
  ( Monoid w
  ) => RunMonadTrans () (WriteOnlyT mark w) (Pair w)
  where
    runT :: (Monad m) => () -> WriteOnlyT mark w m a -> m (Pair w a)
    runT () (WriteOnlyT x) = fmap unWriteOnly x

runWriteOnlyT
  :: (Monoid w, Monad m)
  => WriteOnlyT mark w m a -> m (Pair w a)
runWriteOnlyT = runT ()



{- Specialized Lifts -}

instance
  ( Monoid w
  ) => LiftCatch () (WriteOnlyT mark w) (Pair w)
  where
    liftCatch
      :: (Monad m)
      => Catch e m (Pair w a) -> Catch e (WriteOnlyT mark w m) a
    liftCatch catch x h = WriteOnlyT $ fmap WriteOnly $ catch
      (runWriteOnlyT x) (\e -> runWriteOnlyT $ h e)

instance
  ( Monoid w
  ) => LiftLocal () (WriteOnlyT mark w) (Pair w)
  where
    liftLocal
      :: (Monad m)
      => Local r m (Pair w a) -> Local r (WriteOnlyT mark w m) a
    liftLocal local f =
      WriteOnlyT . fmap WriteOnly . local f . fmap unWriteOnly . unWriteOnlyT



{- Effect Class -}

instance
  ( Monoid w, Monad m, MonadIdentity mark
  ) => MonadWriteOnly mark w (WriteOnlyT mark w m)
  where
    draft = WriteOnlyT . fmap draft . unWriteOnlyT

    tell = WriteOnlyT . return . tell
