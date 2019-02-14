{-#
  LANGUAGE
    InstanceSigs,
    KindSignatures,
    FlexibleInstances,
    QuantifiedConstraints,
    MultiParamTypeClasses
#-}

module Control.FX.Monad.Trans.WriterT where

import Data.Typeable (Typeable)
import Control.Applicative (liftA2)

import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans.Class

newtype WriterT
  (k :: * -> *)
  (w :: *)
  (m :: * -> *)
  (a :: *)
    = WriterT
        { unWriterT :: m (Writer k w a)
        } deriving (Typeable)

instance
  ( Show w, Show a
  , forall u. (Show u) => Show (m u)
  ) => Show (WriterT mark w m a)
  where
    show (WriterT x) = "(WriterT " ++ show x ++ ")"

instance
  ( Monoid w, Monad m
  ) => Functor (WriterT mark w m)
  where
    fmap f = WriterT . fmap (fmap f) . unWriterT

instance
  ( Monoid w, Monad m
  ) => Applicative (WriterT mark w m)
  where
    pure :: a -> WriterT mark w m a
    pure = WriterT . pure . pure

    (WriterT f) <*> (WriterT x) =
      WriterT $ (liftA2 (<*>) f x)

instance
  ( Monoid w, Monad m
  ) => Monad (WriterT mark w m)
  where
    return = WriterT . return . return

    (WriterT x) >>= f =
      WriterT $ do
        Writer (Pair w1 a) <- x
        Writer (Pair w2 b) <- unWriterT $ f a
        return $ Writer $ Pair (w1 <> w2) b

instance
  ( Monoid w, Monad m, Central m
  ) => Central (WriterT mark w m)
  where
    commute :: (Applicative f) => WriterT mark w m (f a) -> f (WriterT mark w m a)
    commute = fmap (WriterT) . commute . fmap commute . unWriterT

instance
  ( Monoid w
  ) => MonadTrans (WriterT mark w)
  where
    lift :: (Monad m) => m a -> WriterT mark w m a
    lift x = WriterT $ (x >>= (return . pure))

instance
  ( Monoid w
  ) => MonadFunctor (WriterT mark w)
  where
    hoist f = WriterT . f . unWriterT

instance
  ( Monoid w
  ) => RunMonadTrans () (WriterT mark w) (Pair w)
  where
    runT :: (Monad m) => () -> WriterT mark w m a -> m (Pair w a)
    runT () (WriterT x) = fmap unWriter x

runWriterT
  :: (Monoid w, Monad m)
  => WriterT mark w m a -> m (Pair w a)
runWriterT = runT ()



{- Specialized Lifts -}

instance
  ( Monoid w
  ) => LiftCatch () (WriterT mark w) (Pair w)
  where
    liftCatch
      :: (Monad m)
      => Catch e m (Pair w a) -> Catch e (WriterT mark w m) a
    liftCatch catch x h = WriterT $ fmap Writer $ catch
      (runWriterT x) (\e -> runWriterT $ h e)

instance
  ( Monoid w
  ) => LiftLocal () (WriterT mark w) (Pair w)
  where
    liftLocal
      :: (Monad m)
      => Local r m (Pair w a) -> Local r (WriterT mark w m) a
    liftLocal local f =
      WriterT . fmap Writer . local f . fmap unWriter . unWriterT



{- Effect Class -}

instance
  ( Monoid w, Monad m, MonadIdentity mark
  ) => MonadWriter mark w (WriterT mark w m)
  where
    draft = WriterT . fmap draft . unWriterT

    tell = WriterT . return . tell
