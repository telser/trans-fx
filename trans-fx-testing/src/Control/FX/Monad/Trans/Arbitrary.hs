{-#
  LANGUAGE
    UndecidableInstances
#-}

module Control.FX.Monad.Trans.Arbitrary where

import Test.QuickCheck (Arbitrary(..), CoArbitrary(..))
import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans

import Control.FX.Functor.Arbitrary

instance
  ( Arbitrary (m a), Functor m
  ) => Arbitrary (IdentityT m a)
  where
    arbitrary = IdentityT <$> arbitrary

instance
  ( Arbitrary (m a), CoArbitrary r, Functor m
  ) => Arbitrary (ReaderT mark r m a)
  where
    arbitrary = do
      f <- arbitrary
      return (ReaderT $ Reader f)

instance
  ( Arbitrary (m (Pair s a)), CoArbitrary s
  ) => Arbitrary (StateT mark s m a)
  where
    arbitrary = StateT <$> arbitrary

instance
  ( Arbitrary (m (Writer mark w a)), Functor m
  ) => Arbitrary (WriterT mark w m a)
  where
    arbitrary = WriterT <$> arbitrary

instance
  ( Arbitrary (m a), Arbitrary e, Functor m
  ) => Arbitrary (ExceptT mark e m a)
  where
    arbitrary = do
      x <- arbitrary
      p <- arbitrary
      if p
        then return $ ExceptT $ fmap Accept x
        else do
          e <- arbitrary
          return $ ExceptT $ fmap (const (Except e)) x

instance
  ( Arbitrary (m a), Functor m
  ) => Arbitrary (MaybeT m a)
  where
    arbitrary = do
      x <- arbitrary
      p <- arbitrary
      return $ MaybeT $
        fmap (if p then Just else const Nothing) x

instance
  ( Arbitrary (t1 (t2 m) a)
  ) => Arbitrary (ComposeT t1 t2 m a)
  where
    arbitrary = ComposeT <$> arbitrary

instance
  ( Arbitrary (t m a)
  ) => Arbitrary (ApplyT t m a)
  where
    arbitrary = ApplyT <$> arbitrary
