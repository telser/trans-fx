{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Control.FX.Arbitrary () where

import Test.QuickCheck (Arbitrary(..), CoArbitrary(..), Gen)

import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans
import Control.FX.Monad.Trans.Trans

instance
  ( Arbitrary a, Arbitrary b
  ) => Arbitrary (Pair a b)
  where
    arbitrary = Pair <$> arbitrary <*> arbitrary

instance
  ( Arbitrary a
  ) => Arbitrary (LeftZero a)
  where
    arbitrary = do
      p <- arbitrary
      if p
        then return LeftUnit
        else LeftZero <$> arbitrary

instance
  ( Arbitrary a
  ) => Arbitrary (RightZero a)
  where
    arbitrary = do
      p <- arbitrary
      if p
        then return RightUnit
        else RightZero <$> arbitrary

instance
  ( Arbitrary a
  ) => Arbitrary (Identity a)
  where
    arbitrary = Identity <$> arbitrary

instance
  ( CoArbitrary a
  ) => CoArbitrary (Identity a)
  where
    coarbitrary (Identity x) gen =
      coarbitrary x gen

instance
  ( Arbitrary a, Arbitrary s, CoArbitrary s
  ) => Arbitrary (State mark s a)
  where
    arbitrary = State <$> arbitrary

instance
  ( Arbitrary a, CoArbitrary r
  ) => Arbitrary (ReadOnly mark r a)
  where
    arbitrary = ReadOnly <$> arbitrary

instance
  ( Arbitrary a, Arbitrary w, Monoid w
  ) => Arbitrary (WriteOnly mark w a)
  where
    arbitrary = WriteOnly <$> arbitrary

instance
  ( Arbitrary a, Arbitrary e
  ) => Arbitrary (Except mark e a)
  where
    arbitrary = do
      q <- arbitrary
      if q
        then Accept <$> arbitrary
        else Except <$> arbitrary

instance
  ( Arbitrary (m a), Functor m
  ) => Arbitrary (IdentityT m a)
  where
    arbitrary = IdentityT <$> arbitrary

instance
  ( Arbitrary (m a), CoArbitrary r, Functor m
  ) => Arbitrary (ReadOnlyT mark r m a)
  where
    arbitrary = do
      f <- arbitrary
      return (ReadOnlyT $ ReadOnly f)

instance
  ( Arbitrary (m (Pair s a)), CoArbitrary s
  ) => Arbitrary (StateT mark s m a)
  where
    arbitrary = StateT <$> arbitrary

instance
  ( Arbitrary (m (WriteOnly mark w a)), Functor m
  ) => Arbitrary (WriteOnlyT mark w m a)
  where
    arbitrary = WriteOnlyT <$> arbitrary

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
  ( Arbitrary (m a), Functor m, MonadIdentity mark
  ) => Arbitrary (HaltT mark m a)
  where
    arbitrary = do
      x <- arbitrary
      p <- arbitrary
      return $ HaltT $
        fmap (if p then Step else const Halt) x

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

instance Arbitrary (Unit m) where
  arbitrary = return Unit

instance (Arbitrary (a m), Arbitrary b) => Arbitrary (Sing a b m) where
  arbitrary = Sing <$> arbitrary <*> arbitrary

instance
  ( Arbitrary (t m a)
  ) => Arbitrary (IdentityTT t m a)
  where
    arbitrary = IdentityTT <$> arbitrary

instance
  ( Monad m
  ) => Arbitrary (Eval Identity m)
  where
    arbitrary = do
      return $ Eval (return . unIdentity)

instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , Arbitrary (mark (p a)), Arbitrary (t m a)
  ) => Arbitrary (PromptTT mark p t m a)
  where
    arbitrary = do
      w <- arbitrary
      if w
        then do
          p <- arbitrary :: Gen (mark (p a))
          return $ fmap unwrap $ prompt p
        else liftT <$> arbitrary

instance
  ( Monad m, MonadTrans t, MonadTrans w, MonadTransTrans u, Arbitrary (w (u t m) a)
  ) => Arbitrary (OverTT u w t m a)
  where
    arbitrary = OverTT <$> arbitrary


