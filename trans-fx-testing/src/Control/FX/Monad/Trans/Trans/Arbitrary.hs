{-#
  LANGUAGE
    FlexibleInstances,
    ScopedTypeVariables,
    UndecidableInstances,
    QuantifiedConstraints
#-}

module Control.FX.Monad.Trans.Trans.Arbitrary where

import Test.QuickCheck (Arbitrary(..), CoArbitrary(..), Gen)

import Control.FX.Monad
import Control.FX.Monad.Trans
import Control.FX.Monad.Trans.Trans

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
  ( Monad m, MonadTrans t, Arbitrary (p a), Arbitrary (t m a)
  ) => Arbitrary (PromptTT p t m a)
  where
    arbitrary = do
      w <- arbitrary
      if w
        then prompt <$> arbitrary
        else liftT <$> arbitrary

instance
  ( Monad m, MonadTrans t, MonadTrans w, MonadTransTrans u, Arbitrary (w (u t m) a)
  ) => Arbitrary (OverTT u w t m a)
  where
    arbitrary = OverTT <$> arbitrary
