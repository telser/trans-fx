{-#
  LANGUAGE
    GADTs,
    Rank2Types,
    KindSignatures,
    FlexibleInstances,
    UndecidableInstances,
    QuantifiedConstraints,
    MultiParamTypeClasses
#-}

module Control.FX.Monad.Trans.Trans.EqIn where

import Test.QuickCheck (Arbitrary(..))

import Control.FX
import Control.FX.EqIn.Class

instance Arbitrary (Unit m) where
  arbitrary = return Unit

instance
  ( EqIn h (t m a)
  ) => EqIn (Unit m, h) (IdentityTT t m a)
  where
    eqIn (Unit, h) (IdentityTT x) (IdentityTT y) = eqIn h x y

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, Commutant mark, EqIn h (t m (mark a))
  ) => EqIn (Eval p m, h) (PromptTT mark p t m a)
  where
    eqIn (eval, h) x y =
      eqIn h (runTT eval x) (runTT eval y)

instance (Arbitrary (a m), Arbitrary b) => Arbitrary (Sing a b m) where
  arbitrary = Sing <$> arbitrary <*> arbitrary

instance
  ( Monad m, MonadTrans t, MonadFunctor w, MonadTransTrans u
  , Eq a, RunMonadTransTrans z1 u f1, RunMonadTrans z2 w f2
  , forall x. (Eq x) => Eq (f1 (f2 x))
  , forall x. (Eq x) => EqIn h (t m x)
  ) => EqIn (Sing z1 z2 m, h) (OverTT u w t m a)
  where
    eqIn (k,h) x y =
      eqIn h (runTT k x) (runTT k y)
