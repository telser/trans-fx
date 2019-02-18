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

import Control.FX.Monad.Unit
import Control.FX.Monad.Trans
import Control.FX.Monad.Trans.Trans
import Control.FX.EqIn.Class

instance Arbitrary (Unit m) where
  arbitrary = return Unit

instance
  ( EqIn h (t m a)
  ) => EqIn (Unit m, h) (IdentityTT t m a)
  where
    eqIn (Unit, h) (IdentityTT x) (IdentityTT y) = eqIn h x y

instance Show (Eval p m) where
  show _ = "Eval"

instance
  ( Monad m, MonadTrans t, EqIn h (t m a)
  ) => EqIn (Eval p m, h) (PromptTT mark p t m a)
  where
    eqIn (Eval eval, h) x y =
      eqIn h (runPromptTT eval x) (runPromptTT eval y)

instance Show (Sing a b m) where
  show _ = "Sing"

instance (Arbitrary (a m), Arbitrary b) => Arbitrary (Sing a b m) where
  arbitrary = Sing <$> arbitrary <*> arbitrary

instance
  ( Monad m, MonadTrans t, MonadFunctor w, MonadTransTrans u, Eq a
  , RunMonadTransTrans z1 u f1, RunMonadTrans z2 w f2
  , forall x. (Eq x) => Eq (f1 (f2 x))
  , forall x. (Eq x) => EqIn h (t m x)
  ) => EqIn (Trip z1 z2 m, h) (OverTT u w t m a)
  where
    eqIn (Trip z1 z2, h) x y =
      eqIn h (runOverTT z1 z2 x) (runOverTT z1 z2 y)

data Trip
  (z :: (* -> *) -> *)
  (a :: *)
  (m :: * -> *)
    = Trip (z m) a

instance Show (Trip z a m) where
  show _ = "Trip"

instance
  ( Arbitrary a, Arbitrary (z m)
  ) => Arbitrary (Trip z a m)
  where
    arbitrary = Trip <$> arbitrary <*> arbitrary
