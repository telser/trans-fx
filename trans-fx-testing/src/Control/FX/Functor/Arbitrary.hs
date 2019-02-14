module Control.FX.Functor.Arbitrary where

import Test.QuickCheck (Arbitrary(..))

import Control.FX.Functor

instance
  ( Arbitrary a, Arbitrary b
  ) => Arbitrary (Pair a b)
  where
    arbitrary = Pair <$> arbitrary <*> arbitrary

instance
  ( Arbitrary g
  ) => Arbitrary (Tag g)
  where
    arbitrary = Tag <$> arbitrary
