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
