module Control.FX.Monad.Arbitrary where

import Test.QuickCheck (Arbitrary(..), CoArbitrary(..))
import Control.FX.Functor
import Control.FX.Monad

import Control.FX.Functor.Arbitrary

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
  ( Arbitrary a, Arbitrary g
  ) => Arbitrary (Tagged g a)
  where
    arbitrary = Tagged <$> arbitrary <*> arbitrary

instance
  ( Arbitrary a, Arbitrary e
  ) => Arbitrary (Except mark e a)
  where
    arbitrary = do
      q <- arbitrary
      if q
        then Accept <$> arbitrary
        else Except <$> arbitrary
