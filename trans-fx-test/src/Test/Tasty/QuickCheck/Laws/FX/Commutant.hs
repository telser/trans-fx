{-# LANGUAGE RankNTypes #-}

module Test.Tasty.QuickCheck.Laws.FX.Commutant (
    testCommutantLaws

  -- * Commutant Laws
  , testCommutantLawPureLeft
) where

import Data.Proxy
  ( Proxy(..) )
import Data.Typeable
  ( Typeable, typeRep )
import Test.Tasty
  ( TestTree, testGroup )
import Test.Tasty.QuickCheck
  ( testProperty, Property, Arbitrary(..), CoArbitrary(..) )
import Text.Show.Functions
  ()
import Test.Tasty.QuickCheck.Laws.Class

import Control.FX



testCommutantLaws
  :: ( Commutant c, Applicative f, Eq a
     , Arbitrary w, Arbitrary (c a)
     , Show w, Show (c a)
     , Typeable a, Typeable c, Typeable f
     )
  => Proxy c -- ^ Type constructor under test
  -> Proxy f -- ^ Inner monad
  -> Proxy w -- ^ Equality context for @t m@
  -> Proxy a -- ^ Value type
  -> (forall u. (Eq u) => w -> f (c u) -> f (c u) -> Bool) -- ^ Equality test
  -> TestTree
testCommutantLaws pc pf pw pa eq =
  let
    label = "Commutant Laws for " ++ (show $ typeRep pc) ++ " with " ++
      "f :: " ++ (show $ typeRep pf) ++ ", " ++
      "a :: " ++ (show $ typeRep pa)
  in
    testGroup label
      [ testCommutantLawPureLeft pc pf pw pa eq
      ]



-- | @commute . fmap pure === pure@
testCommutantLawPureLeft
  :: ( Commutant c, Applicative f, Eq a
     , Arbitrary w, Arbitrary (c a)
     , Show w, Show (c a)
     )
  => Proxy c -- ^ Type constructor under test
  -> Proxy f -- ^ Inner monad
  -> Proxy w -- ^ Equality context for @t m@
  -> Proxy a -- ^ Value type
  -> (forall u. (Eq u) => w -> f (c u) -> f (c u) -> Bool) -- ^ Equality test
  -> TestTree
testCommutantLawPureLeft pf pm pw pa eq =
  testProperty "commute . fmap pure === pure" $
    commutantLawPureLeft pf pm pw pa eq

commutantLawPureLeft
  :: (Commutant c, Applicative f, Eq a)
  => Proxy c -> Proxy f -> Proxy w -> Proxy a
  -> (forall u. (Eq u) => w -> f (c u) -> f (c u) -> Bool) -- ^ Equality test
  -> w -> c a -> Bool
commutantLawPureLeft _ _ _ _ eq w x =
  (eq w) (commute . fmap pure $ x) (pure $ x)
