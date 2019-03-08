{-# LANGUAGE FlexibleContexts #-}

module Test.Tasty.QuickCheck.Laws.FX.EqIn (
    testEqInLaws

  -- * EqIn Laws
  , testEqInLawReflexive
  , testEqInLawSymmetric
  , testEqInLawTransitive
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

import Control.FX.EqIn



testEqInLaws
  :: ( EqIn t, Typeable t, Typeable a, Eq a
     , Arbitrary (Context t)
     , Arbitrary (t a)
     , Show (Context t), Show (t a)
     )
  => Proxy t -- ^ Constructor under test
  -> Proxy a -- ^ Value type
  -> TestTree
testEqInLaws pt pa =
  let
    label = "EqIn Laws for " ++ (show $ typeRep pt) ++ " with "
      ++ " a = " ++ (show $ typeRep pa)
  in
    testGroup label
      [ testEqInLawReflexive pt pa
      , testEqInLawSymmetric pt pa
      , testEqInLawTransitive pt pa
      ]



-- | @eqIn env x x == True@
testEqInLawReflexive
  :: ( EqIn t, Eq a
     , Arbitrary (Context t), Arbitrary (t a)
     , Show (Context t), Show (t a)
     )
  => Proxy t -- ^ Constructor under test
  -> Proxy a -- ^ Value type
  -> TestTree
testEqInLawReflexive pt pa =
  testProperty "eqIn env a a === True" $
    eqInLawReflexive pt pa

eqInLawReflexive
  :: ( EqIn t, Eq a
     )
  => Proxy t
  -> Proxy a
  -> Context t -> t a -> Bool
eqInLawReflexive _ _ env a =
  eqIn env a a == True



-- | @eqIn env x y == eqIn env y x@
testEqInLawSymmetric
  :: ( EqIn t, Eq a
     , Arbitrary (Context t), Arbitrary (t a)
     , Show (Context t), Show (t a)
     )
  => Proxy t -- ^ Constructor under test
  -> Proxy a -- ^ Value type
  -> TestTree
testEqInLawSymmetric pt pa =
  testProperty "eqIn env a a === True" $
    eqInLawSymmetric pt pa

eqInLawSymmetric
  :: ( EqIn t, Eq a
     )
  => Proxy t
  -> Proxy a
  -> Context t -> t a -> t a -> Bool
eqInLawSymmetric _ _ env a b =
  (eqIn env a b) == (eqIn env b a)



-- | If @eqIn env x y == True@ and @eqIn env y z == True@ then @eqIn env x z == True@.
testEqInLawTransitive
  :: ( EqIn t, Eq a
     , Arbitrary (Context t), Arbitrary (t a)
     , Show (Context t), Show (t a)
     )
  => Proxy t -- ^ Constructor under test
  -> Proxy a -- ^ Value type
  -> TestTree
testEqInLawTransitive pt pa =
  testProperty "eqIn env a a === True" $
    eqInLawTransitive pt pa

eqInLawTransitive
  :: ( EqIn t, Eq a
     )
  => Proxy t
  -> Proxy a
  -> Context t -> t a -> t a -> t a -> Bool
eqInLawTransitive _ _ env a b c =
  if (eqIn env a b) && (eqIn env b c)
    then eqIn env a c else True
