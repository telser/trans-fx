{-# LANGUAGE MultiParamTypeClasses #-}

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
  :: ( EqIn env a, Typeable env, Typeable a
     , Arbitrary env, Arbitrary a
     , Show env, Show a
     )
  => Proxy env -- ^ Equality context
  -> Proxy a   -- ^ Value type
  -> TestTree
testEqInLaws pEnv pa =
  let
    label = "EqIn Laws for " ++ (show $ typeRep pa) ++ " under " ++
      "env :: " ++ (show $ typeRep pEnv)
  in
    testGroup label
      [ testEqInLawReflexive pEnv pa
      , testEqInLawSymmetric pEnv pa
      , testEqInLawTransitive pEnv pa
      ]



-- | @eqIn env x x == True@
testEqInLawReflexive
  :: ( EqIn env a
     , Arbitrary env, Arbitrary a
     , Show env, Show a
     )
  => Proxy env -- ^ Equality context
  -> Proxy a   -- ^ Value type
  -> TestTree
testEqInLawReflexive pEnv pa =
  testProperty "eqIn env a a === True" $
    eqInLawReflexive pEnv pa

eqInLawReflexive
  :: ( EqIn env a
     )
  => Proxy env
  -> Proxy a
  -> env -> a -> Bool
eqInLawReflexive _ _ env a =
  eqIn env a a == True



-- | @eqIn env x y == eqIn env y x@
testEqInLawSymmetric
  :: ( EqIn env a
     , Arbitrary env, Arbitrary a
     , Show env, Show a
     )
  => Proxy env -- ^ Equality context
  -> Proxy a   -- ^ Value type
  -> TestTree
testEqInLawSymmetric pEnv pa =
  testProperty "eqIn env a a === True" $
    eqInLawSymmetric pEnv pa

eqInLawSymmetric
  :: ( EqIn env a
     )
  => Proxy env
  -> Proxy a
  -> env -> a -> a -> Bool
eqInLawSymmetric _ _ env a b =
  (eqIn env a b) == (eqIn env b a)



-- | If @eqIn env x y == True@ and @eqIn env y z == True@ then @eqIn env x z == True@.
testEqInLawTransitive
  :: ( EqIn env a
     , Arbitrary env, Arbitrary a
     , Show env, Show a
     )
  => Proxy env -- ^ Equality context
  -> Proxy a   -- ^ Value type
  -> TestTree
testEqInLawTransitive pEnv pa =
  testProperty "eqIn env a a === True" $
    eqInLawTransitive pEnv pa

eqInLawTransitive
  :: ( EqIn env a
     )
  => Proxy env
  -> Proxy a
  -> env -> a -> a -> a -> Bool
eqInLawTransitive _ _ env a b c =
  if (eqIn env a b) && (eqIn env b c)
    then eqIn env a c else True
