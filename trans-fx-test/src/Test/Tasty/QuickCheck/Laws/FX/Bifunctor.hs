{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Test.Tasty.QuickCheck.Laws.FX.Bifunctor (
    testBifunctorLaws

  -- * Bifunctor Laws
  , testBifunctorLawIdentityLeft
  , testBifunctorLawIdentityRight
  , testBifunctorLawHomomorphismLeft
  , testBifunctorLawHomomorphismRight
  , testBifunctorLawCommutative
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




testBifunctorLaws
  :: ( Bifunctor f, Typeable f
     , Arbitrary a, Arbitrary (f a b), Arbitrary t, Arbitrary c, Arbitrary d
     , CoArbitrary a, CoArbitrary b, CoArbitrary c
     , Show a, Show (f a b), Show t
     , Eq a, Eq b, Eq c, Eq d
     , Typeable f, Typeable a, Typeable b, Typeable c, Typeable d
     )
  => Proxy f -- ^ Constructor under test
  -> Proxy t -- ^ Equality context for @f@
  -> Proxy a -- ^ Value type
  -> Proxy b -- ^ Value type
  -> Proxy c -- ^ Value type
  -> Proxy d -- ^ Value type
  -> (forall u v. (Eq u, Eq v) => t -> f u v -> f u v -> Bool) -- ^ Equality test
  -> TestTree
testBifunctorLaws pf pt pa pb pc pd eq =
  let
    label = "Bifunctor Laws for " ++ (show $ typeRep pa) ++ " with " ++
      "a :: " ++ (show $ typeRep pa) ++
      "b :: " ++ (show $ typeRep pb) ++
      "c :: " ++ (show $ typeRep pc) ++
      "d :: " ++ (show $ typeRep pd)
  in
    testGroup label
      [ testBifunctorLawIdentityLeft pf pt pa pb eq
      , testBifunctorLawIdentityRight pf pt pa pb eq
      , testBifunctorLawHomomorphismLeft pf pt pa pb pc pd eq
      , testBifunctorLawHomomorphismRight pf pt pa pb pc pd eq
      , testBifunctorLawCommutative pf pt pa pb pc pd eq
      ]



-- | @bimap1 id === id@
testBifunctorLawIdentityLeft
  :: ( Bifunctor f
     , Arbitrary (f a b), Arbitrary t
     , Show (f a b), Show t
     , Eq a, Eq b
     )
  => Proxy f -- ^ Constructor under test
  -> Proxy t -- ^ Equality context for @f@
  -> Proxy a -- ^ Value type
  -> Proxy b -- ^ Value type
  -> (forall u v. (Eq u, Eq v) => t -> f u v -> f u v -> Bool) -- ^ Equality test
  -> TestTree
testBifunctorLawIdentityLeft pf pt pa pb eq =
  testProperty "bimap1 id === id" $
    bifunctorLawIdentityLeft pf pt pa pb eq

bifunctorLawIdentityLeft
  :: ( Bifunctor f
     , Arbitrary (f a b), Arbitrary t
     , Show (f a b), Show t
     , Eq a, Eq b
     )
  => Proxy f
  -> Proxy t
  -> Proxy a
  -> Proxy b
  -> (forall u v. (Eq u, Eq v) => t -> f u v -> f u v -> Bool) -- ^ Equality test
  -> t -> f a b -> Bool
bifunctorLawIdentityLeft _ _ _ _ eq t x =
  (eq t) (bimap1 id x) (x)



-- | @bimap2 id === id@
testBifunctorLawIdentityRight
  :: ( Bifunctor f
     , Arbitrary (f a b), Arbitrary t
     , Show (f a b), Show t
     , Eq a, Eq b
     )
  => Proxy f -- ^ Constructor under test
  -> Proxy t -- ^ Equality context for @f@
  -> Proxy a -- ^ Value type
  -> Proxy b -- ^ Value type
  -> (forall u v. (Eq u, Eq v) => t -> f u v -> f u v -> Bool) -- ^ Equality test
  -> TestTree
testBifunctorLawIdentityRight pf pt pa pb eq =
  testProperty "bimap2 id === id" $
    bifunctorLawIdentityRight pf pt pa pb eq

bifunctorLawIdentityRight
  :: ( Bifunctor f
     , Arbitrary (f a b), Arbitrary t
     , Show (f a b), Show t
     , Eq a, Eq b
     )
  => Proxy f
  -> Proxy t
  -> Proxy a
  -> Proxy b
  -> (forall u v. (Eq u, Eq v) => t -> f u v -> f u v -> Bool) -- ^ Equality test
  -> t -> f a b -> Bool
bifunctorLawIdentityRight _ _ _ _ eq t x =
  (eq t) (bimap2 id x) (x)



-- | @bimap1 g . bimap1 f === bimap1 (g . f)@
testBifunctorLawHomomorphismLeft
  :: ( Bifunctor f
     , Arbitrary (f a b), Arbitrary t, Arbitrary c, Arbitrary d
     , CoArbitrary a, CoArbitrary c
     , Show (f a b), Show t
     , Eq a, Eq b, Eq d
     )
  => Proxy f -- ^ Constructor under test
  -> Proxy t -- ^ Equality context for @f@
  -> Proxy a -- ^ Value type
  -> Proxy b -- ^ Value type
  -> Proxy c -- ^ Value type
  -> Proxy d -- ^ Value type
  -> (forall u v. (Eq u, Eq v) => t -> f u v -> f u v -> Bool) -- ^ Equality test
  -> TestTree
testBifunctorLawHomomorphismLeft pf pt pa pb pc pd eq =
  testProperty "bimap1 g . bimap1 f === bimap1 (g . f)" $
    bifunctorLawHomomorphismLeft pf pt pa pb pc pd eq

bifunctorLawHomomorphismLeft
  :: ( Bifunctor f
     , Arbitrary (f a b), Arbitrary t, Arbitrary c, Arbitrary d
     , CoArbitrary a, CoArbitrary c
     , Show (f a b), Show t
     , Eq a, Eq b, Eq d
     )
  => Proxy f
  -> Proxy t
  -> Proxy a
  -> Proxy b
  -> Proxy c -- ^ Value type
  -> Proxy d -- ^ Value type
  -> (forall u v. (Eq u, Eq v) => t -> f u v -> f u v -> Bool) -- ^ Equality test
  -> t -> f a b -> (a -> c) -> (c -> d) -> Bool
bifunctorLawHomomorphismLeft _ _ _ _ _ _ eq t x f g =
  (eq t) (bimap1 g $ bimap1 f x) (bimap1 (g . f) x)



-- | @bimap2 g . bimap2 f === bimap2 (g . f)@
testBifunctorLawHomomorphismRight
  :: ( Bifunctor f
     , Arbitrary (f a b), Arbitrary t, Arbitrary c, Arbitrary d
     , CoArbitrary b, CoArbitrary c
     , Show (f a b), Show t
     , Eq a, Eq b, Eq d
     )
  => Proxy f -- ^ Constructor under test
  -> Proxy t -- ^ Equality context for @f@
  -> Proxy a -- ^ Value type
  -> Proxy b -- ^ Value type
  -> Proxy c -- ^ Value type
  -> Proxy d -- ^ Value type
  -> (forall u v. (Eq u, Eq v) => t -> f u v -> f u v -> Bool) -- ^ Equality test
  -> TestTree
testBifunctorLawHomomorphismRight pf pt pa pb pc pd eq =
  testProperty "bimap2 g . bimap2 f === bimap2 (g . f)" $
    bifunctorLawHomomorphismRight pf pt pa pb pc pd eq

bifunctorLawHomomorphismRight
  :: ( Bifunctor f
     , Arbitrary (f a b), Arbitrary t, Arbitrary c, Arbitrary d
     , CoArbitrary b, CoArbitrary c
     , Show (f a b), Show t
     , Eq a, Eq b, Eq d
     )
  => Proxy f
  -> Proxy t
  -> Proxy a
  -> Proxy b
  -> Proxy c -- ^ Value type
  -> Proxy d -- ^ Value type
  -> (forall u v. (Eq u, Eq v) => t -> f u v -> f u v -> Bool) -- ^ Equality test
  -> t -> f a b -> (b -> c) -> (c -> d) -> Bool
bifunctorLawHomomorphismRight _ _ _ _ _ _ eq t x f g =
  (eq t) (bimap2 g $ bimap2 f x) (bimap2 (g . f) x)



-- | @bimap1 f . bimap2 g === bimap2 g . bimap1 f@
testBifunctorLawCommutative
  :: ( Bifunctor f
     , Arbitrary (f a b), Arbitrary t, Arbitrary c, Arbitrary d
     , CoArbitrary a, CoArbitrary b
     , Show (f a b), Show t
     , Eq a, Eq b, Eq c, Eq d
     )
  => Proxy f -- ^ Constructor under test
  -> Proxy t -- ^ Equality context for @f@
  -> Proxy a -- ^ Value type
  -> Proxy b -- ^ Value type
  -> Proxy c -- ^ Value type
  -> Proxy d -- ^ Value type
  -> (forall u v. (Eq u, Eq v) => t -> f u v -> f u v -> Bool) -- ^ Equality test
  -> TestTree
testBifunctorLawCommutative pf pt pa pb pc pd eq =
  testProperty "bimap1 f . bimap2 g === bimap2 g . bimap1 f" $
    bifunctorLawCommutative pf pt pa pb pc pd eq

bifunctorLawCommutative
  :: ( Bifunctor f
     , Arbitrary (f a b), Arbitrary t, Arbitrary c, Arbitrary d
     , CoArbitrary a, CoArbitrary b
     , Show (f a b), Show t
     , Eq a, Eq b, Eq c, Eq d
     )
  => Proxy f
  -> Proxy t
  -> Proxy a
  -> Proxy b
  -> Proxy c -- ^ Value type
  -> Proxy d -- ^ Value type
  -> (forall u v. (Eq u, Eq v) => t -> f u v -> f u v -> Bool) -- ^ Equality test
  -> t -> f a b -> (a -> c) -> (b -> d) -> Bool
bifunctorLawCommutative _ _ _ _ _ _ eq t x f g =
  (eq t) (bimap1 f $ bimap2 g x) (bimap2 g $ bimap1 f x)
