{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module Test.Tasty.QuickCheck.Laws.FX.Data.Stack (
    testStackMonadLaws

  -- * Stack Monad Laws
  , testStackMonadLawPushPop
) where



import Control.Monad
  ( liftM2 )
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
import Control.FX.Data


-- | Constructs a @TestTree@ checking that the stack monad laws hold for @m@ with data type @d@, stack functor type @f@, and value types @a@ and @b@, using a given equality test for values of type @forall u. m u@. The equality context type @t@ is for constructors @m@ from which we can only extract a value within a context, such as reader-like constructors.
testStackMonadLaws
  :: ( Monad m, IsStack f
     , Eq d, Eq a
     , Show t, Show d
     , Show (m a)
     , Arbitrary t, Arbitrary d, Arbitrary a
     , Typeable m, Typeable f, Typeable a, Typeable d
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy f -- ^ Stack functor type
  -> Proxy d -- ^ Stack data type
  -> Proxy a -- ^ Value type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (Proxy f -> d -> m ()) -- ^ @push@
  -> (Proxy f -> m (Maybe d)) -- ^ @pop@
  -> TestTree
testStackMonadLaws pm pt pf pd pa eq push pop =
  let
    label = "Stack Monad Laws for " ++ (show $ typeRep pm) ++ " with " ++
      "f :: " ++ (show $ typeRep pf) ++ ", " ++
      "d :: " ++ (show $ typeRep pd) ++ ", " ++
      "a :: " ++ (show $ typeRep pa)
  in
    testGroup label
      [ testStackMonadLawPushPop pm pt pf pd eq push pop
      ]



-- | @push p d >> pop p  ===  return (Just d)@
testStackMonadLawPushPop
  :: ( Monad m, IsStack f
     , Eq d
     , Show t, Show d
     , Arbitrary t, Arbitrary d
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy f -- ^ Stack functor type
  -> Proxy d -- ^ Stack data type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (Proxy f -> d -> m ()) -- ^ @push@
  -> (Proxy f -> m (Maybe d)) -- ^ @pop@
  -> TestTree
testStackMonadLawPushPop pm pt pf pd eq push pop =
  testProperty "push p d >> pop p  ===  return (Just d)" $
    stackMonadLawPushPop pm pt pf pd eq push pop

stackMonadLawPushPop
  :: (Monad m, Eq d, IsStack f)
  => Proxy m -> Proxy t -> Proxy f -> Proxy d
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (Proxy f -> d -> m ()) -- ^ @push@
  -> (Proxy f -> m (Maybe d)) -- ^ @pop@
  -> t -> d -> Bool
stackMonadLawPushPop _ _ pf _ eq push pop t d =
  (eq t) (push pf d >> pop pf) (return (Just d))
