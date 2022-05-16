{-# LANGUAGE RankNTypes #-}

module Test.Tasty.QuickCheck.Laws.FX.Central (
    testCentralLaws

  -- * Central Laws
  , testCentralLawJoinLeft
  , testCentralLawJoinRight
  , testCentralLawReturnRight
) where

import Control.Monad
  ( join )
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



-- | @commute . fmap join === join . fmap commute . commute@
testCentralLawJoinLeft
  :: ( Central c, Monad m, Eq a
     , Arbitrary w, Arbitrary (c (m (m a)))
     , Show w, Show (c (m (m a)))
     )
  => Proxy c -- ^ Type constructor under test
  -> Proxy m -- ^ Inner monad
  -> Proxy w -- ^ Equality context for @t m@
  -> Proxy a -- ^ Value type
  -> (forall u. (Eq u) => w -> m (c u) -> m (c u) -> Bool) -- ^ Equality test
  -> TestTree
testCentralLawJoinLeft pf pm pw pa eq =
  testProperty "commute . fmap join === join . fmap commute . commute" $
    centralLawJoinLeft pf pm pw pa eq

centralLawJoinLeft
  :: (Central c, Monad m, Eq a)
  => Proxy c -> Proxy m -> Proxy w -> Proxy a
  -> (forall u. (Eq u) => w -> m (c u) -> m (c u) -> Bool) -- ^ Equality test
  -> w -> c (m (m a)) -> Bool
centralLawJoinLeft _ _ _ _ eq w x =
  (eq w) (commute . fmap join $ x) (join . fmap commute . commute $ x)



-- | @commute . join === fmap join . commute . fmap commute@
testCentralLawJoinRight
  :: ( Central c, Monad m, Eq a
     , Arbitrary w, Arbitrary (c (c (m a)))
     , Show w, Show (c (c (m a)))
     )
  => Proxy c -- ^ Type constructor under test
  -> Proxy m -- ^ Inner monad
  -> Proxy w -- ^ Equality context for @t m@
  -> Proxy a -- ^ Value type
  -> (forall u. (Eq u) => w -> m (c u) -> m (c u) -> Bool) -- ^ Equality test
  -> TestTree
testCentralLawJoinRight pf pm pw pa eq =
  testProperty "commute . join === fmap join . commute . fmap commute" $
    centralLawJoinRight pf pm pw pa eq

centralLawJoinRight
  :: (Central c, Monad m, Eq a)
  => Proxy c -> Proxy m -> Proxy w -> Proxy a
  -> (forall u. (Eq u) => w -> m (c u) -> m (c u) -> Bool) -- ^ Equality test
  -> w -> c (c (m a)) -> Bool
centralLawJoinRight _ _ _ _ eq w x =
  (eq w) (commute . join $ x) (fmap join . commute . fmap commute $ x)



-- | @commute . return === fmap return@
testCentralLawReturnRight
  :: ( Central c, Monad m, Eq a
     , Arbitrary w, Arbitrary (m a)
     , Show w, Show (m a)
     )
  => Proxy c -- ^ Type constructor under test
  -> Proxy m -- ^ Inner monad
  -> Proxy w -- ^ Equality context for @t m@
  -> Proxy a -- ^ Value type
  -> (forall u. (Eq u) => w -> m (c u) -> m (c u) -> Bool) -- ^ Equality test
  -> TestTree
testCentralLawReturnRight pf pm pw pa eq =
  testProperty "commute . return === fmap return" $
    centralLawReturnRight pf pm pw pa eq

centralLawReturnRight
  :: (Central c, Monad m, Eq a)
  => Proxy c -> Proxy m -> Proxy w -> Proxy a
  -> (forall u. (Eq u) => w -> m (c u) -> m (c u) -> Bool) -- ^ Equality test
  -> w -> m a -> Bool
centralLawReturnRight _ _ _ _ eq w x =
  (eq w) (commute . return $ x) (fmap return $ x)



-- | Constructs a @TestTree@ checking that the @Central@ class laws hold for @c@ over monad @m@ with value type @a@, using the given equality test for values of type @forall u. m (c u)@. The equality context @w@ is for constructors from which we can only extract a value within a context, such as reader-like constructors.
testCentralLaws
  :: ( Central c, Monad m
     , Eq a
     , Show w, Show a, Show (m a)
     , Show (c (c (m a))), Show (c (m (m a)))
     , Arbitrary w, Arbitrary a, Arbitrary (m a)
     , Arbitrary (c (c (m a))), Arbitrary (c (m (m a)))
     , Typeable m, Typeable a, Typeable c
     )
  => Proxy c -- ^ Constructor under test
  -> Proxy m -- ^ Inner monad
  -> Proxy w -- ^ Equality context for @t m@
  -> Proxy a -- ^ Value type
  -> (forall u. (Eq u) => w -> m (c u) -> m (c u) -> Bool) -- ^ Equality test
  -> TestTree
testCentralLaws pc pm pw pa eq =
  let
    label = "Central Laws for " ++ (show $ typeRep pc) ++ " with " ++
      "m :: " ++ (show $ typeRep pm) ++ ", " ++
      "a :: " ++ (show $ typeRep pa)
  in
    testGroup label
      [ testCentralLawJoinLeft pc pm pw pa eq
      , testCentralLawJoinRight pc pm pw pa eq
      , testCentralLawReturnRight pc pm pw pa eq
      ]
