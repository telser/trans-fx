{-# LANGUAGE Rank2Types #-}

module Test.Tasty.QuickCheck.Laws.FX.MonadTrans (
    testMonadTransLaws

  -- * MonadTrans Laws
  , testMonadTransLawIdentity
  , testMonadTransLawHomomorphism

  -- * TestTrees
  , testMonadTransLaws1
  , testMonadTransLaws2
  , testMonadTransLaws3
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

import Control.FX.Monad.Trans.Class



-- | @lift . return === return@
testMonadTransLawIdentity
  :: ( MonadTrans t, Monad m, Eq a
     , Arbitrary w, Arbitrary a
     , Show w, Show a
     )
  => Proxy t -- ^ Type constructor under test
  -> Proxy m -- ^ Inner monad
  -> Proxy w -- ^ Equality context for @t m@
  -> Proxy a -- ^ Value type
  -> (forall u. (Eq u) => w -> t m u -> t m u -> Bool) -- ^ Equality test
  -> TestTree
testMonadTransLawIdentity pt pm pw pa eq =
  testProperty "lift . return === return" $
    monadTransLawIdentity pt pm pw pa eq

monadTransLawIdentity
  :: (MonadTrans t, Monad m, Eq a)
  => Proxy t -> Proxy m -> Proxy w -> Proxy a
  -> (forall u. (Eq u) => w -> t m u -> t m u -> Bool) -- ^ Equality test
  -> w -> a -> Bool
monadTransLawIdentity _ _ _ _ eq w a =
  (eq w) (lift $ return a) (return a)



-- | @lift (x >>= f) == lift x >>= (lift . f)@
testMonadTransLawHomomorphism
  :: ( MonadTrans t, Monad m, Eq b
     , Show w, Show (m a)
     , Arbitrary w, CoArbitrary a, Arbitrary (m a), Arbitrary (m b)
     )
  => Proxy t -- ^ Type constructor under test
  -> Proxy m -- ^ Inner monad
  -> Proxy w -- ^ Equality context for @t m@
  -> Proxy a -- ^ Value type
  -> Proxy b -- ^ Value type
  -> (forall u. (Eq u) => w -> t m u -> t m u -> Bool)
  -> TestTree
testMonadTransLawHomomorphism pt pm pw pa pb eq =
  testProperty "lift (x >>= f) === lift x >>= (lift . f)" $
    monadTransLawHomomorphism pt pm pw pa pb eq

monadTransLawHomomorphism
  :: (MonadTrans t, Monad m, Eq b)
  => Proxy t -> Proxy m -> Proxy w -> Proxy a -> Proxy b
  -> (forall u. (Eq u) => w -> t m u -> t m u -> Bool) -- ^ Equality test
  -> w -> m a -> (a -> m b) -> Bool
monadTransLawHomomorphism _ _ _ _ _ eq w x f =
  (eq w) (lift (x >>= f)) (lift x >>= (lift . f))



-- | Constructs a @TestTree@ checking that the @MonadTrans@ class laws hold for @t@ with inner monad @m@ and value types @a@ and @b@, using the given equality test for values of type @forall u. t m u@. The equality context @w@ is for constructors @t m@ from which we can only extract a value within a context, such as reader-like constructors.
testMonadTransLaws
  :: ( MonadTrans t, Monad m
     , Eq a, Eq b
     , Show w, Show a, Show (m a)
     , Arbitrary w, Arbitrary a, CoArbitrary a
     , Arbitrary (m a), Arbitrary (m b)
     , Typeable t, Typeable m, Typeable a, Typeable b
     )
  => Proxy t -- ^ Constructor under test
  -> Proxy m -- ^ Inner monad
  -> Proxy w -- ^ Equality context for @t m@
  -> Proxy a -- ^ Value type
  -> Proxy b -- ^ Value type
  -> (forall u. (Eq u) => w -> t m u -> t m u -> Bool) -- ^ Equality test
  -> TestTree
testMonadTransLaws pt pm pw pa pb eq =
  let
    label = "MonadTrans Laws for " ++ (show $ typeRep pt) ++ " on " ++
      "m :: " ++ (show $ typeRep pm) ++ " with " ++
      "a :: " ++ (show $ typeRep pa) ++ ", " ++
      "b :: " ++ (show $ typeRep pb)
  in
    testGroup label
      [ testMonadTransLawIdentity pt pm pw pa eq
      , testMonadTransLawHomomorphism pt pm pw pa pb eq
      ]



-- | All possible value type selections for @testMonadTransLaws@ from one choice
testMonadTransLaws1
  :: ( MonadTrans t, Monad m
     , Checkable a
     , Show w, Show (m a)
     , Arbitrary w, Arbitrary a, CoArbitrary a
     , Arbitrary (m a)
     , Typeable t, Typeable m
     )
  => Proxy t -- ^ Constructor under test
  -> Proxy m -- ^ Inner monad
  -> Proxy w -- ^ Equality context for @t m@
  -> Proxy a -- ^ Value type
  -> (forall u. (Eq u) => w -> t m u -> t m u -> Bool) -- ^ Equality test
  -> TestTree
testMonadTransLaws1 pt pm pw pa eq =
  let label = "MonadTrans Laws for " ++ (show $ typeRep pt) in
  testGroup label
    [ testMonadTransLaws pt pm pw pa pa eq
    ]



-- | All possible value type selections for @testMonadTransLaws@ from two choices
testMonadTransLaws2
  :: ( MonadTrans t, Monad m
     , Checkable a, Checkable b
     , Show w, Show (m a), Show (m b)
     , Arbitrary w, Arbitrary (m a), Arbitrary (m b)
     , Typeable t, Typeable m
     )
  => Proxy t -- ^ Constructor under test
  -> Proxy m -- ^ Inner monad
  -> Proxy w -- ^ Equality context for @t m@
  -> Proxy a -- ^ Value type
  -> Proxy b -- ^ Value type
  -> (forall u. (Eq u) => w -> t m u -> t m u -> Bool) -- ^ Equality test
  -> TestTree
testMonadTransLaws2 pt pm pw pa pb eq =
  let label = "MonadTrans Laws for " ++ (show $ typeRep pt) in
  testGroup label
    [ testMonadTransLaws pt pm pw pa pa eq
    , testMonadTransLaws pt pm pw pa pb eq
    , testMonadTransLaws pt pm pw pb pa eq
    , testMonadTransLaws pt pm pw pb pb eq
    ]



-- | All possible value type selections for @testMonadTransLaws@ from three choices
testMonadTransLaws3
  :: ( MonadTrans t, Monad m
     , Checkable a, Checkable b, Checkable c
     , Show w, Show (m a), Show (m b), Show (m c)
     , Arbitrary w, Arbitrary (m a), Arbitrary (m b), Arbitrary (m c)
     , Typeable t, Typeable m
     )
  => Proxy t -- ^ Constructor under test
  -> Proxy m -- ^ Inner monad
  -> Proxy w -- ^ Equality context for @t m@
  -> Proxy a -- ^ Value type
  -> Proxy b -- ^ Value type
  -> Proxy c -- ^ Value type
  -> (forall u. (Eq u) => w -> t m u -> t m u -> Bool) -- ^ Equality test
  -> TestTree
testMonadTransLaws3 pt pm pw pa pb pc eq =
  let label = "MonadTrans Laws for " ++ (show $ typeRep pt) in
  testGroup label
    [ testMonadTransLaws pt pm pw pa pa eq
    , testMonadTransLaws pt pm pw pa pb eq
    , testMonadTransLaws pt pm pw pa pc eq
    , testMonadTransLaws pt pm pw pb pa eq
    , testMonadTransLaws pt pm pw pb pb eq
    , testMonadTransLaws pt pm pw pb pc eq
    , testMonadTransLaws pt pm pw pc pa eq
    , testMonadTransLaws pt pm pw pc pb eq
    , testMonadTransLaws pt pm pw pc pc eq
    ]
