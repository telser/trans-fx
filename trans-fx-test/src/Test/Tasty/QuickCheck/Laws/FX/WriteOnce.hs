{-# LANGUAGE Rank2Types #-}

module Test.Tasty.QuickCheck.Laws.FX.WriteOnce (
    testWriteOnceMonadLaws

  -- * WriteOnce Monad Laws
  , testWriteOnceMonadLawEtchEtch
  , testWriteOnceMonadLawEtchPress
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


-- | Constructs a @TestTree@ checking that the append only monad laws hold for @m@ with jot state type @w@ and value types @a@ and @b@, using a given equality test for values of type @forall u. m u@. The equality context type @t@ is for constructors @m@ from which we can only extract a value within a context, such as reader-like constructors.
testWriteOnceMonadLaws
  :: ( Monad m
     , Eq w, Eq a, Eq b
     , Show t, Show w, Show a
     , Show (m a), Show (m b)
     , Arbitrary t, Arbitrary w, Arbitrary a
     , Arbitrary (m a), Arbitrary (m b)
     , Typeable m, Typeable w, Typeable a, Typeable b
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy w -- ^ Writer type
  -> Proxy a -- ^ Value type
  -> Proxy b -- ^ Value type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (m (Maybe w)) -- ^ @press@
  -> (w -> m Bool) -- ^ @etch@
  -> TestTree
testWriteOnceMonadLaws pm pt pw pa pb eq press etch =
  let
    label = "WriteOnce Monad Laws for " ++ (show $ typeRep pm) ++ " with " ++
      "w :: " ++ (show $ typeRep pw) ++ ", " ++
      "a :: " ++ (show $ typeRep pa) ++ ", " ++
      "b :: " ++ (show $ typeRep pb)
  in
    testGroup label
      [ testWriteOnceMonadLawEtchEtch pm pt pw eq etch
      , testWriteOnceMonadLawEtchPress pm pt pw eq press etch
      ]



-- | @etch a >> etch b  ===  etch a >> return False@
testWriteOnceMonadLawEtchEtch
  :: ( Monad m
     , Eq w
     , Show t, Show w
     , Arbitrary t, Arbitrary w
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy w -- ^ Writer type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (w -> m Bool) -- ^ @etch@
  -> TestTree
testWriteOnceMonadLawEtchEtch pm pt pw eq etch =
  testProperty "etch a >> etch b  ===  etch a >> return False" $
    writeOnceMonadLawEtchEtch pm pt pw eq etch

writeOnceMonadLawEtchEtch
  :: (Monad m, Eq w)
  => Proxy m -> Proxy t -> Proxy w
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (w -> m Bool) -- ^ @etch@
  -> t -> w -> w -> Bool
writeOnceMonadLawEtchEtch _ _ _ eq etch t a b =
  (eq t) (etch a >> etch b) (etch a >> return False)



-- | @etch a >> press  ===  return (Just $ pure a)@
testWriteOnceMonadLawEtchPress
  :: ( Monad m, Eq w
     , Show t, Arbitrary w, Show w
     , Arbitrary t
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy w -- ^ Writer type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (m (Maybe w)) -- ^ @press@
  -> (w -> m Bool) -- ^ @etch@
  -> TestTree
testWriteOnceMonadLawEtchPress pm pt pw eq press etch =
  testProperty "etch a >> press  ===  return (Just $ pure a)" $
    writeOnceMonadLawEtchPress pm pt pw eq press etch

writeOnceMonadLawEtchPress
  :: (Monad m, Arbitrary w, Show w, Eq w)
  => Proxy m -> Proxy t -> Proxy w
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (m (Maybe w)) -- ^ @press@
  -> (w -> m Bool) -- ^ @etch@
  -> t -> w -> Bool
writeOnceMonadLawEtchPress _ _ _ eq press etch t a =
  (eq t) (etch a >> press) (etch a >> return (Just a))
