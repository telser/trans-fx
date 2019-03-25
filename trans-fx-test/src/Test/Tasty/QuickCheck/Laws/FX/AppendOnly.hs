{-# LANGUAGE Rank2Types #-}

module Test.Tasty.QuickCheck.Laws.FX.AppendOnly (
    testAppendOnlyMonadLaws

  -- * AppendOnly Monad Laws
  , testAppendOnlyMonadLawJotUnit
  , testAppendOnlyMonadLawJotHom
  , testAppendOnlyMonadLawLookUnit
  , testAppendOnlyMonadLawJotLook
  , testAppendOnlyMonadLawLookNeutral
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
testAppendOnlyMonadLaws
  :: ( Monoid w, Monad m
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
  -> (m w) -- ^ @look@
  -> (w -> m ()) -- ^ @jot@
  -> TestTree
testAppendOnlyMonadLaws pm pt pw pa pb eq look jot =
  let
    label = "AppendOnly Monad Laws for " ++ (show $ typeRep pm) ++ " with " ++
      "w :: " ++ (show $ typeRep pw) ++ ", " ++
      "a :: " ++ (show $ typeRep pa) ++ ", " ++
      "b :: " ++ (show $ typeRep pb)
  in
    testGroup label
      [ testAppendOnlyMonadLawJotUnit pm pt pw eq jot
      , testAppendOnlyMonadLawJotHom pm pt pw eq jot
      , testAppendOnlyMonadLawLookUnit pm pt pw eq look
      , testAppendOnlyMonadLawJotLook pm pt pw eq look jot
      , testAppendOnlyMonadLawLookNeutral pm pt pw pa pb eq look
      ]



-- | @jot mempty === return ()@
testAppendOnlyMonadLawJotUnit
  :: ( Monad m
     , Eq w, Monoid w
     , Show t, Show w
     , Arbitrary t, Arbitrary w
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy w -- ^ Writer type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (w -> m ()) -- ^ @jot@
  -> TestTree
testAppendOnlyMonadLawJotUnit pm pt pw eq jot =
  testProperty "jot mempty === return ()" $
    appendOnlyMonadLawJotUnit pm pt pw eq jot

appendOnlyMonadLawJotUnit
  :: (Monad m, Eq w, Monoid w)
  => Proxy m -> Proxy t -> Proxy w
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (w -> m ()) -- ^ @jot@
  -> t -> w -> Bool
appendOnlyMonadLawJotUnit _ _ _ eq jot t w =
  (eq t) (jot mempty) (return ())



-- | @jot (a <> b) == jot a >> jot b@
testAppendOnlyMonadLawJotHom
  :: ( Monoid w, Monad m
     , Show t, Arbitrary w, Show w
     , Arbitrary t
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy w -- ^ Writer type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (w -> m ()) -- ^ @jot@
  -> TestTree
testAppendOnlyMonadLawJotHom pm pt pw eq jot =
  testProperty "jot (a <> b) == jot a >> jot b" $
    appendOnlyMonadLawJotHom pm pt pw eq jot

appendOnlyMonadLawJotHom
  :: (Monoid w, Monad m, Arbitrary w, Show w)
  => Proxy m -> Proxy t -> Proxy w
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (w -> m ()) -- ^ @jot@
  -> t -> w -> w -> Bool
appendOnlyMonadLawJotHom _ _ _ eq jot t w1 w2 =
  (eq t) (jot w1 >> jot w2) (jot (w1 <> w2))



-- | @look == return mempty@
testAppendOnlyMonadLawLookUnit
  :: ( Monoid w, Monad m, Eq w
     , Show t, Show w
     , Arbitrary t, Arbitrary w
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy w -- ^ Writer type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (m w) -- ^ @look@
  -> TestTree
testAppendOnlyMonadLawLookUnit pm pt pw eq look =
  testProperty "look === return mempty" $
    appendOnlyMonadLawLookUnit pm pt pw eq look

appendOnlyMonadLawLookUnit
  :: (Monoid w, Monad m, Eq w)
  => Proxy m -> Proxy t -> Proxy w
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (m w) -- ^ @look@
  -> t -> Bool
appendOnlyMonadLawLookUnit _ _ _ eq look t =
  (eq t) (look) (return mempty)



-- | @jot w >> look  ===  jot w >> return w@
testAppendOnlyMonadLawJotLook
  :: ( Monoid w, Monad m
     , Eq w
     , Show t, Show w
     , Arbitrary t, Arbitrary w
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy w -- ^ Writer type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (m w) -- ^ @look@
  -> (w -> m ()) -- ^ @jot@
  -> TestTree
testAppendOnlyMonadLawJotLook pm pt pw eq look jot =
  testProperty "jot w >> look  ===  jot w >> return w" $
    appendOnlyMonadLawJotLook pm pt pw eq look jot

appendOnlyMonadLawJotLook
  :: (Monoid w, Monad m, Eq w)
  => Proxy m -> Proxy t -> Proxy w
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (m w) -- ^ @look@
  -> (w -> m ()) -- ^ @jot@
  -> t -> w -> Bool
appendOnlyMonadLawJotLook _ _ _ eq look jot t w =
  (eq t) (jot w >> look) (jot w >> return w)



-- | @x >> look >> y  ===  x >> y@
testAppendOnlyMonadLawLookNeutral
  :: ( Monoid w, Monad m
     , Eq w, Eq b
     , Show t
     , Show (m a), Show (m b)
     , Arbitrary t
     , Arbitrary (m a), Arbitrary (m b)
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy w -- ^ Writer type
  -> Proxy a -- ^ Value type
  -> Proxy b -- ^ Value type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (m w) -- ^ @look@
  -> TestTree
testAppendOnlyMonadLawLookNeutral pm pt pw pa pb eq look =
  testProperty "x >> look >> y  ===  x >> y" $
    appendOnlyMonadLawLookNeutral pm pt pw pa pb eq look

appendOnlyMonadLawLookNeutral
  :: (Monoid w, Monad m, Eq w, Eq b)
  => Proxy m -> Proxy t -> Proxy w -> Proxy a -> Proxy b
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (m w) -- ^ @look@
  -> t -> m a -> m b -> Bool
appendOnlyMonadLawLookNeutral _ _ _ _ _ eq look t x y =
  (eq t) (x >> look >> y) (x >> y)
