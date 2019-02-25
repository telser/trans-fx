{-# LANGUAGE Rank2Types #-}

module Test.Tasty.QuickCheck.Laws.FX.AppendOnly (
    testAppendOnlyMonadLaws

--  -- * AppendOnly Monad Laws
--  , testWriterMonadLawDraftTell
--  , testWriterMonadLawTellMempty
--  , testWriterMonadLawTellMappend
--  , testWriterMonadLawDraftReturn
--  , testWriterMonadLawDraftBind
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


-- | Constructs a @TestTree@ checking that the append only monad laws hold for @m@ with append state type @w@ and value types @a@ and @b@, using a given equality test for values of type @forall u. m u@. The equality context type @t@ is for constructors @m@ from which we can only extract a value within a context, such as reader-like constructors.
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
  -> (w -> m ()) -- ^ @append@
  -> TestTree
testAppendOnlyMonadLaws pm pt pw pa pb eq look append =
  let
    label = "AppendOnly Monad Laws for " ++ (show $ typeRep pm) ++ " with " ++
      "w :: " ++ (show $ typeRep pw) ++ ", " ++
      "a :: " ++ (show $ typeRep pa) ++ ", " ++
      "b :: " ++ (show $ typeRep pb)
  in
    testGroup label
      [ testAppendOnlyMonadLawAppendUnit pm pt pw eq append
      , testAppendOnlyMonadLawAppendHom pm pt pw eq append
      , testAppendOnlyMonadLawLookUnit pm pt pw eq look
      , testAppendOnlyMonadLawAppendLook pm pt pw eq look append
      , testAppendOnlyMonadLawLookNeutral pm pt pw pa pb eq look
      ]



-- | @append mempty === return ()@
testAppendOnlyMonadLawAppendUnit
  :: ( Monad m
     , Eq w, Monoid w
     , Show t, Show w
     , Arbitrary t, Arbitrary w
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy w -- ^ Writer type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (w -> m ()) -- ^ @append@
  -> TestTree
testAppendOnlyMonadLawAppendUnit pm pt pw eq append =
  testProperty "append mempty === return ()" $
    appendOnlyMonadLawAppendUnit pm pt pw eq append

appendOnlyMonadLawAppendUnit
  :: (Monad m, Eq w, Monoid w)
  => Proxy m -> Proxy t -> Proxy w
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (w -> m ()) -- ^ @append@
  -> t -> w -> Bool
appendOnlyMonadLawAppendUnit _ _ _ eq append t w =
  (eq t) (append mempty) (return ())



-- | @append (a <> b) == append a >> append b@
testAppendOnlyMonadLawAppendHom
  :: ( Monoid w, Monad m
     , Show t, Arbitrary w, Show w
     , Arbitrary t
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy w -- ^ Writer type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (w -> m ()) -- ^ @append@
  -> TestTree
testAppendOnlyMonadLawAppendHom pm pt pw eq append =
  testProperty "append (a <> b) == append a >> append b" $
    appendOnlyMonadLawAppendHom pm pt pw eq append

appendOnlyMonadLawAppendHom
  :: (Monoid w, Monad m, Arbitrary w, Show w)
  => Proxy m -> Proxy t -> Proxy w
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (w -> m ()) -- ^ @append@
  -> t -> w -> w -> Bool
appendOnlyMonadLawAppendHom _ _ _ eq append t w1 w2 =
  (eq t) (append w1 >> append w2) (append (w1 <> w2))



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
  -> (m w) -- ^ look
  -> t -> Bool
appendOnlyMonadLawLookUnit _ _ _ eq look t =
  (eq t) (look) (return mempty)



-- | @append w >> look  ===  append w >> return w@
testAppendOnlyMonadLawAppendLook
  :: ( Monoid w, Monad m
     , Eq w
     , Show t, Show w
     , Arbitrary t, Arbitrary w
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy w -- ^ Writer type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (m w) -- ^ look
  -> (w -> m ()) -- ^ @append@
  -> TestTree
testAppendOnlyMonadLawAppendLook pm pt pw eq look append =
  testProperty "append w >> look  ===  append w >> return w" $
    appendOnlyMonadLawAppendLook pm pt pw eq look append

appendOnlyMonadLawAppendLook
  :: (Monoid w, Monad m, Eq w)
  => Proxy m -> Proxy t -> Proxy w
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (m w) -- ^ look
  -> (w -> m ()) -- ^ @append@
  -> t -> w -> Bool
appendOnlyMonadLawAppendLook _ _ _ eq look append t w =
  (eq t) (append w >> look) (append w >> return w)



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
  -> (m w) -- ^ look
  -> TestTree
testAppendOnlyMonadLawLookNeutral pm pt pw pa pb eq look =
  testProperty "x >> look >> y  ===  x >> y" $
    appendOnlyMonadLawLookNeutral pm pt pw pa pb eq look

appendOnlyMonadLawLookNeutral
  :: (Monoid w, Monad m, Eq w, Eq b)
  => Proxy m -> Proxy t -> Proxy w -> Proxy a -> Proxy b
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (m w) -- ^ look
  -> t -> m a -> m b -> Bool
appendOnlyMonadLawLookNeutral _ _ _ _ _ eq look t x y =
  (eq t) (x >> look >> y) (x >> y)
