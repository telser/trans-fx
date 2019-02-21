{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Test.Tasty.QuickCheck.Laws.FX.LiftDraft (
    testLiftDraftLaws

  -- * LiftDraft Laws
  , testLiftDraftLawHom
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

import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans




testLiftDraftLaws
  :: ( LiftDraft z t f, MonadWriteOnly mark w m
     , Arbitrary h, Arbitrary (m a)
     , Show h, Show (m a)
     , Eq a, Eq (mark w)
     , Typeable t, Typeable m, Typeable mark, Typeable w, Typeable a
     )
  => Proxy (t :: (* -> *) -> * -> *) -- ^ Constructor under test
  -> Proxy h -- ^ Equality context for @t m@
  -> Proxy (m :: * -> *) -- ^ Inner monad
  -> Proxy w -- ^ Writer type
  -> Proxy (mark :: * -> *) -- ^ Mark type
  -> Proxy a -- ^ Value type
  -> (forall u. (Eq u) => h -> t m u -> t m u -> Bool) -- ^ Equality test
  -> TestTree
testLiftDraftLaws pt ph pm pw pmark pa eq =
  let
    label = "LiftCatch Laws for " ++ (show $ typeRep pt) ++ " on " ++
      "m :: " ++ (show $ typeRep pm) ++ " with " ++
      "mark :: " ++ (show $ typeRep pmark) ++ " with " ++
      "w :: " ++ (show $ typeRep pw) ++ " with " ++
      "a :: " ++ (show $ typeRep pa)
  in
    testGroup label
      [ testLiftDraftLawHom pt ph pm pw pmark pa eq
      ]




-- | @liftDraft draft (lift x) === lift (draft x)@
testLiftDraftLawHom
  :: ( LiftDraft z t f, MonadWriteOnly mark w m
     , Arbitrary h, Arbitrary (m a)
     , Show h, Show (m a)
     , Eq a, Eq (mark w)
     )
  => Proxy t -- ^ Constructor under test
  -> Proxy h -- ^ Equality context for @t m@
  -> Proxy m -- ^ Inner monad
  -> Proxy w -- ^ Writer type
  -> Proxy mark -- ^ Mark type
  -> Proxy a -- ^ Value type
  -> (forall u. (Eq u) => h -> t m u -> t m u -> Bool) -- ^ Equality test
  -> TestTree
testLiftDraftLawHom pt ph pm pw pmark pa eq =
  testProperty "liftDraft draft (lift x) === lift (draft x)" $
    liftDraftLawHom pt ph pm pw pmark pa eq

liftDraftLawHom
  :: forall t h m z f w mark a
   . ( LiftDraft z t f, MonadWriteOnly mark w m
     , Arbitrary h, Arbitrary (m a)
     , Show h, Show (m a)
     , Eq a, Eq (mark w)
     )
  => Proxy t -- ^ Constructor under test
  -> Proxy h -- ^ Equality context for @t m@
  -> Proxy m -- ^ Inner monad
  -> Proxy w -- ^ Writer type
  -> Proxy mark -- ^ Mark type
  -> Proxy a -- ^ Value type
  -> (forall u. (Eq u) => h -> t m u -> t m u -> Bool) -- ^ Equality test
  -> h -> m a -> Bool
liftDraftLawHom _ _ _ _ _ _ eq v x =
  eq v (liftDraft draft $ lift x) ((lift $ draft x) :: t m (Pair (mark w) a) )
