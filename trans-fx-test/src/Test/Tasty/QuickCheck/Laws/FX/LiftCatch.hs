{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Test.Tasty.QuickCheck.Laws.FX.LiftCatch (
    testLiftCatchLaws

  -- * LiftCatch Laws
  , testLiftCatchLawHom
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




testLiftCatchLaws
  :: ( LiftCatch t, MonadExcept mark e m
     , Arbitrary h, Arbitrary (m a), CoArbitrary (mark e)
     , Show h, Show (m a)
     , Eq a
     , Typeable t, Typeable m, Typeable mark, Typeable e, Typeable a
     )
  => Proxy (t :: (* -> *) -> * -> *) -- ^ Constructor under test
  -> Proxy h -- ^ Equality context for @t m@
  -> Proxy (m :: * -> *) -- ^ Inner monad
  -> Proxy e -- ^ Error type
  -> Proxy (mark :: * -> *) -- ^ Mark type
  -> Proxy a -- ^ Value type
  -> (forall u. (Eq u) => h -> t m u -> t m u -> Bool) -- ^ Equality test
  -> TestTree
testLiftCatchLaws pt ph pm pe pmark pa eq =
  let
    label = "LiftCatch Laws for " ++ (show $ typeRep pt) ++ " on " ++
      "m :: " ++ (show $ typeRep pm) ++ " with " ++
      "mark :: " ++ (show $ typeRep pmark) ++ " with " ++
      "e :: " ++ (show $ typeRep pe) ++ " with " ++
      "a :: " ++ (show $ typeRep pa)
  in
    testGroup label
      [ testLiftCatchLawHom pt ph pm pe pmark pa eq
      ]




-- | @lift (catch x h) === liftCatch catch (lift x) (lift . h)@
testLiftCatchLawHom
  :: ( LiftCatch t, MonadExcept mark e m
     , Arbitrary h, Arbitrary (m a), CoArbitrary (mark e)
     , Show h, Show (m a)
     , Eq a
     )
  => Proxy t -- ^ Constructor under test
  -> Proxy h -- ^ Equality context for @t m@
  -> Proxy m -- ^ Inner monad
  -> Proxy e -- ^ Error type
  -> Proxy mark -- ^ Mark type
  -> Proxy a -- ^ Value type
  -> (forall u. (Eq u) => h -> t m u -> t m u -> Bool) -- ^ Equality test
  -> TestTree
testLiftCatchLawHom pt ph pm pe pmark pa eq =
  testProperty "lift (catch x h) === liftCatch catch (lift x) (lift . h)" $
    liftCatchLawHom pt ph pm pe pmark pa eq

liftCatchLawHom
  :: ( LiftCatch t, MonadExcept mark e m
     , Arbitrary h, Arbitrary (m a), CoArbitrary (mark e)
     , Show h, Show (m a)
     , Eq a
     )
  => Proxy t -- ^ Constructor under test
  -> Proxy h -- ^ Equality context for @t m@
  -> Proxy m -- ^ Inner monad
  -> Proxy e -- ^ Error type
  -> Proxy mark -- ^ Mark type
  -> Proxy a -- ^ Value type
  -> (forall u. (Eq u) => h -> t m u -> t m u -> Bool) -- ^ Equality test
  -> h -> m a -> (mark e -> m a) -> Bool
liftCatchLawHom _ _ _ _ _ _ eq v x h =
  eq v (lift $ catch x h) (liftCatch catch (lift x) (lift . h))
