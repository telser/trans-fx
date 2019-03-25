{-# LANGUAGE Rank2Types #-}

module Test.Tasty.QuickCheck.Laws.FX.MonadTransTrans (
    testMonadTransTransLaws

  -- * MonadTransTrans Laws
  , testMonadTransTransLawIdentity
  , testMonadTransTransLawHomomorphism
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

import Control.FX.Monad.Class
import Control.FX.Monad.Trans.Trans.Class


-- | @liftT . return === return@
testMonadTransTransLawIdentity
  :: ( MonadTransTrans u, MonadTrans t, Monad m, Eq a
     , Arbitrary w, Arbitrary a
     , Show w, Show a
     )
  => Proxy u -- ^ Type constructor under test
  -> Proxy t -- ^ Inner monad transformer
  -> Proxy m -- ^ Inner monad
  -> Proxy w -- ^ Equality context for @u t m@
  -> Proxy a -- ^ Value type
  -> (forall x. (Eq x) => w -> u t m x -> u t m x -> Bool) -- ^ Equality test
  -> TestTree
testMonadTransTransLawIdentity pu pt pm pw pa eq =
  testProperty "liftT . return === return" $
    monadTransTransLawIdentity pu pt pm pw pa eq

monadTransTransLawIdentity
  :: ( MonadTransTrans u, MonadTrans t, Monad m, Eq a )
  => Proxy u -> Proxy t -> Proxy m -> Proxy w -> Proxy a
  -> (forall x. (Eq x) => w -> u t m x -> u t m x -> Bool) -- ^ Equality test
  -> w -> a -> Bool
monadTransTransLawIdentity _ _ _ _ _ eq w a =
  (eq w) (liftT $ return a) (return a)



-- | @liftT (x >>= f) == liftT x >>= (liftT . f)@
testMonadTransTransLawHomomorphism
  :: ( MonadTransTrans u, MonadTrans t, Monad m, Eq b
     , Show w, Show (t m a)
     , Arbitrary w, CoArbitrary a, Arbitrary (t m a), Arbitrary (t m b)
     )
  => Proxy u -- ^ Type constructor under test
  -> Proxy t -- ^ Inner monad transformer
  -> Proxy m -- ^ Inner monad
  -> Proxy w -- ^ Equality context for @t m@
  -> Proxy a -- ^ Value type
  -> Proxy b -- ^ Value type
  -> (forall x. (Eq x) => w -> u t m x -> u t m x -> Bool)
  -> TestTree
testMonadTransTransLawHomomorphism pu pt pm pw pa pb eq =
  testProperty "liftT (x >>= f) === liftT x >>= (liftT . f)" $
    monadTransTransLawHomomorphism pu pt pm pw pa pb eq

monadTransTransLawHomomorphism
  :: ( MonadTransTrans u, MonadTrans t, Monad m, Eq b )
  => Proxy u -> Proxy t -> Proxy m -> Proxy w -> Proxy a -> Proxy b
  -> (forall x. (Eq x) => w -> u t m x -> u t m x -> Bool) -- ^ Equality test
  -> w -> t m a -> (a -> t m b) -> Bool
monadTransTransLawHomomorphism _ _ _ _ _ _ eq w x f =
  (eq w) (liftT (x >>= f)) (liftT x >>= (liftT . f))




-- | Constructs a @TestTree@ checking that the @MonadTransTrans@ class laws hold for @u@ with inner transformer @t@ and monad @m@ and value types @a@ and @b@, using the given equality test for values of type @forall x. u t m x@. The equality context @w@ is for constructors @t m@ from which we can only extract a value within a context, such as reader-like constructors.
testMonadTransTransLaws
  :: ( MonadTransTrans u, MonadTrans t, Monad m
     , Eq a, Eq b
     , Show w, Show a, Show (t m a)
     , Arbitrary w, Arbitrary a, CoArbitrary a
     , Arbitrary (t m a), Arbitrary (t m b)
     , Typeable u, Typeable t, Typeable m, Typeable a, Typeable b
     )
  => Proxy u -- ^ Constructor under test
  -> Proxy t -- ^ Inner monad transformer
  -> Proxy m -- ^ Inner monad
  -> Proxy w -- ^ Equality context for @t m@
  -> Proxy a -- ^ Value type
  -> Proxy b -- ^ Value type
  -> (forall x. (Eq x) => w -> u t m x -> u t m x -> Bool) -- ^ Equality test
  -> TestTree
testMonadTransTransLaws pu pt pm pw pa pb eq =
  let
    label = "MonadTransTrans Laws for " ++ (show $ typeRep pu) ++ " with " ++
      "t :: " ++ (show $ typeRep pt) ++ ", " ++
      "m :: " ++ (show $ typeRep pm) ++ ", " ++
      "a :: " ++ (show $ typeRep pa) ++ ", " ++
      "b :: " ++ (show $ typeRep pb)
  in
    testGroup label
      [ testMonadTransTransLawIdentity pu pt pm pw pa eq
      , testMonadTransTransLawHomomorphism pu pt pm pw pa pb eq
      ]
