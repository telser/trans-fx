{-#
  LANGUAGE
    KindSignatures,
    FlexibleContexts,
    ScopedTypeVariables,
    QuantifiedConstraints,
    MultiParamTypeClasses
#-}

module Main where

import Data.Proxy
import Data.Typeable
import System.Environment

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.QuickCheck.Laws

import Control.FX
import Control.FX.Arbitrary
import Control.FX.EqIn

import Test.Tasty.QuickCheck.Laws.FX.EqIn
import Test.Tasty.QuickCheck.Laws.FX.Central
import Test.Tasty.QuickCheck.Laws.FX.Bifunctor
import Test.Tasty.QuickCheck.Laws.FX.Commutant
import Test.Tasty.QuickCheck.Laws.FX.MonadTrans
import Test.Tasty.QuickCheck.Laws.FX.MonadTransTrans
import Test.Tasty.QuickCheck.Laws.FX.LiftCatch
import Test.Tasty.QuickCheck.Laws.FX.LiftDraft
import Test.Tasty.QuickCheck.Laws.FX.AppendOnly



main :: IO ()
main = do
  setEnv "TASTY_NUM_THREADS" "6"
  setEnv "TASTY_QUICKCHECK_TESTS" "100"
  setEnv "TASTY_HIDE_SUCCESSES" "TRUE"
  defaultMain $ testGroup "Laws"
    [ testGroup "EqIn"
      [ test_all_EqIn
      ]
    , testGroup "Functor"
      [ test_all_Functor_F
      , test_all_Functor_S
      , test_all_Functor_C
      , test_all_Functor_B
      ]

    ,  testGroup "Monad"
      [ test_all_Monad_FAM
      , test_all_Monad_FX
      , test_all_Monad_C
      ]

    , testGroup "MonadTrans"
      [ test_all_MonadTrans_FAM
      , test_all_MonadTrans_T
      , test_all_MonadTrans_FX
      , test_all_MonadTrans_LiftCatch
      , test_all_MonadTrans_LiftDraft
      ]

    , testGroup "MonadTransTrans"
      [ test_all_MonadTransTrans_FAM
      , test_all_MonadTransTrans_TT
      ]
    ]

pU :: Proxy ()
pU = Proxy

pI :: Proxy Int
pI = Proxy

pB :: Proxy Bool
pB = Proxy

pT2 :: Proxy a -> Proxy b -> Proxy (a,b)
pT2 _ _ = Proxy

pEval :: Proxy (p :: * -> *) -> Proxy (m :: * -> *) -> Proxy (Eval p m)
pEval _ _ = Proxy

pId :: Proxy Identity
pId = Proxy

pIdU :: Proxy (Identity ())
pIdU = Proxy

pIdI :: Proxy (Identity Int)
pIdI = Proxy

pIdB :: Proxy (Identity Bool)
pIdB = Proxy

pAp :: Proxy (m :: * -> *) -> Proxy (a :: *) -> Proxy (m a)
pAp _ _ = Proxy

pMb :: Proxy Maybe
pMb = Proxy

pAwait :: Proxy exp -> Proxy (Await exp)
pAwait _ = Proxy

pYield :: Proxy res -> Proxy (Yield res)
pYield _ = Proxy

pUnit :: Proxy (a :: * -> *) -> Proxy (Unit a)
pUnit _ = Proxy

fixDraft :: (Monad m) => (m a -> m (Pair w a)) -> m a -> m (a,w)
fixDraft draft x = do
  Pair w a <- draft x
  return (a,w)

instance Semigroup Bool where (<>) = (&&)
instance Monoid Bool where mempty = True; mappend = (&&)
instance Semigroup Int where (<>) = (+)
instance Monoid Int where mempty = 0; mappend = (+)

instance (Semigroup w) => Semigroup (Q w) where
  (Q x) <> (Q y) = Q (x <> y)
instance (Monoid w) => Monoid (Q w) where
  mempty = Q mempty
  mappend = (<>)

data Q a = Q a deriving (Eq, Show)

instance Functor Q where
  fmap f (Q x) = Q (f x)

instance Applicative Q where
  pure = Q
  (Q f) <*> (Q x) = Q (f x)

instance Monad Q where
  return = Q
  (Q x) >>= f = f x

instance MonadIdentity Q where
  unwrap (Q x) = x

instance (Arbitrary a) => Arbitrary (Q a) where
  arbitrary = Q <$> arbitrary

pQU :: Proxy (Q ())
pQU = Proxy

pQI :: Proxy (Q Int)
pQI = Proxy

pQB :: Proxy (Q Bool)
pQB = Proxy



{--------}
{- EqIn -}
{--------}

test_all_EqIn :: TestTree
test_all_EqIn = testGroup "All EqIn"
  [ testEqInLaws (Proxy :: Proxy ()) (Proxy :: Proxy (Identity Int))
  , testEqInLaws (Proxy :: Proxy ()) (Proxy :: Proxy (Maybe Int))

  , testEqInLaws (Proxy :: Proxy (Identity ())) (Proxy :: Proxy (Except Identity () Int))
  , testEqInLaws (Proxy :: Proxy (Identity ())) (Proxy :: Proxy (WriteOnly Identity () Int))
  , testEqInLaws (Proxy :: Proxy (Identity ())) (Proxy :: Proxy (ReadOnly Identity () Int))
  , testEqInLaws (Proxy :: Proxy (Identity ())) (Proxy :: Proxy (State Identity () Int))
  ]



{-----------}
{- Functor -}
{-----------}

test_all_Functor_F :: TestTree
test_all_Functor_F = testGroup "All Functor (F)"
  [ testFunctorLaws2 (Proxy :: Proxy (Pair Bool)) pU pB pI eqIn
  , testFunctorLaws2 (Proxy :: Proxy (Pair Int))  pU pB pI eqIn

  , testFunctorLaws2 (Proxy :: Proxy LeftZero)  pU pB pI eqIn
  , testFunctorLaws2 (Proxy :: Proxy RightZero) pU pB pI eqIn
  ]

test_all_Functor_S :: TestTree
test_all_Functor_S = testGroup "All Functor (S)"
  [ testMonoidLaws (Proxy :: Proxy (LeftZero Bool))
  , testMonoidLaws (Proxy :: Proxy (LeftZero Int))

  , testMonoidLaws (Proxy :: Proxy (RightZero Bool))
  , testMonoidLaws (Proxy :: Proxy (RightZero Int))
  ]

-- Test the Commutant class laws for several concrete functors over several applicative functors.
test_all_Functor_C :: TestTree
test_all_Functor_C = testGroup "All Functor (C)"
  [ test_Functor_C (Proxy :: Proxy Identity)

  , test_Functor_C (Proxy :: Proxy Maybe)
  , test_Functor_C (Proxy :: Proxy LeftZero)
  , test_Functor_C (Proxy :: Proxy RightZero)

  , test_Functor_C (Proxy :: Proxy (Pair Bool))
  , test_Functor_C (Proxy :: Proxy (Pair Int))

  , test_Functor_C (Proxy :: Proxy (Except Identity Bool))
  , test_Functor_C (Proxy :: Proxy (Except Identity Int))

  , test_Functor_C (Proxy :: Proxy (WriteOnly Identity Bool))
  , test_Functor_C (Proxy :: Proxy (WriteOnly Identity Int))
  ]

-- | Test the Bifunctor laws
test_all_Functor_B :: TestTree
test_all_Functor_B = testGroup "All Bifunctor"
  [ testBifunctorLaws (Proxy :: Proxy Pair) pU pI pI pI pI eqIn
  , testBifunctorLaws (Proxy :: Proxy Either) pU pI pI pI pI eqIn
  , testBifunctorLaws (Proxy :: Proxy (Except Identity)) pIdU pI pI pI pI eqIn
  ]

-- Test the Commutant class laws for a concrete functor over several applicative functors.
test_Functor_C
  :: ( Commutant c, Typeable c
     , forall x. (Arbitrary x) => Arbitrary (c x)
     , forall x. (Show x) => Show (c x)
     , forall x. (Eq x) => Eq (c x)
     )
  => Proxy (c :: * -> *)
  -> TestTree
test_Functor_C proxyC =
  testGroup "Monad (C)"
    [ testCommutantLaws proxyC (Proxy :: Proxy Identity) pU pI eqIn

    , testCommutantLaws proxyC (Proxy :: Proxy Maybe) pU pI eqIn

    , testCommutantLaws proxyC (Proxy :: Proxy (Except Identity Bool)) pIdU pI eqIn
    , testCommutantLaws proxyC (Proxy :: Proxy (Except Identity Int))  pIdU pI eqIn

    , testCommutantLaws proxyC (Proxy :: Proxy (WriteOnly Identity Bool)) pIdU pI eqIn
    , testCommutantLaws proxyC (Proxy :: Proxy (WriteOnly Identity Int))  pIdU pI eqIn

    , testCommutantLaws proxyC (Proxy :: Proxy (State Identity Bool)) pIdB pI eqIn
    , testCommutantLaws proxyC (Proxy :: Proxy (State Identity Int))  pIdI pI eqIn

    , testCommutantLaws proxyC (Proxy :: Proxy (ReadOnly Identity Bool)) pIdB pI eqIn
    , testCommutantLaws proxyC (Proxy :: Proxy (ReadOnly Identity Int))  pIdI pI eqIn
    ]





{---------}
{- Monad -}
{---------}

-- Test Functor, Applicative, and Monad laws for several concrete monads.
test_all_Monad_FAM :: TestTree
test_all_Monad_FAM = testGroup "All Monad (FAM)"
  [ test_Monad_FAM (Proxy :: Proxy Identity) pU

  , test_Monad_FAM (Proxy :: Proxy (Except Identity Bool)) pIdU
  , test_Monad_FAM (Proxy :: Proxy (Except Identity Int))  pIdU

  , test_Monad_FAM (Proxy :: Proxy (WriteOnly Identity Bool)) pIdU
  , test_Monad_FAM (Proxy :: Proxy (WriteOnly Identity Int))  pIdU

  , test_Monad_FAM (Proxy :: Proxy (State Identity Bool)) pIdB
  , test_Monad_FAM (Proxy :: Proxy (State Identity Int))  pIdI

  , test_Monad_FAM (Proxy :: Proxy (ReadOnly Identity Bool)) pIdB
  , test_Monad_FAM (Proxy :: Proxy (ReadOnly Identity Int))  pIdI

  , test_Monad_FAM (Proxy :: Proxy (AppendOnly Identity Bool)) pIdU
  , test_Monad_FAM (Proxy :: Proxy (AppendOnly Identity Int))  pIdU
  ]



-- Test effect class laws for several concrete monads.
test_all_Monad_FX :: TestTree
test_all_Monad_FX = testGroup "All Monad (FX)"
  [ testStateMonadLaws (Proxy :: Proxy (State Identity Bool)) pIdB (pAp pId pB) pI eqIn get put
  , testStateMonadLaws (Proxy :: Proxy (State Identity Int))  pIdI (pAp pId pI) pI eqIn get put

  , testReaderMonadLaws (Proxy :: Proxy (ReadOnly Identity Bool)) pIdB (pAp pId pB) pB pI eqIn ask local
  , testReaderMonadLaws (Proxy :: Proxy (ReadOnly Identity Int))  pIdI (pAp pId pI) pB pI eqIn ask local

  , testWriterMonadLaws (Proxy :: Proxy (WriteOnly Identity Bool)) pIdU (pAp pId pB) pB pI eqIn tell (fixDraft draft)
  , testWriterMonadLaws (Proxy :: Proxy (WriteOnly Identity Int))  pIdU (pAp pId pI) pB pI eqIn tell (fixDraft draft)

  , testErrorMonadLaws (Proxy :: Proxy (Except Identity Bool)) pIdU (pAp pId pB) pB pI eqIn throw catch
  , testErrorMonadLaws (Proxy :: Proxy (Except Identity Int))  pIdU (pAp pId pI) pB pI eqIn throw catch

  , testAppendOnlyMonadLaws (Proxy :: Proxy (AppendOnly Identity Bool)) pIdU (pAp pId pB) pB pI eqIn look jot
  , testAppendOnlyMonadLaws (Proxy :: Proxy (AppendOnly Identity Int))  pIdU (pAp pId pI) pB pI eqIn look jot
  ]



-- Test the Commutant class laws for several concrete functors over several applicative functors.
test_all_Monad_C :: TestTree
test_all_Monad_C = testGroup "All Functor (C)"
  [ test_Monad_C (Proxy :: Proxy Identity)

  , test_Monad_C (Proxy :: Proxy Maybe)

  , test_Monad_C (Proxy :: Proxy (Except Identity Bool))
  , test_Monad_C (Proxy :: Proxy (Except Identity Int))

  , test_Monad_C (Proxy :: Proxy (WriteOnly Identity Bool))
  , test_Monad_C (Proxy :: Proxy (WriteOnly Identity Int))
  ]



-- Test Functor, Applicative, and Monad instances for a concrete monad.
test_Monad_FAM
  :: ( Functor m, Applicative m, Monad m, Typeable m
     , Show w
       , forall u
           . ( Show u, Typeable u
             )
          => Show (m u)
     , Arbitrary w, forall u. (Arbitrary u) => Arbitrary (m u)
     , forall u. (Eq u) => EqIn w (m u) )
  => Proxy (m :: * -> *)
  -> Proxy (w :: *)
  -> TestTree
test_Monad_FAM proxyM proxyW =
  testGroup "Monad (FAM)"
    [ testFunctorLaws2     proxyM proxyW pB pI eqIn
    , testApplicativeLaws2 proxyM proxyW pB pI eqIn
    , testMonadLaws2       proxyM proxyW pB pI eqIn
    ]



-- Test the Central class laws for a concrete functor over several applicative functors.
test_Monad_C
  :: ( Central c, Typeable c
     , forall x. (Arbitrary x) => Arbitrary (c x)
     , forall x. (Show x, Typeable x) => Show (c x)
     , forall x. (Eq x) => Eq (c x)
     )
  => Proxy (c :: * -> *)
  -> TestTree
test_Monad_C proxyC =
  testGroup "Monad (C)"
    [ testCentralLaws proxyC (Proxy :: Proxy Identity) pU pI eqIn

    , testCentralLaws proxyC (Proxy :: Proxy Maybe) pU pI eqIn

    , testCentralLaws proxyC (Proxy :: Proxy (Except Identity Bool)) pIdU pI eqIn
    , testCentralLaws proxyC (Proxy :: Proxy (Except Identity Int))  pIdU pI eqIn

    , testCentralLaws proxyC (Proxy :: Proxy (WriteOnly Identity Bool)) pIdU pI eqIn
    , testCentralLaws proxyC (Proxy :: Proxy (WriteOnly Identity Int))  pIdU pI eqIn

    , testCentralLaws proxyC (Proxy :: Proxy (State Identity Bool)) pIdB pI eqIn
    , testCentralLaws proxyC (Proxy :: Proxy (State Identity Int))  pIdI pI eqIn

    , testCentralLaws proxyC (Proxy :: Proxy (ReadOnly Identity Bool)) pIdB pI eqIn
    , testCentralLaws proxyC (Proxy :: Proxy (ReadOnly Identity Int))  pIdI pI eqIn
    ]





{--------------}
{- MonadTrans -}
{--------------}

pPogo :: Proxy mark -> Proxy sus -> Proxy (Pogo mark sus)
pPogo _ _ = Proxy

-- Test Functor, Applicative, and Monad laws for several concrete monad transformers over several concrete monads.
test_all_MonadTrans_FAM :: TestTree
test_all_MonadTrans_FAM = testGroup "All MonadTrans (FAM)"
  [ test_MonadTrans_FAM (Proxy :: Proxy IdentityT) pU

  , test_MonadTrans_FAM (Proxy :: Proxy (HaltT Identity)) pIdU

  , test_MonadTrans_FAM (Proxy :: Proxy (ExceptT Identity Bool)) pIdU
  , test_MonadTrans_FAM (Proxy :: Proxy (ExceptT Identity Int))  pIdU

  , test_MonadTrans_FAM (Proxy :: Proxy (WriteOnlyT Identity Bool)) pIdU
  , test_MonadTrans_FAM (Proxy :: Proxy (WriteOnlyT Identity Int))  pIdU

  , test_MonadTrans_FAM (Proxy :: Proxy (StateT Identity Bool)) pIdB
  , test_MonadTrans_FAM (Proxy :: Proxy (StateT Identity Int))  pIdI

  , test_MonadTrans_FAM (Proxy :: Proxy (ReadOnlyT Identity Bool)) pIdB
  , test_MonadTrans_FAM (Proxy :: Proxy (ReadOnlyT Identity Int))  pIdI

  , test_MonadTrans_FAM (Proxy :: Proxy (CoroutineT Identity Identity))     (pPogo pId pId)
  , test_MonadTrans_FAM (Proxy :: Proxy (CoroutineT Identity (Await Bool))) (pPogo pId (pAwait pB))
  , test_MonadTrans_FAM (Proxy :: Proxy (CoroutineT Identity (Yield Bool))) (pPogo pId (pYield pB))

  , testGroup "ComposeT"
    [ test_MonadTrans_ComposeT_FAM (Proxy :: Proxy IdentityT) pU

    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (HaltT Identity)) pIdU

    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (ExceptT Identity Bool)) pIdU
    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (ExceptT Identity Int))  pIdU

    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (WriteOnlyT Identity Bool)) pIdU
    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (WriteOnlyT Identity Int))  pIdU

    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (ReadOnlyT Identity Bool)) pIdB
    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (ReadOnlyT Identity Int))  pIdI

    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (StateT Identity Bool)) pIdB
    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (StateT Identity Int))  pIdI

    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (CoroutineT Identity Identity))     (pPogo pId pId)
    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (CoroutineT Identity (Await Bool))) (pPogo pId (pAwait pB))
    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (CoroutineT Identity (Yield Bool))) (pPogo pId (pYield pB))
    ]
  ]



-- Test MonadTrans laws for several concrete monad transformers over several concrete monads.
test_all_MonadTrans_T :: TestTree
test_all_MonadTrans_T = testGroup "All MonadTrans (T)"
  [ test_MonadTrans_T (Proxy :: Proxy IdentityT) pU

  , test_MonadTrans_T (Proxy :: Proxy (HaltT Identity)) pIdU

  , test_MonadTrans_T (Proxy :: Proxy (ExceptT Identity Bool)) pIdU
  , test_MonadTrans_T (Proxy :: Proxy (ExceptT Identity Int))  pIdU

  , test_MonadTrans_T (Proxy :: Proxy (WriteOnlyT Identity Bool)) pIdU
  , test_MonadTrans_T (Proxy :: Proxy (WriteOnlyT Identity Int))  pIdU

  , test_MonadTrans_T (Proxy :: Proxy (StateT Identity Bool)) pIdB
  , test_MonadTrans_T (Proxy :: Proxy (StateT Identity Int))  pIdI

  , test_MonadTrans_T (Proxy :: Proxy (ReadOnlyT Identity Bool)) pIdB
  , test_MonadTrans_T (Proxy :: Proxy (ReadOnlyT Identity Int))  pIdI

  , test_MonadTrans_T (Proxy :: Proxy (CoroutineT Identity Identity))     (pPogo pId pId)
  , test_MonadTrans_T (Proxy :: Proxy (CoroutineT Identity (Await Bool))) (pPogo pId (pAwait pB))
  , test_MonadTrans_T (Proxy :: Proxy (CoroutineT Identity (Yield Bool))) (pPogo pId (pYield pB))
  ]



-- Test effect class laws for several concrete monad transformers over several concrete monads.
test_all_MonadTrans_FX :: TestTree
test_all_MonadTrans_FX = testGroup "All MonadTrans (FX)"
  [ testGroup "State"
    [ test_MonadTrans_State (Proxy :: Proxy (StateT Identity Bool)) pIdB pId pB

    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (StateT Identity Bool) (IdentityT))) (pT2 pIdB pU) pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (StateT Identity Int)  (IdentityT))) (pT2 pIdI pU) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (StateT Identity Bool) ((HaltT Identity))))    (pT2 pIdB pIdU) pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (StateT Identity Int)  ((HaltT Identity))))    (pT2 pIdI pIdU) pId pI

    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (IdentityT) (StateT Identity Bool))) (pT2 pU pIdB) pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (IdentityT) (StateT Identity Int)))  (pT2 pU pIdI) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT ((HaltT Identity))    (StateT Identity Bool))) (pT2 pIdU pIdB) pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT ((HaltT Identity))    (StateT Identity Int)))  (pT2 pIdU pIdI) pId pI
    ]

  , testGroup "Except"
    [ test_MonadTrans_Except (Proxy :: Proxy (ExceptT Identity Bool)) pIdU pId pB

    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ExceptT Identity Bool) (IdentityT))) (pT2 pIdU pU) pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ExceptT Identity Int)  (IdentityT))) (pT2 pIdU pU) pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ExceptT Identity Bool) ((HaltT Identity))))    (pT2 pIdU pIdU) pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ExceptT Identity Int)  ((HaltT Identity))))    (pT2 pIdU pIdU) pId pI

    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (IdentityT) (ExceptT Identity Bool))) (pT2 pU pIdU) pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (IdentityT) (ExceptT Identity Int)))  (pT2 pU pIdU) pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT ((HaltT Identity))    (ExceptT Identity Bool))) (pT2 pIdU pIdU) pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT ((HaltT Identity))    (ExceptT Identity Int)))  (pT2 pIdU pIdU) pId pI
    ]

  , testGroup "WriteOnly"
    [ test_MonadTrans_WriteOnly (Proxy :: Proxy (WriteOnlyT Identity Bool)) pIdU pId pB

    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Identity Bool) (IdentityT))) (pT2 pIdU pU) pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Identity Int)  (IdentityT))) (pT2 pIdU pU) pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Identity Bool) ((HaltT Identity))))    (pT2 pIdU pIdU) pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Identity Int)  ((HaltT Identity))))    (pT2 pIdU pIdU) pId pI

    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (IdentityT) (WriteOnlyT Identity Bool))) (pT2 pU pIdU) pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (IdentityT) (WriteOnlyT Identity Int)))  (pT2 pU pIdU) pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT ((HaltT Identity))    (WriteOnlyT Identity Bool))) (pT2 pIdU pIdU) pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT ((HaltT Identity))    (WriteOnlyT Identity Int)))  (pT2 pIdU pIdU) pId pI
    ]

  , testGroup "ReadOnly"
    [ test_MonadTrans_ReadOnly (Proxy :: Proxy (ReadOnlyT Identity Bool)) pIdB pId pB

    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Identity Bool) (IdentityT))) (pT2 pIdB pU) pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Identity Int)  (IdentityT))) (pT2 pIdI pU) pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Identity Bool) ((HaltT Identity))))    (pT2 pIdB pIdU) pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Identity Int)  ((HaltT Identity))))    (pT2 pIdI pIdU) pId pI

    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (IdentityT) (ReadOnlyT Identity Bool))) (pT2 pU pIdB) pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (IdentityT) (ReadOnlyT Identity Int)))  (pT2 pU pIdI) pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT ((HaltT Identity))    (ReadOnlyT Identity Bool))) (pT2 pIdU pIdB) pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT ((HaltT Identity))    (ReadOnlyT Identity Int)))  (pT2 pIdU pIdI) pId pI
    ]
  ]



test_all_MonadTrans_LiftCatch :: TestTree
test_all_MonadTrans_LiftCatch = testGroup "MonadTrans (LiftCatch)"
  [ testLiftCatchLaws (Proxy :: Proxy IdentityT)              (Proxy :: Proxy ((), Identity ())) (Proxy :: Proxy (Except Identity Int)) pI pId pI eqIn
  , testLiftCatchLaws (Proxy :: Proxy (HaltT Identity))                 (Proxy :: Proxy (Identity (), Identity ())) (Proxy :: Proxy (Except Identity Int)) pI pId pI eqIn

  , testLiftCatchLaws (Proxy :: Proxy (WriteOnlyT Identity Int)) (Proxy :: Proxy (Identity (), Identity ())) (Proxy :: Proxy (Except Identity Int)) pI pId pI eqIn
  , testLiftCatchLaws (Proxy :: Proxy (ReadOnlyT Identity Int)) (Proxy :: Proxy (Identity Int, Identity ())) (Proxy :: Proxy (Except Identity Int)) pI pId pI eqIn
  , testLiftCatchLaws (Proxy :: Proxy (StateT Identity Int))  (Proxy :: Proxy (Identity Int, Identity ())) (Proxy :: Proxy (Except Identity Int)) pI pId pI eqIn
  ]



test_all_MonadTrans_LiftDraft :: TestTree
test_all_MonadTrans_LiftDraft = testGroup "MonadTrans (LiftDraft)"
  [ testLiftDraftLaws (Proxy :: Proxy IdentityT)              (Proxy :: Proxy ((), Identity ())) (Proxy :: Proxy (WriteOnly Identity Int)) pI pId pI eqIn
  , testLiftDraftLaws (Proxy :: Proxy (HaltT Identity))                 (Proxy :: Proxy (Identity (), Identity ())) (Proxy :: Proxy (WriteOnly Identity Int)) pI pId pI eqIn

  , testLiftDraftLaws (Proxy :: Proxy (ExceptT Identity Int)) (Proxy :: Proxy (Identity (), Identity ())) (Proxy :: Proxy (WriteOnly Identity Int)) pI pId pI eqIn
  , testLiftDraftLaws (Proxy :: Proxy (ReadOnlyT Identity Int)) (Proxy :: Proxy (Identity Int, Identity ())) (Proxy :: Proxy (WriteOnly Identity Int)) pI pId pI eqIn
  , testLiftDraftLaws (Proxy :: Proxy (StateT Identity Int))  (Proxy :: Proxy (Identity Int, Identity ())) (Proxy :: Proxy (WriteOnly Identity Int)) pI pId pI eqIn
  ]



-- Test Functor, Applicative, and Monad instances for a concrete monad transformer over several concrete monads.
test_MonadTrans_FAM
  :: forall t wt
   . ( MonadTrans t, Typeable t
     , Show wt
       , forall u m
           . ( Show u, Typeable u, Typeable m
             , forall x. (Show x, Typeable x) => Show (m x) )
          => Show (t m u)
     , Arbitrary wt
       , forall u m
           . ( Monad m
             , Arbitrary u
             , forall x. (Arbitrary x) => Arbitrary (m x) )
          => Arbitrary (t m u)
     , forall u m wm
         . ( Monad m
           , Eq u
           , forall x. (Eq x) => EqIn wm (m x) )
        => EqIn (wt,wm) (t m u) )
  => Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (wt :: *)
  -> TestTree
test_MonadTrans_FAM proxyT proxyWT =
  testGroup "MonadTrans (FAM)"
    [ test_MonadTrans_Monad_FAM proxyT proxyWT (Proxy :: Proxy Identity) pU

    , test_MonadTrans_Monad_FAM proxyT proxyWT (Proxy :: Proxy Maybe)    pU

    , test_MonadTrans_Monad_FAM proxyT proxyWT (Proxy :: Proxy (State Identity Bool)) pIdB
    , test_MonadTrans_Monad_FAM proxyT proxyWT (Proxy :: Proxy (State Identity Int))  pIdI

    , test_MonadTrans_Monad_FAM proxyT proxyWT (Proxy :: Proxy (ReadOnly Identity Bool)) pIdB
    , test_MonadTrans_Monad_FAM proxyT proxyWT (Proxy :: Proxy (ReadOnly Identity Int))  pIdI

    , test_MonadTrans_Monad_FAM proxyT proxyWT (Proxy :: Proxy (WriteOnly Identity Bool)) pIdU
    , test_MonadTrans_Monad_FAM proxyT proxyWT (Proxy :: Proxy (WriteOnly Identity Int))  pIdU

    , test_MonadTrans_Monad_FAM proxyT proxyWT (Proxy :: Proxy (Except Identity Bool)) pIdU
    , test_MonadTrans_Monad_FAM proxyT proxyWT (Proxy :: Proxy (Except Identity Int))  pIdU
    ]



-- Test Functor, Applicative, and Monad instances for a concrete monad transformer over a specific concrete monad.
test_MonadTrans_Monad_FAM
  :: forall m t wm wt
   . ( Functor m, Applicative m, Monad m, Typeable m, MonadTrans t, Typeable t
     , Show wt, Show wm
       , forall u
           . ( Show u, Typeable u )
          => Show (t m u)
     , Arbitrary wt, Arbitrary wm
       , forall u
           . ( Arbitrary u )
          => Arbitrary (t m u)
     , forall u
         . ( Eq u )
        => EqIn (wt,wm) (t m u) )
  => Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (wt :: *)
  -> Proxy (m :: * -> *)
  -> Proxy (wm :: *)
  -> TestTree
test_MonadTrans_Monad_FAM proxyT proxyWT proxyM proxyWM =
  testGroup "MonadTrans Monad (FAM)"
    [ testFunctorLaws2     (Proxy :: Proxy (t m)) (pT2 proxyWT proxyWM) pB pI eqIn
    , testApplicativeLaws2 (Proxy :: Proxy (t m)) (pT2 proxyWT proxyWM) pB pI eqIn
    , testMonadLaws2       (Proxy :: Proxy (t m)) (pT2 proxyWT proxyWM) pB pI eqIn
    ]



-- Test Functor, Applicative, and Monad laws for a concrete monad transformer composed with several concrete monad transformers over several concrete monads.
test_MonadTrans_ComposeT_FAM
  :: forall t1 wt1
   . ( MonadTrans t1, Typeable t1
     , Show wt1
       , forall u m t2
           . ( Monad m, MonadTrans t2, Show u, Typeable m, Typeable t2, Typeable u
             , forall x. (Show x, Typeable x) => Show (t2 m x) )
          => Show (t1 (t2 m) u)
     , Arbitrary wt1
       , forall u m t2
           . ( Monad m, MonadTrans t2, Arbitrary u
             , forall x. (Arbitrary x) => Arbitrary (t2 m x) )
          => Arbitrary (t1 (t2 m) u)
     , forall u m wm t2 wt2
         . ( Monad m, MonadTrans t2, Eq u
           , forall x. (Eq x) => EqIn (wt2,wm) (t2 m x) )
        => EqIn (wt1,(wt2,wm)) (t1 (t2 m) u)
     )
  => Proxy (t1 :: (* -> *) -> * -> *)
  -> Proxy (wt1 :: *)
  -> TestTree
test_MonadTrans_ComposeT_FAM proxyT1 proxyWT1 = testGroup "ComposeT"
  [ test_MonadTrans_ComposeT_Monad_FAM proxyT1 proxyWT1 (Proxy :: Proxy IdentityT) pU

  , test_MonadTrans_ComposeT_Monad_FAM proxyT1 proxyWT1 (Proxy :: Proxy (HaltT Identity)) pIdU

  , test_MonadTrans_ComposeT_Monad_FAM proxyT1 proxyWT1 (Proxy :: Proxy (ExceptT Identity Bool)) pIdU
  , test_MonadTrans_ComposeT_Monad_FAM proxyT1 proxyWT1 (Proxy :: Proxy (ExceptT Identity Int))  pIdU

  , test_MonadTrans_ComposeT_Monad_FAM proxyT1 proxyWT1 (Proxy :: Proxy (WriteOnlyT Identity Bool)) pIdU
  , test_MonadTrans_ComposeT_Monad_FAM proxyT1 proxyWT1 (Proxy :: Proxy (WriteOnlyT Identity Int))  pIdU

  , test_MonadTrans_ComposeT_Monad_FAM proxyT1 proxyWT1 (Proxy :: Proxy (ReadOnlyT Identity Bool)) pIdB
  , test_MonadTrans_ComposeT_Monad_FAM proxyT1 proxyWT1 (Proxy :: Proxy (ReadOnlyT Identity Int))  pIdI

  , test_MonadTrans_ComposeT_Monad_FAM proxyT1 proxyWT1 (Proxy :: Proxy (StateT Identity Bool)) pIdB
  , test_MonadTrans_ComposeT_Monad_FAM proxyT1 proxyWT1 (Proxy :: Proxy (StateT Identity Int))  pIdI
  ]



-- Test Functor, Applicative, and Monad laws for the composite of two concrete monad transformers over several concrete monads.
test_MonadTrans_ComposeT_Monad_FAM
  :: forall t1 wt1 t2 wt2
   . ( MonadTrans t1, Typeable t1, MonadTrans t2, Typeable t2
     , Show wt1, Show wt2
       , forall u m
           . ( Monad m, Show u, Typeable u, Typeable m
             , forall x. (Show x, Typeable x) => Show (m x) )
          => Show (t1 (t2 m) u)
     , Arbitrary wt1, Arbitrary wt2
       , forall u m
           . ( Monad m, Arbitrary u
             , forall x. (Arbitrary x) => Arbitrary (m x) )
          => Arbitrary (t1 (t2 m) u)
     , forall u m wm
         . ( Monad m, Eq u
           , forall x. (Eq x) => EqIn wm (m x) )
        => EqIn (wt1,(wt2,wm)) (t1 (t2 m) u)
     )
  => Proxy (t1 :: (* -> *) -> * -> *)
  -> Proxy (wt1 :: *)
  -> Proxy (t2 :: (* -> *) -> * -> *)
  -> Proxy (wt2 :: *)
  -> TestTree
test_MonadTrans_ComposeT_Monad_FAM proxyT1 proxyWT1 proxyT2 proxyWT2 =
  testGroup "ComposeT"
    [ test_Monad_FAM (Proxy :: Proxy (ComposeT t1 t2 Identity)) (pT2 (pT2 proxyWT1 proxyWT2) pU)
    , test_Monad_FAM (Proxy :: Proxy (ComposeT t1 t2 Maybe))    (pT2 (pT2 proxyWT1 proxyWT2) pU)

    , test_Monad_FAM (Proxy :: Proxy (ComposeT t1 t2 (State Identity Bool))) (pT2 (pT2 proxyWT1 proxyWT2) pIdB)
    , test_Monad_FAM (Proxy :: Proxy (ComposeT t1 t2 (State Identity Int)))  (pT2 (pT2 proxyWT1 proxyWT2) pIdI)

    , test_Monad_FAM (Proxy :: Proxy (ComposeT t1 t2 (ReadOnly Identity Bool))) (pT2 (pT2 proxyWT1 proxyWT2) pIdB)
    , test_Monad_FAM (Proxy :: Proxy (ComposeT t1 t2 (ReadOnly Identity Int)))  (pT2 (pT2 proxyWT1 proxyWT2) pIdI)
    ]



-- Test the MonadTrans laws for a concrete monad transformer over several concrete monads.
test_MonadTrans_T
  :: forall t wt
   . ( MonadTrans t, Typeable t
     , Show wt
       , forall u m
           . ( Show u, Typeable u, Typeable m
             , forall x. (Show x, Typeable x) => Show (m x) )
          => Show (t m u)
     , Arbitrary wt
       , forall u m
           . ( Monad m, Arbitrary u
             , forall x. (Arbitrary x) => Arbitrary (m x) )
          => Arbitrary (t m u)
     , forall u m wm
         . ( Monad m, Eq u
           , forall x. (Eq x) => EqIn wm (m x) )
        => EqIn (wt,wm) (t m u) )
  => Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (wt :: *)
  -> TestTree
test_MonadTrans_T proxyT proxyWT =
  testGroup "MonadTrans"
    [ testGroup "T"
      [ testMonadTransLaws2 proxyT (Proxy :: Proxy Identity) (pT2 proxyWT pU) pB pI eqIn

      , testMonadTransLaws2 proxyT (Proxy :: Proxy Maybe) (pT2 proxyWT pU) pB pI eqIn

      , testMonadTransLaws2 proxyT (Proxy :: Proxy (State Identity Bool)) (pT2 proxyWT pIdB) pB pI eqIn
      , testMonadTransLaws2 proxyT (Proxy :: Proxy (State Identity Int))  (pT2 proxyWT pIdI) pB pI eqIn

      , testMonadTransLaws2 proxyT (Proxy :: Proxy (ReadOnly Identity Bool)) (pT2 proxyWT pIdB) pB pI eqIn
      , testMonadTransLaws2 proxyT (Proxy :: Proxy (ReadOnly Identity Int))  (pT2 proxyWT pIdI) pB pI eqIn

      , testMonadTransLaws2 proxyT (Proxy :: Proxy (WriteOnly Identity Bool)) (pT2 proxyWT pIdU) pB pI eqIn
      , testMonadTransLaws2 proxyT (Proxy :: Proxy (WriteOnly Identity Int))  (pT2 proxyWT pIdU) pB pI eqIn

      , testMonadTransLaws2 proxyT (Proxy :: Proxy (Except Identity Bool)) (pT2 proxyWT pIdU) pB pI eqIn
      , testMonadTransLaws2 proxyT (Proxy :: Proxy (Except Identity Int))  (pT2 proxyWT pIdU) pB pI eqIn
      ]
    ]



-- Test the MonadState laws for a concrete monad transformer over several concrete monads.
test_MonadTrans_State
  :: forall t wt s mark
   . ( MonadTrans t, MonadIdentity mark
     , Typeable t, Typeable mark, Typeable s
     , Eq (mark s), Show (mark s), Arbitrary (mark s), CoArbitrary (mark s)
     , Show wt
       , forall u m
           . ( Show u, Typeable u, Typeable m )
          => Show (t m u)
     , Arbitrary wt
       , forall u m
           . ( Monad m, Arbitrary u
             , forall x. (Arbitrary x) => Arbitrary (m x) )
          => Arbitrary (t m u)
     , forall u m wm
         . ( Monad m, Eq u
           , forall v. (Eq v) => EqIn wm (m v) )
        => EqIn (wt,wm) (t m u)
     , forall m
         . ( Monad m )
        => MonadState mark s (t m) )
  => Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (wt :: *)
  -> Proxy (mark :: * -> *)
  -> Proxy (s :: *)
  -> TestTree
test_MonadTrans_State proxyT proxyWT proxyMark proxyS =
  testGroup ""
    [ test_MonadTrans_Monad_State proxyT proxyWT proxyMark proxyS (Proxy :: Proxy Identity) pU

    , test_MonadTrans_Monad_State proxyT proxyWT proxyMark proxyS (Proxy :: Proxy Maybe) pU

    , test_MonadTrans_Monad_State proxyT proxyWT proxyMark proxyS (Proxy :: Proxy (Except Identity Bool)) pIdU
    , test_MonadTrans_Monad_State proxyT proxyWT proxyMark proxyS (Proxy :: Proxy (Except Identity Int))  pIdU
    ]



-- Test the MonadState laws for a concrete monad transformer and concrete monad over several value types.
test_MonadTrans_Monad_State
  :: forall m t wm wt s mark
   . ( Functor m, Applicative m, Monad m, MonadTrans t, MonadIdentity mark
     , Typeable m, Typeable t, Typeable mark, Typeable s
     , Eq (mark s), Show (mark s), Arbitrary (mark s), CoArbitrary (mark s)
     , Show wt, Show wm
       , forall u
           . ( Show u, Typeable u )
          => Show (t m u)
     , Arbitrary wt, Arbitrary wm
       , forall u
           . ( Arbitrary u )
          => Arbitrary (t m u)
     , forall u
         . ( Eq u )
        => EqIn (wt,wm) (t m u)
     , MonadState mark s (t m) )
  => Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (wt :: *)
  -> Proxy (mark :: * -> *)
  -> Proxy (s :: *)
  -> Proxy (m :: * -> *)
  -> Proxy (wm :: *)
  -> TestTree
test_MonadTrans_Monad_State proxyT proxyWT proxyMark proxyS proxyM proxyWM =
  testGroup "MonadTrans (FAM)"
    [ testStateMonadLaws (Proxy :: Proxy (t m)) (pT2 proxyWT proxyWM) (pAp proxyMark proxyS) pI eqIn get put
    , testStateMonadLaws (Proxy :: Proxy (t m)) (pT2 proxyWT proxyWM) (pAp proxyMark proxyS) pB eqIn get put
    ]



-- Test the MonadError laws for a concrete monad transformer over several concrete monads.
test_MonadTrans_Except
  :: forall t wt e mark
   . ( MonadTrans t, MonadIdentity mark
     , Typeable t, Typeable mark, Typeable e
     , Eq (mark e), Show (mark e), Arbitrary (mark e), CoArbitrary (mark e)
     , Show wt
       , forall u m
           . ( Show u, Typeable u, Typeable m
             , forall x. (Show x, Typeable x) => Show (m x) )
          => Show (t m u)
     , Arbitrary wt
       , forall u m
           . ( Monad m, Arbitrary u
             , forall x. (Arbitrary x) => Arbitrary (m x) )
          => Arbitrary (t m u)
     , forall u m wm
         . ( Monad m, Eq u
           , forall v. (Eq v) => EqIn wm (m v) )
        => EqIn (wt,wm) (t m u)
     , forall m
         . ( Monad m )
        => MonadExcept mark e (t m) )
  => Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (wt :: *)
  -> Proxy (mark :: * -> *)
  -> Proxy (e :: *)
  -> TestTree
test_MonadTrans_Except proxyT proxyWT proxyMark proxyS =
  testGroup ""
    [ test_MonadTrans_Monad_Except proxyT proxyWT proxyMark proxyS (Proxy :: Proxy Identity) pU

    , test_MonadTrans_Monad_Except proxyT proxyWT proxyMark proxyS (Proxy :: Proxy Maybe) pU

    , test_MonadTrans_Monad_Except proxyT proxyWT proxyMark proxyS (Proxy :: Proxy (Except Q Bool)) pQU
    , test_MonadTrans_Monad_Except proxyT proxyWT proxyMark proxyS (Proxy :: Proxy (Except Q Int))  pQU
    ]



-- Test the MonadError laws for a concrete monad transformer and concrete monad over several value types.
test_MonadTrans_Monad_Except
  :: forall m t wm wt e mark
   . ( Functor m, Applicative m, Monad m, MonadTrans t, MonadIdentity mark
     , Typeable m, Typeable t, Typeable mark, Typeable e
     , Eq (mark e), Show (mark e), Arbitrary (mark e), CoArbitrary (mark e)
     , Show wt, Show wm
       , forall u
           . ( Show u, Typeable u
             , forall x. (Show x, Typeable x) => Show (m x) )
          => Show (t m u)
     , Arbitrary wt, Arbitrary wm
       , forall u
           . ( Arbitrary u )
          => Arbitrary (t m u)
     , forall u
         . ( Eq u )
        => EqIn (wt,wm) (t m u)
     , MonadExcept mark e (t m) )
  => Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (wt :: *)
  -> Proxy (mark :: * -> *)
  -> Proxy (e :: *)
  -> Proxy (m :: * -> *)
  -> Proxy (wm :: *)
  -> TestTree
test_MonadTrans_Monad_Except proxyT proxyWT proxyMark proxyE proxyM proxyWM =
  testGroup "MonadTrans (FAM)"
    [ testErrorMonadLaws (Proxy :: Proxy (t m)) (pT2 proxyWT proxyWM) (pAp proxyMark proxyE) pI pI eqIn throw catch
    , testErrorMonadLaws (Proxy :: Proxy (t m)) (pT2 proxyWT proxyWM) (pAp proxyMark proxyE) pB pB eqIn throw catch
    ]



-- Test the MonadWriter laws for a concrete monad transformer over several concrete monads.
test_MonadTrans_WriteOnly
  :: forall t wt w mark
   . ( MonadTrans t, MonadIdentity mark
     , Typeable t, Typeable mark, Typeable w, Monoid (mark w)
     , Eq (mark w), Show (mark w), Arbitrary (mark w), CoArbitrary (mark w)
     , Show wt
       , forall u m
           . ( Show u, Typeable u, Typeable m
             , forall x. (Show x, Typeable x) => Show (m x) )
          => Show (t m u)
     , Arbitrary wt
       , forall u m
           . ( Monad m, Arbitrary u
             , forall x. (Arbitrary x) => Arbitrary (m x) )
          => Arbitrary (t m u)
     , forall u m wm
         . ( Monad m, Eq u
           , forall v. (Eq v) => EqIn wm (m v) )
        => EqIn (wt,wm) (t m u)
     , forall m
         . ( Monad m )
        => MonadWriteOnly mark w (t m) )
  => Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (wt :: *)
  -> Proxy (mark :: * -> *)
  -> Proxy (w :: *)
  -> TestTree
test_MonadTrans_WriteOnly proxyT proxyWT proxyMark proxyS =
  testGroup ""
    [ test_MonadTrans_Monad_WriteOnly proxyT proxyWT proxyMark proxyS (Proxy :: Proxy Identity) pU

    , test_MonadTrans_Monad_WriteOnly proxyT proxyWT proxyMark proxyS (Proxy :: Proxy Maybe) pU

    , test_MonadTrans_Monad_WriteOnly proxyT proxyWT proxyMark proxyS (Proxy :: Proxy (Except Q Bool)) pQU
    , test_MonadTrans_Monad_WriteOnly proxyT proxyWT proxyMark proxyS (Proxy :: Proxy (Except Q Int))  pQU
    ]



-- Test the MonadWriter laws for a concrete monad transformer and concrete monad over several value types.
test_MonadTrans_Monad_WriteOnly
  :: forall m t wm wt w mark
   . ( Functor m, Applicative m, Monad m, MonadTrans t, MonadIdentity mark
     , Typeable m, Typeable t, Typeable mark, Typeable w, Monoid (mark w)
     , Eq (mark w), Show (mark w), Arbitrary (mark w), CoArbitrary (mark w)
     , Show wt, Show wm
       , forall u. ( Show u, Typeable u ) => Show (m u)
       , forall u. ( Show u, Typeable u ) => Show (t m u)
     , Arbitrary wt, Arbitrary wm
       , forall u
           . ( Arbitrary u )
          => Arbitrary (t m u)
     , forall u
         . ( Eq u )
        => EqIn (wt,wm) (t m u)
     , MonadWriteOnly mark w (t m) )
  => Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (wt :: *)
  -> Proxy (mark :: * -> *)
  -> Proxy (w :: *)
  -> Proxy (m :: * -> *)
  -> Proxy (wm :: *)
  -> TestTree
test_MonadTrans_Monad_WriteOnly proxyT proxyWT proxyMark proxyE proxyM proxyWM =
  testGroup "MonadTrans (FAM)"
    [ testWriterMonadLaws (Proxy :: Proxy (t m)) (pT2 proxyWT proxyWM) (pAp proxyMark proxyE) pI pI eqIn tell (fixDraft draft)
    , testWriterMonadLaws (Proxy :: Proxy (t m)) (pT2 proxyWT proxyWM) (pAp proxyMark proxyE) pB pB eqIn tell (fixDraft draft)
    ]



-- Test the MonadReadOnly laws for a concrete monad transformer over several concrete monads.
test_MonadTrans_ReadOnly
  :: forall t wt r mark
   . ( MonadTrans t, MonadIdentity mark
     , Typeable t, Typeable mark, Typeable r
     , Eq (mark r), Show (mark r), Arbitrary (mark r), CoArbitrary (mark r)
     , Show wt
       , forall u m
           . ( Show u, Typeable u, Typeable m )
          => Show (t m u)
     , Arbitrary wt
       , forall u m
           . ( Monad m, Arbitrary u
             , forall x. (Arbitrary x) => Arbitrary (m x) )
          => Arbitrary (t m u)
     , forall u m wm
         . ( Monad m, Eq u
           , forall v. (Eq v) => EqIn wm (m v) )
        => EqIn (wt,wm) (t m u)
     , forall m
         . ( Monad m )
        => MonadReadOnly mark r (t m) )
  => Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (wt :: *)
  -> Proxy (mark :: * -> *)
  -> Proxy (r :: *)
  -> TestTree
test_MonadTrans_ReadOnly proxyT proxyWT proxyMark proxyR =
  testGroup ""
    [ test_MonadTrans_Monad_ReadOnly proxyT proxyWT proxyMark proxyR (Proxy :: Proxy Identity) pU

    , test_MonadTrans_Monad_ReadOnly proxyT proxyWT proxyMark proxyR (Proxy :: Proxy Maybe) pU

    , test_MonadTrans_Monad_ReadOnly proxyT proxyWT proxyMark proxyR (Proxy :: Proxy (Except Identity Bool)) pIdU
    , test_MonadTrans_Monad_ReadOnly proxyT proxyWT proxyMark proxyR (Proxy :: Proxy (Except Identity Int))  pIdU
    ]



-- Test the MonadReadOnly laws for a concrete monad transformer and concrete monad over several value types.
test_MonadTrans_Monad_ReadOnly
  :: forall m t wm wt r mark
   . ( Functor m, Applicative m, Monad m, MonadTrans t, MonadIdentity mark
     , Typeable m, Typeable t, Typeable mark, Typeable r
     , Eq (mark r), Show (mark r), Arbitrary (mark r), CoArbitrary (mark r)
     , Show wt, Show wm
       , forall u
           . ( Show u, Typeable u )
          => Show (t m u)
     , Arbitrary wt, Arbitrary wm
       , forall u
           . ( Arbitrary u )
          => Arbitrary (t m u)
     , forall u
         . ( Eq u )
        => EqIn (wt,wm) (t m u)
     , MonadReadOnly mark r (t m) )
  => Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (wt :: *)
  -> Proxy (mark :: * -> *)
  -> Proxy (r :: *)
  -> Proxy (m :: * -> *)
  -> Proxy (wm :: *)
  -> TestTree
test_MonadTrans_Monad_ReadOnly proxyT proxyWT proxyMark proxyR proxyM proxyWM =
  testGroup "MonadTrans (FAM)"
    [ testReaderMonadLaws (Proxy :: Proxy (t m)) (pT2 proxyWT proxyWM) (pAp proxyMark proxyR) pI pI eqIn ask local
    , testReaderMonadLaws (Proxy :: Proxy (t m)) (pT2 proxyWT proxyWM) (pAp proxyMark proxyR) pB pB eqIn ask local
    ]





{-------------------}
{- MonadTransTrans -}
{-------------------}

test_all_MonadTransTrans_FAM :: TestTree
test_all_MonadTransTrans_FAM = testGroup "All MonadTransTrans (FAM)"
  [ testGroup "IdentityTT"
    -- IdentityT
    [ test_MonadTransTrans_MonadTrans_FAM (Proxy :: Proxy IdentityTT) (Proxy :: Proxy Unit)
    ]

  , testGroup "PromptTT"
    [ test_MonadTransTrans_MonadTrans_FAM (Proxy :: Proxy (PromptTT Identity Identity)) (Proxy :: Proxy (Eval Identity))
    ]

  , testGroup "OverTT"
    [ test_MonadTransTrans_MonadTrans_FAM (Proxy :: Proxy (OverTT IdentityTT (IdentityT))) (pSS (Proxy :: Proxy Unit) pU)

    , test_MonadTransTrans_MonadTrans_FAM (Proxy :: Proxy (OverTT IdentityTT ((HaltT Identity)))) (pSS (Proxy :: Proxy Unit) pIdU)

    , test_MonadTransTrans_MonadTrans_FAM (Proxy :: Proxy (OverTT IdentityTT (ExceptT Identity Bool))) (pSS (Proxy :: Proxy Unit) pIdU)
    , test_MonadTransTrans_MonadTrans_FAM (Proxy :: Proxy (OverTT IdentityTT (ExceptT Identity Int)))  (pSS (Proxy :: Proxy Unit) pIdU)
    ]
  ]


pSS
  :: Proxy (z :: (* -> *) -> *)
  -> Proxy (a :: *)
  -> Proxy (Sing z a)
pSS _ _ = Proxy


test_all_MonadTransTrans_TT :: TestTree
test_all_MonadTransTrans_TT = testGroup "All MonadTransTrans (TT)"
  [ testMonadTransTransLaws (Proxy :: Proxy IdentityTT) (Proxy :: Proxy IdentityT) (Proxy :: Proxy Identity) (pT2 (pUnit pId) (pT2 pU pU)) pB pI eqIn
  ]



test_MonadTransTrans_MonadTrans_FAM
  :: ( MonadTransTrans u, Typeable u
       , forall m1. (Monad m1, Typeable m1) => Show (uw m1)
       , forall x m t
           . ( Show x, Monad m, MonadTrans t, Typeable m, Typeable t, Typeable x
             , forall y. (Show y, Typeable y) => Show (t m y) )
          => Show (u t m x)
       , forall m1. (Monad m1) => Arbitrary (uw m1)
       , forall x m t
           . ( Arbitrary x, Monad m, MonadTrans t
             , forall x. (Arbitrary x) => Arbitrary (t m x) )
          => Arbitrary (u t m x)
     , forall x m mw t tw
         . ( Eq x, Eq mw, Monad m, MonadTrans t
           , forall z. (Eq z) => EqIn (tw,mw) (t m z) )
        => EqIn (uw m, (tw,mw)) (u t m x) )
  => Proxy (u :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *)
  -> Proxy (uw :: (* -> *) -> *)
  -> TestTree
test_MonadTransTrans_MonadTrans_FAM proxyU proxyUW =
  testGroup "MonadTransTrans - MonadTrans (FAM)"
    [ test_MonadTransTrans_Monad_FAM proxyU proxyUW (Proxy :: Proxy IdentityT) (pU)

    , test_MonadTransTrans_Monad_FAM proxyU proxyUW (Proxy :: Proxy (HaltT Identity)) (pIdU)

    , test_MonadTransTrans_Monad_FAM proxyU proxyUW (Proxy :: Proxy (ExceptT Identity Bool)) (pIdU)
    , test_MonadTransTrans_Monad_FAM proxyU proxyUW (Proxy :: Proxy (ExceptT Identity Int))  (pIdU)

    , test_MonadTransTrans_Monad_FAM proxyU proxyUW (Proxy :: Proxy (WriteOnlyT Identity Bool)) (pIdU)
    , test_MonadTransTrans_Monad_FAM proxyU proxyUW (Proxy :: Proxy (WriteOnlyT Identity Int))  (pIdU)

    , test_MonadTransTrans_Monad_FAM proxyU proxyUW (Proxy :: Proxy (ReadOnlyT Identity Bool)) (pIdB)
    , test_MonadTransTrans_Monad_FAM proxyU proxyUW (Proxy :: Proxy (ReadOnlyT Identity Int))  (pIdI)

    , test_MonadTransTrans_Monad_FAM proxyU proxyUW (Proxy :: Proxy (StateT Identity Bool)) (pIdB)
    , test_MonadTransTrans_Monad_FAM proxyU proxyUW (Proxy :: Proxy (StateT Identity Int))  (pIdI)
    ]



test_MonadTransTrans_Monad_FAM
  :: ( MonadTrans t, Typeable t, MonadTransTrans u, Typeable u
     , Show tw
       , forall m1. (Monad m1, Typeable m1) => Show (uw m1)
       , forall x m
           . ( Show x, Monad m, Typeable m, Typeable x
             , forall y. (Show y, Typeable y) => Show (m y) )
          => Show (u t m x)
     , Arbitrary tw
       , forall m1. (Monad m1) => Arbitrary (uw m1)
       , forall x m
           . ( Arbitrary x, Monad m
             , forall x. (Arbitrary x) => Arbitrary (m x) )
          => Arbitrary (u t m x)
     , forall x m mw
         . ( Eq x, Eq mw, Monad m
           , forall z. (Eq z) => EqIn mw (m z)
           , forall z. (Eq z) => EqIn (tw,mw) (t m z) )
        => EqIn (uw m, (tw,mw)) (u t m x)
     , forall x mw mark. (Eq x, Eq mw, MonadIdentity mark) => (EqIn (tw, mark mw) (t (ReadOnly mark mw) x))
     , forall x mw mark. (Eq x, Eq mw, MonadIdentity mark, Monoid mw) => (EqIn (tw, mark ()) (t (WriteOnly mark mw) x))
     , forall x mw mark. (Eq x, Eq mw, MonadIdentity mark) => (EqIn (tw, mark ()) (t (Except mark mw) x))
     , forall x. (Eq x) => (EqIn (tw, ()) (t (Maybe) x))
     , forall x. (Eq x) => (EqIn (tw, ()) (t (Identity) x))
     , forall x mw mark. (Eq x, Eq mw, MonadIdentity mark) => (EqIn (tw, mark mw) (t (State mark mw) x)) )
  => Proxy (u :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *)
  -> Proxy (uw :: (* -> *) -> *)
  -> Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (tw :: *)
  -> TestTree
test_MonadTransTrans_Monad_FAM proxyU proxyUW proxyT proxyTW =
  testGroup "MonadTransTrans - Monad (FAM)"
    [ test_MonadTransTrans_FAM proxyU proxyUW proxyT proxyTW (Proxy :: Proxy Identity) pU

    , test_MonadTransTrans_FAM proxyU proxyUW proxyT proxyTW (Proxy :: Proxy Maybe) pU

    , test_MonadTransTrans_FAM proxyU proxyUW proxyT proxyTW (Proxy :: Proxy (Except Identity Bool)) pIdU
    , test_MonadTransTrans_FAM proxyU proxyUW proxyT proxyTW (Proxy :: Proxy (Except Identity Int))  pIdU

    , test_MonadTransTrans_FAM proxyU proxyUW proxyT proxyTW (Proxy :: Proxy (WriteOnly Identity Bool)) pIdU
    , test_MonadTransTrans_FAM proxyU proxyUW proxyT proxyTW (Proxy :: Proxy (WriteOnly Identity Int))  pIdU

    , test_MonadTransTrans_FAM proxyU proxyUW proxyT proxyTW (Proxy :: Proxy (State Identity Bool)) pIdB
    , test_MonadTransTrans_FAM proxyU proxyUW proxyT proxyTW (Proxy :: Proxy (State Identity Int))  pIdI

    , test_MonadTransTrans_FAM proxyU proxyUW proxyT proxyTW (Proxy :: Proxy (ReadOnly Identity Bool)) pIdB
    , test_MonadTransTrans_FAM proxyU proxyUW proxyT proxyTW (Proxy :: Proxy (ReadOnly Identity Int))  pIdI
    ]



test_MonadTransTrans_FAM
  :: ( Monad m, Typeable m, MonadTrans t, Typeable t, MonadTransTrans u, Typeable u
     , Show mw, Show tw
       , forall m1. (Monad m1, Typeable m1) => Show (uw m1)
       , forall x 
           . ( Show x, Typeable x )
          => Show (u t m x)
     , Arbitrary mw, Arbitrary tw
       , forall m1. (Monad m1) => Arbitrary (uw m1)
       , forall x
           . ( Arbitrary x )
          => Arbitrary (u t m x)
     , forall x
         . ( Eq x )
        => EqIn (uw m, (tw,mw)) (u t m x) )
  => Proxy (u :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *)
  -> Proxy (uw :: (* -> *) -> *)
  -> Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (tw :: *)
  -> Proxy (m :: * -> *)
  -> Proxy (mw :: *)
  -> TestTree
test_MonadTransTrans_FAM proxyU proxyUW proxyT proxyTW proxyM proxyMW =
  testGroup "Monad (FAM)"
    [ testFunctorLaws2     (pTT proxyU proxyT proxyM) (pT2 (pUU proxyUW proxyM) (pT2 proxyTW proxyMW)) pB pI eqIn
    , testApplicativeLaws2 (pTT proxyU proxyT proxyM) (pT2 (pUU proxyUW proxyM) (pT2 proxyTW proxyMW)) pB pI eqIn
    , testMonadLaws2       (pTT proxyU proxyT proxyM) (pT2 (pUU proxyUW proxyM) (pT2 proxyTW proxyMW)) pB pI eqIn
    ]



pTT
  :: Proxy (u :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *)
  -> Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (m :: * -> *)
  -> Proxy (u t m)
pTT _ _ _ = Proxy

pUU
  :: Proxy (uw :: (* -> *) -> *)
  -> Proxy (m :: * -> *)
  -> Proxy (uw m)
pUU _ _ = Proxy
