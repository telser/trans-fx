{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Data.Proxy
import Data.Typeable
import System.Environment

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.QuickCheck.Laws

import Control.FX
import Control.FX.IO
import Control.FX.Arbitrary

import Test.Tasty.QuickCheck.Laws.FX



main :: IO ()
main = do
  setEnv "TASTY_NUM_THREADS" "6"
  setEnv "TASTY_QUICKCHECK_TESTS" "100"
  defaultMain $ testGroup "Laws"
    [ testGroup "EqIn"
      [ test_all_Eq
      , test_all_EqIn
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
      , test_all_MonadTransTrans_FX
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

instance Commutant Q where
  commute (Q x) = fmap Q x

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

test_all_Eq :: TestTree
test_all_Eq = testGroup "All Eq"
  [ testEqLaws (Proxy :: Proxy (Context Identity))
  , testEqLaws (Proxy :: Proxy (Context (Halt Identity)))
  , testEqLaws (Proxy :: Proxy (Context (State Identity ())))
  , testEqLaws (Proxy :: Proxy (Context (ReadOnly Identity ())))
  , testEqLaws (Proxy :: Proxy (Context (WriteOnly Identity ())))
  , testEqLaws (Proxy :: Proxy (Context (AppendOnly Identity ())))
  , testEqLaws (Proxy :: Proxy (Context (WriteOnce Identity ())))
  , testEqLaws (Proxy :: Proxy (Context (Except Identity ())))

  , testEqLaws (Proxy :: Proxy (Input Identity))
  , testEqLaws (Proxy :: Proxy (Input (Halt Identity)))
  , testEqLaws (Proxy :: Proxy (Input (State Identity ())))
  , testEqLaws (Proxy :: Proxy (Input (ReadOnly Identity ())))
  , testEqLaws (Proxy :: Proxy (Input (WriteOnly Identity ())))
  , testEqLaws (Proxy :: Proxy (Input (AppendOnly Identity ())))
  , testEqLaws (Proxy :: Proxy (Input (WriteOnce Identity ())))
  , testEqLaws (Proxy :: Proxy (Input (Except Identity ())))

  , testEqLaws (Proxy :: Proxy (Output Identity ()))
  , testEqLaws (Proxy :: Proxy (Output (Halt Identity) ()))
  , testEqLaws (Proxy :: Proxy (Output (State Identity ()) ()))
  , testEqLaws (Proxy :: Proxy (Output (ReadOnly Identity ()) ()))
  , testEqLaws (Proxy :: Proxy (Output (WriteOnly Identity ()) ()))
  , testEqLaws (Proxy :: Proxy (Output (AppendOnly Identity ()) ()))
  , testEqLaws (Proxy :: Proxy (Output (WriteOnce Identity ()) ()))
  , testEqLaws (Proxy :: Proxy (Output (Except Identity ()) ()))
  ]

test_all_EqIn :: TestTree
test_all_EqIn = testGroup "All EqIn"
  [ testEqInLaws (Proxy :: Proxy (Identity)) (Proxy :: Proxy Int)
  , testEqInLaws (Proxy :: Proxy (Maybe)) (Proxy :: Proxy Int)
  , testEqInLaws (Proxy :: Proxy (Halt Identity)) (Proxy :: Proxy Int)

  , testEqInLaws (Proxy :: Proxy (Except Identity ())) (Proxy :: Proxy Int)
  , testEqInLaws (Proxy :: Proxy (WriteOnly Identity ())) (Proxy :: Proxy Int)
  , testEqInLaws (Proxy :: Proxy (ReadOnly Identity ())) (Proxy :: Proxy Int)
  , testEqInLaws (Proxy :: Proxy (State Identity ())) (Proxy :: Proxy Int)
  , testEqInLaws (Proxy :: Proxy (AppendOnly Identity ())) (Proxy :: Proxy Int)
  , testEqInLaws (Proxy :: Proxy (WriteOnly Identity ())) (Proxy :: Proxy Int)
  ]



{-----------}
{- Functor -}
{-----------}

test_all_Functor_F :: TestTree
test_all_Functor_F = testGroup "All Functor (F)"
  [ testFunctorLaws2 (Proxy :: Proxy (Pair Bool)) Proxy pB pI eqIn
  , testFunctorLaws2 (Proxy :: Proxy (Pair Int))  Proxy pB pI eqIn

  , testFunctorLaws2 (Proxy :: Proxy LeftZero)  Proxy pB pI eqIn
  , testFunctorLaws2 (Proxy :: Proxy RightZero) Proxy pB pI eqIn
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
  [ -- testBifunctorLaws (Proxy :: Proxy Pair) Proxy pI pI pI pI eqIn
--  , testBifunctorLaws (Proxy :: Proxy Either) pU pI pI pI pI eqIn
--  , testBifunctorLaws (Proxy :: Proxy (Except Identity)) (Proxy :: Proxy (Context (Except Identity Int))) pI pI pI pI eqIn
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
    [ testCommutantLaws proxyC (Proxy :: Proxy Identity) Proxy pI eqIn

    , testCommutantLaws proxyC (Proxy :: Proxy Maybe) Proxy pI eqIn

    , testCommutantLaws proxyC (Proxy :: Proxy (Halt Identity)) Proxy pI eqIn

    , testCommutantLaws proxyC (Proxy :: Proxy (Except Identity Bool)) Proxy pI eqIn
    , testCommutantLaws proxyC (Proxy :: Proxy (Except Identity Int))  Proxy pI eqIn

    , testCommutantLaws proxyC (Proxy :: Proxy (WriteOnly Identity Bool)) Proxy pI eqIn
    , testCommutantLaws proxyC (Proxy :: Proxy (WriteOnly Identity Int))  Proxy pI eqIn

    , testCommutantLaws proxyC (Proxy :: Proxy (State Identity Bool)) Proxy pI eqIn
    , testCommutantLaws proxyC (Proxy :: Proxy (State Identity Int))  Proxy pI eqIn

    , testCommutantLaws proxyC (Proxy :: Proxy (ReadOnly Identity Bool)) Proxy pI eqIn
    , testCommutantLaws proxyC (Proxy :: Proxy (ReadOnly Identity Int))  Proxy pI eqIn

    , testCommutantLaws proxyC (Proxy :: Proxy (AppendOnly Identity Bool)) Proxy pI eqIn
    , testCommutantLaws proxyC (Proxy :: Proxy (AppendOnly Identity Int))  Proxy pI eqIn

    , testCommutantLaws proxyC (Proxy :: Proxy (WriteOnce Identity Bool)) Proxy pI eqIn
    , testCommutantLaws proxyC (Proxy :: Proxy (WriteOnce Identity Int))  Proxy pI eqIn
    ]





{---------}
{- Monad -}
{---------}

-- Test Functor, Applicative, and Monad laws for several concrete monads.
test_all_Monad_FAM :: TestTree
test_all_Monad_FAM = testGroup "All Monad (FAM)"
  [ test_Monad_FAM (Proxy :: Proxy Identity)

  , test_Monad_FAM (Proxy :: Proxy (Halt Identity))

  , test_Monad_FAM (Proxy :: Proxy (Except Identity Bool))
  , test_Monad_FAM (Proxy :: Proxy (Except Identity Int))

  , test_Monad_FAM (Proxy :: Proxy (WriteOnly Identity Bool))
  , test_Monad_FAM (Proxy :: Proxy (WriteOnly Identity Int))

  , test_Monad_FAM (Proxy :: Proxy (State Identity Bool))
  , test_Monad_FAM (Proxy :: Proxy (State Identity Int))

  , test_Monad_FAM (Proxy :: Proxy (ReadOnly Identity Bool))
  , test_Monad_FAM (Proxy :: Proxy (ReadOnly Identity Int))

  , test_Monad_FAM (Proxy :: Proxy (AppendOnly Identity Bool))
  , test_Monad_FAM (Proxy :: Proxy (AppendOnly Identity Int))

  , test_Monad_FAM (Proxy :: Proxy (WriteOnce Identity Bool))
  , test_Monad_FAM (Proxy :: Proxy (WriteOnce Identity Int))
  ]



-- Test effect class laws for several concrete monads.
test_all_Monad_FX :: TestTree
test_all_Monad_FX = testGroup "All Monad (FX)"
  [ testStateMonadLaws (Proxy :: Proxy (State Identity Bool)) Proxy Proxy pI eqIn (get :: State Identity Bool (Identity Bool)) put
  , testStateMonadLaws (Proxy :: Proxy (State Identity Int))  Proxy Proxy pI eqIn (get :: State Identity Int (Identity Int)) put

  , testReaderMonadLaws (Proxy :: Proxy (ReadOnly Identity Bool)) Proxy Proxy pB pI eqIn (ask :: ReadOnly Identity Bool (Identity Bool)) local
  , testReaderMonadLaws (Proxy :: Proxy (ReadOnly Identity Int))  Proxy Proxy pB pI eqIn (ask :: ReadOnly Identity Int (Identity Int)) local

  , testWriterMonadLaws (Proxy :: Proxy (WriteOnly Identity Bool)) Proxy (pAp pId pB) pB pI eqIn tell (fixDraft draft)
  , testWriterMonadLaws (Proxy :: Proxy (WriteOnly Identity Int))  Proxy (pAp pId pI) pB pI eqIn tell (fixDraft draft)

  , testErrorMonadLaws (Proxy :: Proxy (Except Identity Bool)) Proxy (pAp pId pB) pB pI eqIn throw catch
  , testErrorMonadLaws (Proxy :: Proxy (Except Identity Int))  Proxy (pAp pId pI) pB pI eqIn throw catch

  , testAppendOnlyMonadLaws (Proxy :: Proxy (AppendOnly Identity Bool)) Proxy (pAp pId pB) pB pI eqIn look jot
  , testAppendOnlyMonadLaws (Proxy :: Proxy (AppendOnly Identity Int))  Proxy (pAp pId pI) pB pI eqIn look jot

  , testWriteOnceMonadLaws (Proxy :: Proxy (WriteOnce Identity Bool)) Proxy (pAp pId pB) pB pI eqIn press etch
  , testWriteOnceMonadLaws (Proxy :: Proxy (WriteOnce Identity Int))  Proxy (pAp pId pI) pB pI eqIn press etch
  ]



-- Test the Commutant class laws for several concrete functors over several applicative functors.
test_all_Monad_C :: TestTree
test_all_Monad_C = testGroup "All Functor (C)"
  [ test_Monad_C (Proxy :: Proxy Identity)

  , test_Monad_C (Proxy :: Proxy Maybe)

  , test_Monad_C (Proxy :: Proxy (Halt Identity))

  , test_Monad_C (Proxy :: Proxy (Except Identity Bool))
  , test_Monad_C (Proxy :: Proxy (Except Identity Int))

  , test_Monad_C (Proxy :: Proxy (WriteOnly Identity Bool))
  , test_Monad_C (Proxy :: Proxy (WriteOnly Identity Int))
  ]



-- Test Functor, Applicative, and Monad instances for a concrete monad.
test_Monad_FAM
  :: ( Functor m, Applicative m, Monad m, Typeable m
     , Show (Context m)
     , forall u. ( Show u, Typeable u ) => Show (m u)
     , Arbitrary (Context m)
     , forall u. (Arbitrary u) => Arbitrary (m u)
     , EqIn m )
  => Proxy (m :: * -> *)
  -> TestTree
test_Monad_FAM proxyM =
  testGroup "Monad (FAM)"
    [ testFunctorLaws2     proxyM Proxy pB pI eqIn
    , testApplicativeLaws2 proxyM Proxy pB pI eqIn
    , testMonadLaws2       proxyM Proxy pB pI eqIn
    ]

pContext
  :: Proxy (m :: * -> *)
  -> Proxy (Context m)
pContext _ = Proxy



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
    [ testCentralLaws proxyC (Proxy :: Proxy Identity) Proxy pI eqIn

    , testCentralLaws proxyC (Proxy :: Proxy Maybe) Proxy pI eqIn

    , testCentralLaws proxyC (Proxy :: Proxy (Halt Identity)) Proxy pI eqIn

    , testCentralLaws proxyC (Proxy :: Proxy (Except Identity Bool)) Proxy pI eqIn
    , testCentralLaws proxyC (Proxy :: Proxy (Except Identity Int))  Proxy pI eqIn

    , testCentralLaws proxyC (Proxy :: Proxy (WriteOnly Identity Bool)) Proxy pI eqIn
    , testCentralLaws proxyC (Proxy :: Proxy (WriteOnly Identity Int))  Proxy pI eqIn

    , testCentralLaws proxyC (Proxy :: Proxy (State Identity Bool)) Proxy pI eqIn
    , testCentralLaws proxyC (Proxy :: Proxy (State Identity Int))  Proxy pI eqIn

    , testCentralLaws proxyC (Proxy :: Proxy (ReadOnly Identity Bool)) Proxy pI eqIn
    , testCentralLaws proxyC (Proxy :: Proxy (ReadOnly Identity Int))  Proxy pI eqIn

    , testCentralLaws proxyC (Proxy :: Proxy (AppendOnly Identity Bool)) Proxy pI eqIn
    , testCentralLaws proxyC (Proxy :: Proxy (AppendOnly Identity Int))  Proxy pI eqIn

    , testCentralLaws proxyC (Proxy :: Proxy (WriteOnce Identity Bool)) Proxy pI eqIn
    , testCentralLaws proxyC (Proxy :: Proxy (WriteOnce Identity Int))  Proxy pI eqIn
    ]





{--------------}
{- MonadTrans -}
{--------------}

-- Test Functor, Applicative, and Monad laws for several concrete monad transformers over several concrete monads.
test_all_MonadTrans_FAM :: TestTree
test_all_MonadTrans_FAM = testGroup "All MonadTrans (FAM)"
  [ test_MonadTrans_FAM (Proxy :: Proxy IdentityT)

  , test_MonadTrans_FAM (Proxy :: Proxy (HaltT Identity))

  , test_MonadTrans_FAM (Proxy :: Proxy (ExceptT Identity Bool))
  , test_MonadTrans_FAM (Proxy :: Proxy (ExceptT Identity Int))

  , test_MonadTrans_FAM (Proxy :: Proxy (WriteOnlyT Identity Bool))
  , test_MonadTrans_FAM (Proxy :: Proxy (WriteOnlyT Identity Int))

  , test_MonadTrans_FAM (Proxy :: Proxy (StateT Identity Bool))
  , test_MonadTrans_FAM (Proxy :: Proxy (StateT Identity Int))

  , test_MonadTrans_FAM (Proxy :: Proxy (ReadOnlyT Identity Bool))
  , test_MonadTrans_FAM (Proxy :: Proxy (ReadOnlyT Identity Int))

  , test_MonadTrans_FAM (Proxy :: Proxy (AppendOnlyT Identity Bool))
  , test_MonadTrans_FAM (Proxy :: Proxy (AppendOnlyT Identity Int))

  , test_MonadTrans_FAM (Proxy :: Proxy (WriteOnceT Identity Bool))
  , test_MonadTrans_FAM (Proxy :: Proxy (WriteOnceT Identity Int))

  , testGroup "ComposeT"
    [ test_MonadTrans_ComposeT_FAM (Proxy :: Proxy IdentityT)

    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (HaltT Identity))

    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (ExceptT Identity Bool))
    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (ExceptT Identity Int))

    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (WriteOnlyT Identity Bool))
    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (WriteOnlyT Identity Int))

    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (ReadOnlyT Identity Bool))
    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (ReadOnlyT Identity Int))

    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (AppendOnlyT Identity Bool))
    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (AppendOnlyT Identity Int))

    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (WriteOnceT Identity Bool))
    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (WriteOnceT Identity Int))

    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (StateT Identity Bool))
    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (StateT Identity Int))
    ]
  ]



-- Test MonadTrans laws for several concrete monad transformers over several concrete monads.
test_all_MonadTrans_T :: TestTree
test_all_MonadTrans_T = testGroup "All MonadTrans (T)"
  [ test_MonadTrans_T (Proxy :: Proxy IdentityT)

  , test_MonadTrans_T (Proxy :: Proxy (HaltT Identity))

  , test_MonadTrans_T (Proxy :: Proxy (ExceptT Identity Bool))
  , test_MonadTrans_T (Proxy :: Proxy (ExceptT Identity Int))

  , test_MonadTrans_T (Proxy :: Proxy (WriteOnlyT Identity Bool))
  , test_MonadTrans_T (Proxy :: Proxy (WriteOnlyT Identity Int))

  , test_MonadTrans_T (Proxy :: Proxy (StateT Identity Bool))
  , test_MonadTrans_T (Proxy :: Proxy (StateT Identity Int))

  , test_MonadTrans_T (Proxy :: Proxy (ReadOnlyT Identity Bool))
  , test_MonadTrans_T (Proxy :: Proxy (ReadOnlyT Identity Int))

  , test_MonadTrans_T (Proxy :: Proxy (AppendOnlyT Identity Bool))
  , test_MonadTrans_T (Proxy :: Proxy (AppendOnlyT Identity Int))

  , test_MonadTrans_T (Proxy :: Proxy (WriteOnceT Identity Bool))
  , test_MonadTrans_T (Proxy :: Proxy (WriteOnceT Identity Int))
  ]



-- Test effect class laws for several concrete monad transformers over several concrete monads.
test_all_MonadTrans_FX :: TestTree
test_all_MonadTrans_FX = testGroup "All MonadTrans (FX)"
  [ testGroup "MonadTrans Monad (State)"
    -- Compose 1
    [ test_MonadTrans_State (Proxy :: Proxy (StateT Identity Bool)) pId pB

    -- Compose 2
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (StateT Identity Bool) IdentityT))           pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (StateT Identity Int)  IdentityT))           pId pI
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (StateT Identity Bool) (HaltT Q)))           pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (StateT Identity Int)  (HaltT Q)))           pId pI
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (StateT Identity Bool) (ReadOnlyT Q Int)))   pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (StateT Identity Int)  (ReadOnlyT Q Int)))   pId pI
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (StateT Identity Bool) (WriteOnlyT Q Int)))  pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (StateT Identity Int)  (WriteOnlyT Q Int)))  pId pI
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (StateT Identity Bool) (AppendOnlyT Q Int))) pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (StateT Identity Int)  (AppendOnlyT Q Int))) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (StateT Identity Bool) (WriteOnceT Q Int)))  pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (StateT Identity Int)  (WriteOnceT Q Int)))  pId pI
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (StateT Identity Bool) (StateT Q Int)))      pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (StateT Identity Int)  (StateT Q Int)))      pId pI

    , test_MonadTrans_State (Proxy :: Proxy (ComposeT IdentityT           (StateT Identity Bool))) pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT IdentityT           (StateT Identity Int)))  pId pI
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (HaltT Q)           (StateT Identity Bool))) pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (HaltT Q)           (StateT Identity Int)))  pId pI
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (StateT Identity Bool))) pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (StateT Identity Int)))  pId pI
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (StateT Identity Bool))) pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (StateT Identity Int)))  pId pI
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (StateT Identity Bool))) pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (StateT Identity Int)))  pId pI
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (StateT Identity Bool))) pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (StateT Identity Int)))  pId pI
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (StateT Q Int)      (StateT Identity Bool))) pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (StateT Q Int)      (StateT Identity Int)))  pId pI

    -- Compose 3
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (StateT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (StateT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (StateT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (StateT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (StateT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (StateT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (StateT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (StateT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (StateT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (StateT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (StateT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (StateT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (StateT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (StateT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (StateT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (StateT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (StateT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (StateT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (StateT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (StateT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (StateT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (StateT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (StateT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (StateT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (StateT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (StateT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (StateT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (StateT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (StateT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (StateT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (StateT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (StateT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (StateT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (StateT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (StateT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (StateT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (StateT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (StateT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (StateT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (StateT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (StateT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (StateT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (StateT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (StateT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (StateT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (StateT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (StateT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (StateT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (StateT Identity Bool) (StateT Q Int))))      pId pB
    ]

  , testGroup "MonadTrans Monad (Except)"
    -- Compose 1
    [ test_MonadTrans_Except (Proxy :: Proxy (ExceptT Identity Bool)) pId pB

    -- Compose 2
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ExceptT Identity Bool) IdentityT))           pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ExceptT Identity Int)  IdentityT))           pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ExceptT Identity Bool) (HaltT Q)))           pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ExceptT Identity Int)  (HaltT Q)))           pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ExceptT Identity Bool) (ReadOnlyT Q Int)))   pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ExceptT Identity Int)  (ReadOnlyT Q Int)))   pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ExceptT Identity Bool) (WriteOnlyT Q Int)))  pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ExceptT Identity Int)  (WriteOnlyT Q Int)))  pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ExceptT Identity Bool) (WriteOnceT Q Int)))  pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ExceptT Identity Int)  (WriteOnceT Q Int)))  pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ExceptT Identity Bool) (AppendOnlyT Q Int))) pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ExceptT Identity Int)  (AppendOnlyT Q Int))) pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ExceptT Identity Bool) (StateT Q Int)))      pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ExceptT Identity Int)  (StateT Q Int)))      pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ExceptT Identity Bool) (ExceptT Q Int)))     pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ExceptT Identity Int)  (ExceptT Q Int)))     pId pI

    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT IdentityT           (ExceptT Identity Bool))) pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT IdentityT           (ExceptT Identity Int)))  pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (HaltT Q)           (ExceptT Identity Bool))) pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (HaltT Q)           (ExceptT Identity Int)))  pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ExceptT Identity Bool))) pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ExceptT Identity Int)))  pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ExceptT Identity Bool))) pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ExceptT Identity Int)))  pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ExceptT Identity Bool))) pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ExceptT Identity Int)))  pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ExceptT Identity Bool))) pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ExceptT Identity Int)))  pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (StateT Q Int)      (ExceptT Identity Bool))) pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (StateT Q Int)      (ExceptT Identity Int)))  pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ExceptT Q Int)     (ExceptT Identity Bool))) pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ExceptT Q Int)     (ExceptT Identity Int)))  pId pI

    -- Compose 3
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (ExceptT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (ExceptT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (ExceptT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (ExceptT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (ExceptT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (ExceptT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (ExceptT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (ExceptT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (ExceptT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (ExceptT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (ExceptT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (ExceptT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (ExceptT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (ExceptT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (ExceptT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (ExceptT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (ExceptT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (ExceptT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (ExceptT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (ExceptT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (ExceptT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (ExceptT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (ExceptT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (ExceptT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (ExceptT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (ExceptT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (ExceptT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (ExceptT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (ExceptT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (ExceptT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (ExceptT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (ExceptT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (ExceptT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (ExceptT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (ExceptT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (ExceptT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (ExceptT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (ExceptT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (ExceptT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (ExceptT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (ExceptT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (ExceptT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (ExceptT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (ExceptT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (ExceptT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (ExceptT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (ExceptT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (ExceptT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (ExceptT Identity Bool) (StateT Q Int))))      pId pB
    ]

  , testGroup "MonadTrans Monad (WriteOnly)"
    -- Compose 1
    [ test_MonadTrans_WriteOnly (Proxy :: Proxy (WriteOnlyT Identity Bool)) pId pB

    -- Compose 2
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Identity Bool) IdentityT))           pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Identity Int)  IdentityT))           pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Identity Bool) (HaltT Q)))           pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Identity Int)  (HaltT Q)))           pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Identity Bool) (ReadOnlyT Q Int)))   pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Identity Int)  (ReadOnlyT Q Int)))   pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Identity Bool) (WriteOnlyT Q Int)))  pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Identity Int)  (WriteOnlyT Q Int)))  pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Identity Bool) (WriteOnceT Q Int)))  pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Identity Int)  (WriteOnceT Q Int)))  pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Identity Bool) (AppendOnlyT Q Int))) pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Identity Int)  (AppendOnlyT Q Int))) pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Identity Bool) (StateT Q Int)))      pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Identity Int)  (StateT Q Int)))      pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Identity Bool) (ExceptT Q Int)))     pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Identity Int)  (ExceptT Q Int)))     pId pI

    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT IdentityT           (WriteOnlyT Identity Bool))) pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT IdentityT           (WriteOnlyT Identity Int)))  pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (HaltT Q)           (WriteOnlyT Identity Bool))) pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (HaltT Q)           (WriteOnlyT Identity Int)))  pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (WriteOnlyT Identity Bool))) pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (WriteOnlyT Identity Int)))  pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (WriteOnlyT Identity Bool))) pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (WriteOnlyT Identity Int)))  pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (WriteOnlyT Identity Bool))) pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (WriteOnlyT Identity Int)))  pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (WriteOnlyT Identity Bool))) pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (WriteOnlyT Identity Int)))  pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (StateT Q Int)      (WriteOnlyT Identity Bool))) pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (StateT Q Int)      (WriteOnlyT Identity Int)))  pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (ExceptT Q Int)     (WriteOnlyT Identity Bool))) pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (ExceptT Q Int)     (WriteOnlyT Identity Int)))  pId pI

    -- Compose 3
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (WriteOnlyT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (WriteOnlyT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (WriteOnlyT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (WriteOnlyT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (WriteOnlyT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (WriteOnlyT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (WriteOnlyT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (WriteOnlyT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (WriteOnlyT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (WriteOnlyT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (WriteOnlyT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (WriteOnlyT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (WriteOnlyT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (WriteOnlyT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (WriteOnlyT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (WriteOnlyT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (WriteOnlyT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (WriteOnlyT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (WriteOnlyT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (WriteOnlyT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (WriteOnlyT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (WriteOnlyT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (WriteOnlyT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (WriteOnlyT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (WriteOnlyT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (WriteOnlyT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (WriteOnlyT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (WriteOnlyT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (WriteOnlyT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (WriteOnlyT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (WriteOnlyT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (WriteOnlyT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (WriteOnlyT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (WriteOnlyT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (WriteOnlyT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (WriteOnlyT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (WriteOnlyT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (WriteOnlyT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (WriteOnlyT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (WriteOnlyT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (WriteOnlyT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (WriteOnlyT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (WriteOnlyT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (WriteOnlyT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (WriteOnlyT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (WriteOnlyT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (WriteOnlyT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (WriteOnlyT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (WriteOnlyT Identity Bool) (StateT Q Int))))      pId pB
    ]

  , testGroup "MonadTrans Monad (ReadOnly)"
    -- Compose 1
    [ test_MonadTrans_ReadOnly (Proxy :: Proxy (ReadOnlyT Identity Bool)) pId pB

    -- Compose 2
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Identity Bool) IdentityT))           pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Identity Int)  IdentityT))           pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Identity Bool) (HaltT Q)))           pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Identity Int)  (HaltT Q)))           pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Identity Bool) (ReadOnlyT Q Int)))   pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Identity Int)  (ReadOnlyT Q Int)))   pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Identity Bool) (WriteOnlyT Q Int)))  pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Identity Int)  (WriteOnlyT Q Int)))  pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Identity Bool) (WriteOnceT Q Int)))  pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Identity Int)  (WriteOnceT Q Int)))  pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Identity Bool) (AppendOnlyT Q Int))) pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Identity Int)  (AppendOnlyT Q Int))) pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Identity Bool) (StateT Q Int)))      pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Identity Int)  (StateT Q Int)))      pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Identity Bool) (ExceptT Q Int)))     pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Identity Int)  (ExceptT Q Int)))     pId pI

    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT IdentityT           (ReadOnlyT Identity Bool))) pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT IdentityT           (ReadOnlyT Identity Int)))  pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (HaltT Q)           (ReadOnlyT Identity Bool))) pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (HaltT Q)           (ReadOnlyT Identity Int)))  pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ReadOnlyT Identity Bool))) pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ReadOnlyT Identity Int)))  pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ReadOnlyT Identity Bool))) pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ReadOnlyT Identity Int)))  pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ReadOnlyT Identity Bool))) pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ReadOnlyT Identity Int)))  pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ReadOnlyT Identity Bool))) pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ReadOnlyT Identity Int)))  pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (StateT Q Int)      (ReadOnlyT Identity Bool))) pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (StateT Q Int)      (ReadOnlyT Identity Int)))  pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (ExceptT Q Int)     (ReadOnlyT Identity Bool))) pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (ExceptT Q Int)     (ReadOnlyT Identity Int)))  pId pI

    -- Compose 3
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (ReadOnlyT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (ReadOnlyT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (ReadOnlyT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (ReadOnlyT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (ReadOnlyT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (ReadOnlyT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (ReadOnlyT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (ReadOnlyT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (ReadOnlyT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (ReadOnlyT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (ReadOnlyT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (ReadOnlyT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (ReadOnlyT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (ReadOnlyT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (ReadOnlyT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (ReadOnlyT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (ReadOnlyT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (ReadOnlyT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (ReadOnlyT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (ReadOnlyT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (ReadOnlyT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (ReadOnlyT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (ReadOnlyT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (ReadOnlyT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (ReadOnlyT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (ReadOnlyT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (ReadOnlyT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (ReadOnlyT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (ReadOnlyT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (ReadOnlyT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (ReadOnlyT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (ReadOnlyT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (ReadOnlyT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (ReadOnlyT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (ReadOnlyT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (ReadOnlyT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (ReadOnlyT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (ReadOnlyT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (ReadOnlyT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (ReadOnlyT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (ReadOnlyT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (ReadOnlyT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (ReadOnlyT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (ReadOnlyT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (ReadOnlyT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (ReadOnlyT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (ReadOnlyT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (ReadOnlyT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (ReadOnlyT Identity Bool) (StateT Q Int))))      pId pB
    ]

  , testGroup "MonadTrans Monad (AppendOnly)"
    -- Compose 1
    [ test_MonadTrans_AppendOnly (Proxy :: Proxy (AppendOnlyT Identity Bool)) pId pB

    -- Compose 2
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Identity Bool) IdentityT))           pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Identity Int)  IdentityT))           pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Identity Bool) (HaltT Q)))           pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Identity Int)  (HaltT Q)))           pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Identity Bool) (ReadOnlyT Q Int)))   pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Identity Int)  (ReadOnlyT Q Int)))   pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Identity Bool) (WriteOnlyT Q Int)))  pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Identity Int)  (WriteOnlyT Q Int)))  pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Identity Bool) (WriteOnceT Q Int)))  pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Identity Int)  (WriteOnceT Q Int)))  pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Identity Bool) (AppendOnlyT Q Int))) pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Identity Int)  (AppendOnlyT Q Int))) pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Identity Bool) (StateT Q Int)))      pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Identity Int)  (StateT Q Int)))      pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Identity Bool) (ExceptT Q Int)))     pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Identity Int)  (ExceptT Q Int)))     pId pI

    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT IdentityT           (AppendOnlyT Identity Bool))) pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT IdentityT           (AppendOnlyT Identity Int)))  pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (HaltT Q)           (AppendOnlyT Identity Bool))) pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (HaltT Q)           (AppendOnlyT Identity Int)))  pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (AppendOnlyT Identity Bool))) pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (AppendOnlyT Identity Int)))  pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (AppendOnlyT Identity Bool))) pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (AppendOnlyT Identity Int)))  pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (AppendOnlyT Identity Bool))) pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (AppendOnlyT Identity Int)))  pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (AppendOnlyT Identity Bool))) pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (AppendOnlyT Identity Int)))  pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (StateT Q Int)      (AppendOnlyT Identity Bool))) pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (StateT Q Int)      (AppendOnlyT Identity Int)))  pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (ExceptT Q Int)     (AppendOnlyT Identity Bool))) pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (ExceptT Q Int)     (AppendOnlyT Identity Int)))  pId pI

    -- Compose 3
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (AppendOnlyT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (AppendOnlyT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (AppendOnlyT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (AppendOnlyT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (AppendOnlyT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (AppendOnlyT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (AppendOnlyT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (AppendOnlyT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (AppendOnlyT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (AppendOnlyT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (AppendOnlyT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (AppendOnlyT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (AppendOnlyT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (AppendOnlyT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (AppendOnlyT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (AppendOnlyT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (AppendOnlyT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (AppendOnlyT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (AppendOnlyT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (AppendOnlyT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (AppendOnlyT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (AppendOnlyT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (AppendOnlyT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (AppendOnlyT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (AppendOnlyT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (AppendOnlyT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (AppendOnlyT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (AppendOnlyT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (AppendOnlyT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (AppendOnlyT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (AppendOnlyT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (AppendOnlyT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (AppendOnlyT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (AppendOnlyT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (AppendOnlyT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (AppendOnlyT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (AppendOnlyT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (AppendOnlyT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (AppendOnlyT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (AppendOnlyT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (AppendOnlyT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (AppendOnlyT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (AppendOnlyT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (AppendOnlyT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (AppendOnlyT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (AppendOnlyT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (AppendOnlyT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (AppendOnlyT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (AppendOnlyT Identity Bool) (StateT Q Int))))      pId pB
    ]

  , testGroup "MonadTrans Monad (WriteOnce)"
    -- Compose 1
    [ test_MonadTrans_WriteOnce (Proxy :: Proxy (WriteOnceT Identity Bool)) pId pB

    -- Compose 2
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnceT Identity Bool) IdentityT))           pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnceT Identity Int)  IdentityT))           pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnceT Identity Bool) (HaltT Q)))           pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnceT Identity Int)  (HaltT Q)))           pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnceT Identity Bool) (ReadOnlyT Q Int)))   pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnceT Identity Int)  (ReadOnlyT Q Int)))   pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnceT Identity Bool) (WriteOnlyT Q Int)))  pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnceT Identity Int)  (WriteOnlyT Q Int)))  pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnceT Identity Bool) (WriteOnceT Q Int)))  pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnceT Identity Int)  (WriteOnceT Q Int)))  pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnceT Identity Bool) (AppendOnlyT Q Int))) pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnceT Identity Int)  (AppendOnlyT Q Int))) pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnceT Identity Bool) (StateT Q Int)))      pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnceT Identity Int)  (StateT Q Int)))      pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnceT Identity Bool) (ExceptT Q Int)))     pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnceT Identity Int)  (ExceptT Q Int)))     pId pI

    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT IdentityT           (WriteOnceT Identity Bool))) pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT IdentityT           (WriteOnceT Identity Int)))  pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (HaltT Q)           (WriteOnceT Identity Bool))) pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (HaltT Q)           (WriteOnceT Identity Int)))  pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (WriteOnceT Identity Bool))) pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (WriteOnceT Identity Int)))  pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (WriteOnceT Identity Bool))) pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (WriteOnceT Identity Int)))  pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (WriteOnceT Identity Bool))) pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (WriteOnceT Identity Int)))  pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (WriteOnceT Identity Bool))) pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (WriteOnceT Identity Int)))  pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (StateT Q Int)      (WriteOnceT Identity Bool))) pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (StateT Q Int)      (WriteOnceT Identity Int)))  pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (ExceptT Q Int)     (WriteOnceT Identity Bool))) pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (ExceptT Q Int)     (WriteOnceT Identity Int)))  pId pI

    -- Compose 3
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (WriteOnceT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (WriteOnceT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (WriteOnceT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (WriteOnceT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (WriteOnceT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (WriteOnceT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT IdentityT           (ComposeT (WriteOnceT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (WriteOnceT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (WriteOnceT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (WriteOnceT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (WriteOnceT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (WriteOnceT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (WriteOnceT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (HaltT Q)           (ComposeT (WriteOnceT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (WriteOnceT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (WriteOnceT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (WriteOnceT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (WriteOnceT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (WriteOnceT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (WriteOnceT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (ReadOnlyT Q Int)   (ComposeT (WriteOnceT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (WriteOnceT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (WriteOnceT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (WriteOnceT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (WriteOnceT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (WriteOnceT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (WriteOnceT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnlyT Q Int)  (ComposeT (WriteOnceT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (WriteOnceT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (WriteOnceT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (WriteOnceT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (WriteOnceT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (WriteOnceT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (WriteOnceT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (AppendOnlyT Q Int) (ComposeT (WriteOnceT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (WriteOnceT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (WriteOnceT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (WriteOnceT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (WriteOnceT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (WriteOnceT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (WriteOnceT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (WriteOnceT Q Int)  (ComposeT (WriteOnceT Identity Bool) (StateT Q Int))))      pId pB

    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (WriteOnceT Identity Bool) IdentityT)))           pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (WriteOnceT Identity Bool) (HaltT Q))))           pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (WriteOnceT Identity Bool) (ReadOnlyT Q Int))))   pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (WriteOnceT Identity Bool) (WriteOnlyT Q Int))))  pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (WriteOnceT Identity Bool) (AppendOnlyT Q Int)))) pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (WriteOnceT Identity Bool) (WriteOnceT Q Int))))  pId pB
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ComposeT (StateT Q Int)      (ComposeT (WriteOnceT Identity Bool) (StateT Q Int))))      pId pB
    ]
  ]



test_all_MonadTrans_LiftCatch :: TestTree
test_all_MonadTrans_LiftCatch = testGroup "MonadTrans (LiftCatch)"
  [ testLiftCatchLaws (Proxy :: Proxy IdentityT)        Proxy (Proxy :: Proxy (Except Identity Int)) pI pId pI eqIn
  , testLiftCatchLaws (Proxy :: Proxy (HaltT Identity)) Proxy (Proxy :: Proxy (Except Identity Int)) pI pId pI eqIn

  , testLiftCatchLaws (Proxy :: Proxy (WriteOnlyT Identity Int))  Proxy (Proxy :: Proxy (Except Identity Int)) pI pId pI eqIn
  , testLiftCatchLaws (Proxy :: Proxy (ReadOnlyT Identity Int))   Proxy (Proxy :: Proxy (Except Identity Int)) pI pId pI eqIn
  , testLiftCatchLaws (Proxy :: Proxy (StateT Identity Int))      Proxy (Proxy :: Proxy (Except Identity Int)) pI pId pI eqIn
  , testLiftCatchLaws (Proxy :: Proxy (AppendOnlyT Identity Int)) Proxy (Proxy :: Proxy (Except Identity Int)) pI pId pI eqIn
  , testLiftCatchLaws (Proxy :: Proxy (WriteOnceT Identity Int))  Proxy (Proxy :: Proxy (Except Identity Int)) pI pId pI eqIn
  ]



test_all_MonadTrans_LiftDraft :: TestTree
test_all_MonadTrans_LiftDraft = testGroup "MonadTrans (LiftDraft)"
  [ testLiftDraftLaws (Proxy :: Proxy IdentityT)              (pContextT Proxy Proxy) (Proxy :: Proxy (WriteOnly Identity Int)) pI pId pI eqIn
  , testLiftDraftLaws (Proxy :: Proxy (HaltT Identity))       (pContextT Proxy Proxy) (Proxy :: Proxy (WriteOnly Identity Int)) pI pId pI eqIn

  , testLiftDraftLaws (Proxy :: Proxy (ExceptT Identity Int)) (pContextT Proxy Proxy) (Proxy :: Proxy (WriteOnly Identity Int)) pI pId pI eqIn
  , testLiftDraftLaws (Proxy :: Proxy (ReadOnlyT Identity Int)) (pContextT Proxy Proxy) (Proxy :: Proxy (WriteOnly Identity Int)) pI pId pI eqIn
  , testLiftDraftLaws (Proxy :: Proxy (StateT Identity Int))  (pContextT Proxy Proxy) (Proxy :: Proxy (WriteOnly Identity Int)) pI pId pI eqIn
  ]



-- Test Functor, Applicative, and Monad instances for a concrete monad transformer over several concrete monads.
test_MonadTrans_FAM
  :: forall t
   . ( MonadTrans t, Typeable t
     , forall m. Show (Context m) => Show (Context (t m))
     , forall m. (Arbitrary (Context m)) => Arbitrary (Context (t m))
       , forall u m
           . ( Show u, Typeable u, Typeable m
             , forall x. (Show x, Typeable x) => Show (m x) )
          => Show (t m u)
       , forall u m
           . ( Monad m
             , Arbitrary u
             , forall x. (Arbitrary x) => Arbitrary (m x) )
          => Arbitrary (t m u)
     , forall m. ( Monad m, EqIn m ) => EqIn (t m) )
  => Proxy (t :: (* -> *) -> * -> *)
  -> TestTree
test_MonadTrans_FAM proxyT =
  testGroup "MonadTrans (FAM)"
    [ test_MonadTrans_Monad_FAM proxyT (Proxy :: Proxy Identity)

    , test_MonadTrans_Monad_FAM proxyT (Proxy :: Proxy Maybe)

    , test_MonadTrans_Monad_FAM proxyT (Proxy :: Proxy (State Identity Bool))
    , test_MonadTrans_Monad_FAM proxyT (Proxy :: Proxy (State Identity Int))

    , test_MonadTrans_Monad_FAM proxyT (Proxy :: Proxy (ReadOnly Identity Bool))
    , test_MonadTrans_Monad_FAM proxyT (Proxy :: Proxy (ReadOnly Identity Int))

    , test_MonadTrans_Monad_FAM proxyT (Proxy :: Proxy (WriteOnly Identity Bool))
    , test_MonadTrans_Monad_FAM proxyT (Proxy :: Proxy (WriteOnly Identity Int))

    , test_MonadTrans_Monad_FAM proxyT (Proxy :: Proxy (Except Identity Bool))
    , test_MonadTrans_Monad_FAM proxyT (Proxy :: Proxy (Except Identity Int))

    , test_MonadTrans_Monad_FAM proxyT (Proxy :: Proxy (AppendOnly Identity Bool))
    , test_MonadTrans_Monad_FAM proxyT (Proxy :: Proxy (AppendOnly Identity Int))

    , test_MonadTrans_Monad_FAM proxyT (Proxy :: Proxy (WriteOnce Identity Bool))
    , test_MonadTrans_Monad_FAM proxyT (Proxy :: Proxy (WriteOnce Identity Int))
    ]



-- Test Functor, Applicative, and Monad instances for a concrete monad transformer over a specific concrete monad.
test_MonadTrans_Monad_FAM
  :: forall m t
   . ( Functor m, Applicative m, Monad m, Typeable m, MonadTrans t, Typeable t
     , Show (Context m), Show (Context (t m))
     , forall u. ( Show u, Typeable u ) => Show (t m u)
     , Arbitrary (Context m), Arbitrary (Context (t m))
     , forall u. ( Arbitrary u ) => Arbitrary (t m u)
     , EqIn (t m) )
  => Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (m :: * -> *)
  -> TestTree
test_MonadTrans_Monad_FAM proxyT proxyM =
  testGroup "MonadTrans Monad (FAM)"
    [ testFunctorLaws2     (Proxy :: Proxy (t m)) Proxy pB pI eqIn
    , testApplicativeLaws2 (Proxy :: Proxy (t m)) Proxy pB pI eqIn
    , testMonadLaws2       (Proxy :: Proxy (t m)) Proxy pB pI eqIn
    ]



-- Test Functor, Applicative, and Monad laws for a concrete monad transformer composed with several concrete monad transformers over several concrete monads.
test_MonadTrans_ComposeT_FAM
  :: forall t1
   . ( MonadTrans t1, Typeable t1, ComposableT t1
     , forall m t2. (Show (Context m), Show (Context (t2 m))) => Show (Context (t1 (t2 m)) )
     , forall m t2. (Arbitrary (Context m), Arbitrary (Context (t2 m))) => Arbitrary (Context (t1 (t2 m)) )
       , forall u m t2
           . ( Monad m, MonadTrans t2, Show u, Typeable m, Typeable t2, Typeable u
             , forall x. (Show x, Typeable x) => Show (t2 m x) )
          => Show (t1 (t2 m) u)
       , forall u m t2
           . ( Monad m, MonadTrans t2, Arbitrary u
             , forall x. (Arbitrary x) => Arbitrary (t2 m x) )
          => Arbitrary (t1 (t2 m) u)
     , forall m t2. ( Monad m, MonadTrans t2, EqIn (t2 m) ) => EqIn (t1 (t2 m))
     )
  => Proxy (t1 :: (* -> *) -> * -> *)
  -> TestTree
test_MonadTrans_ComposeT_FAM proxyT1 = testGroup "ComposeT"
  [ test_MonadTrans_ComposeT_Monad_FAM proxyT1 (Proxy :: Proxy IdentityT)

  , test_MonadTrans_ComposeT_Monad_FAM proxyT1 (Proxy :: Proxy (HaltT Identity))

  , test_MonadTrans_ComposeT_Monad_FAM proxyT1 (Proxy :: Proxy (ExceptT Identity Bool))
  , test_MonadTrans_ComposeT_Monad_FAM proxyT1 (Proxy :: Proxy (ExceptT Identity Int))

  , test_MonadTrans_ComposeT_Monad_FAM proxyT1 (Proxy :: Proxy (WriteOnlyT Identity Bool))
  , test_MonadTrans_ComposeT_Monad_FAM proxyT1 (Proxy :: Proxy (WriteOnlyT Identity Int))

  , test_MonadTrans_ComposeT_Monad_FAM proxyT1 (Proxy :: Proxy (ReadOnlyT Identity Bool))
  , test_MonadTrans_ComposeT_Monad_FAM proxyT1 (Proxy :: Proxy (ReadOnlyT Identity Int))

  , test_MonadTrans_ComposeT_Monad_FAM proxyT1 (Proxy :: Proxy (AppendOnlyT Identity Bool))
  , test_MonadTrans_ComposeT_Monad_FAM proxyT1 (Proxy :: Proxy (AppendOnlyT Identity Int))

  , test_MonadTrans_ComposeT_Monad_FAM proxyT1 (Proxy :: Proxy (WriteOnceT Identity Bool))
  , test_MonadTrans_ComposeT_Monad_FAM proxyT1 (Proxy :: Proxy (WriteOnceT Identity Int))

  , test_MonadTrans_ComposeT_Monad_FAM proxyT1 (Proxy :: Proxy (StateT Identity Bool))
  , test_MonadTrans_ComposeT_Monad_FAM proxyT1 (Proxy :: Proxy (StateT Identity Int))
  ]



-- Test Functor, Applicative, and Monad laws for the composite of two concrete monad transformers over several concrete monads.
test_MonadTrans_ComposeT_Monad_FAM
  :: forall t1 t2
   . ( MonadTrans t1, Typeable t1, MonadTrans t2, Typeable t2, ComposableT t1
     , forall m. (Show (Context m)) => Show (Context (t1 (t2 m)))
     , forall m. (Arbitrary (Context m)) => Arbitrary (Context (t1 (t2 m)))
       , forall u m
           . ( Monad m, Show u, Typeable u, Typeable m
             , forall x. (Show x, Typeable x) => Show (m x) )
          => Show (t1 (t2 m) u)
       , forall u m
           . ( Monad m, Arbitrary u
             , forall x. (Arbitrary x) => Arbitrary (m x) )
          => Arbitrary (t1 (t2 m) u)
     , forall m. ( Monad m, EqIn m ) => EqIn (t1 (t2 m))
     )
  => Proxy (t1 :: (* -> *) -> * -> *)
  -> Proxy (t2 :: (* -> *) -> * -> *)
  -> TestTree
test_MonadTrans_ComposeT_Monad_FAM proxyT1 proxyT2 =
  testGroup "ComposeT"
    [ test_Monad_FAM (Proxy :: Proxy (ComposeT t1 t2 Identity))
    , test_Monad_FAM (Proxy :: Proxy (ComposeT t1 t2 Maybe))

    , test_Monad_FAM (Proxy :: Proxy (ComposeT t1 t2 (State Identity Bool)))
    , test_Monad_FAM (Proxy :: Proxy (ComposeT t1 t2 (State Identity Int)))

    , test_Monad_FAM (Proxy :: Proxy (ComposeT t1 t2 (ReadOnly Identity Bool)))
    , test_Monad_FAM (Proxy :: Proxy (ComposeT t1 t2 (ReadOnly Identity Int)))

    , test_Monad_FAM (Proxy :: Proxy (ComposeT t1 t2 (WriteOnly Identity Bool)))
    , test_Monad_FAM (Proxy :: Proxy (ComposeT t1 t2 (WriteOnly Identity Int)))

    , test_Monad_FAM (Proxy :: Proxy (ComposeT t1 t2 (AppendOnly Identity Bool)))
    , test_Monad_FAM (Proxy :: Proxy (ComposeT t1 t2 (AppendOnly Identity Int)))

    , test_Monad_FAM (Proxy :: Proxy (ComposeT t1 t2 (WriteOnce Identity Bool)))
    , test_Monad_FAM (Proxy :: Proxy (ComposeT t1 t2 (WriteOnce Identity Int)))

    , test_Monad_FAM (Proxy :: Proxy (ComposeT t1 t2 (Except Identity Bool)))
    , test_Monad_FAM (Proxy :: Proxy (ComposeT t1 t2 (Except Identity Int)))
    ]



-- Test the MonadTrans laws for a concrete monad transformer over several concrete monads.
test_MonadTrans_T
  :: forall t
   . ( MonadTrans t, Typeable t
     , forall m. Show (Context m) => Show (Context (t m)), forall m. (Arbitrary (Context m)) => Arbitrary (Context (t m))
       , forall u m
           . ( Show u, Typeable u, Typeable m
             , forall x. (Show x, Typeable x) => Show (m x) )
          => Show (t m u)
       , forall u m
           . ( Monad m, Arbitrary u
             , forall x. (Arbitrary x) => Arbitrary (m x) )
          => Arbitrary (t m u)
     , forall m. ( Monad m, EqIn m ) => EqIn (t m) )
  => Proxy (t :: (* -> *) -> * -> *)
  -> TestTree
test_MonadTrans_T proxyT =
  testGroup "MonadTrans"
    [ testGroup "T"
      [ testMonadTransLaws2 proxyT (Proxy :: Proxy Identity) (Proxy :: Proxy (Context (t Identity))) pB pI eqIn

      , testMonadTransLaws2 proxyT (Proxy :: Proxy Maybe) (Proxy :: Proxy (Context (t Maybe))) pB pI eqIn

      , testMonadTransLaws2 proxyT (Proxy :: Proxy (State Identity Bool)) Proxy pB pI eqIn
      , testMonadTransLaws2 proxyT (Proxy :: Proxy (State Identity Int))  Proxy pB pI eqIn

      , testMonadTransLaws2 proxyT (Proxy :: Proxy (ReadOnly Identity Bool)) Proxy pB pI eqIn
      , testMonadTransLaws2 proxyT (Proxy :: Proxy (ReadOnly Identity Int)) Proxy pB pI eqIn

      , testMonadTransLaws2 proxyT (Proxy :: Proxy (WriteOnly Identity Bool)) Proxy pB pI eqIn
      , testMonadTransLaws2 proxyT (Proxy :: Proxy (WriteOnly Identity Int))  Proxy pB pI eqIn

      , testMonadTransLaws2 proxyT (Proxy :: Proxy (AppendOnly Identity Bool)) Proxy pB pI eqIn
      , testMonadTransLaws2 proxyT (Proxy :: Proxy (AppendOnly Identity Int))  Proxy pB pI eqIn

      , testMonadTransLaws2 proxyT (Proxy :: Proxy (WriteOnce Identity Bool)) Proxy pB pI eqIn
      , testMonadTransLaws2 proxyT (Proxy :: Proxy (WriteOnce Identity Int))  Proxy pB pI eqIn

      , testMonadTransLaws2 proxyT (Proxy :: Proxy (Except Identity Bool)) Proxy pB pI eqIn
      , testMonadTransLaws2 proxyT (Proxy :: Proxy (Except Identity Int))  Proxy pB pI eqIn
      ]
    ]



-- Test the MonadState laws for a concrete monad transformer over several concrete monads.
test_MonadTrans_State
  :: forall t s mark
   . ( MonadTrans t, MonadIdentity mark
     , Typeable t, Typeable mark, Typeable s
     , Eq (mark s), Show (mark s), Arbitrary (mark s), CoArbitrary (mark s)
     , forall m. (Show (Context m), Typeable m) => Show (Context (t m))
     , forall m. (Arbitrary (Context m), Monad m) => Arbitrary (Context (t m))
     , forall u m. ( Show u, Typeable u, Typeable m ) => Show (t m u)
     , forall u m. ( Monad m, Arbitrary u, forall x. (Arbitrary x)
         => Arbitrary (m x) ) => Arbitrary (t m u)
     , forall m. ( Monad m , EqIn m ) => EqIn (t m)
     , forall m. ( Monad m ) => MonadState mark s (t m) )
  => Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (mark :: * -> *)
  -> Proxy (s :: *)
  -> TestTree
test_MonadTrans_State proxyT proxyMark proxyS =
  testGroup "MonadTrans (State)"
    [ test_MonadTrans_Monad_State proxyT proxyMark proxyS (Proxy :: Proxy Identity)

    , test_MonadTrans_Monad_State proxyT proxyMark proxyS (Proxy :: Proxy Maybe)

    , test_MonadTrans_Monad_State proxyT proxyMark proxyS (Proxy :: Proxy (Except Identity Bool))
    , test_MonadTrans_Monad_State proxyT proxyMark proxyS (Proxy :: Proxy (Except Identity Int))

    , test_MonadTrans_Monad_State proxyT proxyMark proxyS (Proxy :: Proxy (ReadOnly Identity Bool))
    , test_MonadTrans_Monad_State proxyT proxyMark proxyS (Proxy :: Proxy (ReadOnly Identity Int))

    , test_MonadTrans_Monad_State proxyT proxyMark proxyS (Proxy :: Proxy (WriteOnly Identity Bool))
    , test_MonadTrans_Monad_State proxyT proxyMark proxyS (Proxy :: Proxy (WriteOnly Identity Int))

    , test_MonadTrans_Monad_State proxyT proxyMark proxyS (Proxy :: Proxy (AppendOnly Identity Bool))
    , test_MonadTrans_Monad_State proxyT proxyMark proxyS (Proxy :: Proxy (AppendOnly Identity Int))

    , test_MonadTrans_Monad_State proxyT proxyMark proxyS (Proxy :: Proxy (WriteOnce Identity Bool))
    , test_MonadTrans_Monad_State proxyT proxyMark proxyS (Proxy :: Proxy (WriteOnce Identity Int))
    ]



-- Test the MonadState laws for a concrete monad transformer and concrete monad over several value types.
test_MonadTrans_Monad_State
  :: forall m t s mark
   . ( Functor m, Applicative m, Monad m, MonadTrans t, MonadIdentity mark
     , Typeable m, Typeable t, Typeable mark, Typeable s
     , Show (Context (t m)), Arbitrary (Context (t m))
     , Eq (mark s), Show (mark s), Arbitrary (mark s), CoArbitrary (mark s)
       , forall u
           . ( Show u, Typeable u )
          => Show (t m u)
       , forall u
           . ( Arbitrary u )
          => Arbitrary (t m u)
     , EqIn (t m)
     , MonadState mark s (t m) )
  => Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (mark :: * -> *)
  -> Proxy (s :: *)
  -> Proxy (m :: * -> *)
  -> TestTree
test_MonadTrans_Monad_State proxyT proxyMark proxyS proxyM =
  testGroup "MonadTrans Monad (State)"
    [ testStateMonadLaws (Proxy :: Proxy (t m)) Proxy (pAp proxyMark proxyS) pI eqIn get put
    , testStateMonadLaws (Proxy :: Proxy (t m)) Proxy (pAp proxyMark proxyS) pB eqIn get put
    ]



-- Test the MonadError laws for a concrete monad transformer over several concrete monads.
test_MonadTrans_Except
  :: forall t e mark
   . ( MonadTrans t, MonadIdentity mark
     , Typeable t, Typeable mark, Typeable e
     , Eq (mark e), Show (mark e), Arbitrary (mark e), CoArbitrary (mark e)
     , forall m. (Show (Context m)) => Show (Context (t m))
     , forall m. (Arbitrary (Context m)) => Arbitrary (Context (t m))
       , forall u m
           . ( Show u, Typeable u, Typeable m
             , forall x. (Show x, Typeable x) => Show (m x) )
          => Show (t m u)
       , forall u m
           . ( Monad m, Arbitrary u
             , forall x. (Arbitrary x) => Arbitrary (m x) )
          => Arbitrary (t m u)
     , forall m. ( Monad m, EqIn m ) => EqIn (t m)
     , forall m. ( Monad m ) => MonadExcept mark e (t m) )
  => Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (mark :: * -> *)
  -> Proxy (e :: *)
  -> TestTree
test_MonadTrans_Except proxyT proxyMark proxyS =
  testGroup "MonadTrans (Except)"
    [ test_MonadTrans_Monad_Except proxyT proxyMark proxyS (Proxy :: Proxy Identity)

    , test_MonadTrans_Monad_Except proxyT proxyMark proxyS (Proxy :: Proxy Maybe)

    , test_MonadTrans_Monad_Except proxyT proxyMark proxyS (Proxy :: Proxy (Except Q Bool))
    , test_MonadTrans_Monad_Except proxyT proxyMark proxyS (Proxy :: Proxy (Except Q Int))

    , test_MonadTrans_Monad_Except proxyT proxyMark proxyS (Proxy :: Proxy (State Q Bool))
    , test_MonadTrans_Monad_Except proxyT proxyMark proxyS (Proxy :: Proxy (State Q Int))

    , test_MonadTrans_Monad_Except proxyT proxyMark proxyS (Proxy :: Proxy (ReadOnly Q Bool))
    , test_MonadTrans_Monad_Except proxyT proxyMark proxyS (Proxy :: Proxy (ReadOnly Q Int))

    , test_MonadTrans_Monad_Except proxyT proxyMark proxyS (Proxy :: Proxy (WriteOnly Q Bool))
    , test_MonadTrans_Monad_Except proxyT proxyMark proxyS (Proxy :: Proxy (WriteOnly Q Int))
    ]



-- Test the MonadError laws for a concrete monad transformer and concrete monad over several value types.
test_MonadTrans_Monad_Except
  :: forall m t e mark
   . ( Functor m, Applicative m, Monad m, MonadTrans t, MonadIdentity mark
     , Typeable m, Typeable t, Typeable mark, Typeable e
     , Show (Context (t m)), Arbitrary (Context (t m))
     , Eq (mark e), Show (mark e), Arbitrary (mark e), CoArbitrary (mark e)
       , forall u
           . ( Show u, Typeable u
             , forall x. (Show x, Typeable x) => Show (m x) )
          => Show (t m u)
       , forall u
           . ( Arbitrary u )
          => Arbitrary (t m u)
     , EqIn (t m)
     , MonadExcept mark e (t m) )
  => Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (mark :: * -> *)
  -> Proxy (e :: *)
  -> Proxy (m :: * -> *)
  -> TestTree
test_MonadTrans_Monad_Except proxyT proxyMark proxyE proxyM =
  testGroup "MonadTrans Monad (Except)"
    [ testErrorMonadLaws (Proxy :: Proxy (t m)) (pContextT proxyT proxyM) (pAp proxyMark proxyE) pI pI eqIn throw catch
    , testErrorMonadLaws (Proxy :: Proxy (t m)) (pContextT proxyT proxyM) (pAp proxyMark proxyE) pB pB eqIn throw catch
    ]



-- Test the MonadWriter laws for a concrete monad transformer over several concrete monads.
test_MonadTrans_WriteOnly
  :: forall t w mark
   . ( MonadTrans t, MonadIdentity mark
     , Typeable t, Typeable mark, Typeable w, Monoid (mark w)
     , Eq (mark w), Show (mark w), Arbitrary (mark w), CoArbitrary (mark w)
     , forall m. (Show (Context m), Typeable m) => Show (Context (t m))
     , forall m. (Arbitrary (Context m), Monad m) => Arbitrary (Context (t m))
       , forall u m
           . ( Show u, Typeable u, Typeable m
             , forall x. (Show x, Typeable x) => Show (m x) )
          => Show (t m u)
       , forall u m
           . ( Monad m, Arbitrary u
             , forall x. (Arbitrary x) => Arbitrary (m x) )
          => Arbitrary (t m u)
     , forall m. ( Monad m, EqIn m ) => EqIn (t m)
     , forall m. ( Monad m ) => MonadWriteOnly mark w (t m) )
  => Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (mark :: * -> *)
  -> Proxy (w :: *)
  -> TestTree
test_MonadTrans_WriteOnly proxyT proxyMark proxyS =
  testGroup "MonadTrans (WriteOnly)"
    [ test_MonadTrans_Monad_WriteOnly proxyT proxyMark proxyS (Proxy :: Proxy Identity)

    , test_MonadTrans_Monad_WriteOnly proxyT proxyMark proxyS (Proxy :: Proxy Maybe)

    , test_MonadTrans_Monad_WriteOnly proxyT proxyMark proxyS (Proxy :: Proxy (Except Q Bool))
    , test_MonadTrans_Monad_WriteOnly proxyT proxyMark proxyS (Proxy :: Proxy (Except Q Int))
    ]



-- Test the MonadWriter laws for a concrete monad transformer and concrete monad over several value types.
test_MonadTrans_Monad_WriteOnly
  :: forall m t w mark
   . ( Functor m, Applicative m, Monad m, MonadTrans t, MonadIdentity mark
     , Typeable m, Typeable t, Typeable mark, Typeable w, Monoid (mark w)
     , Show (Context (t m)), Arbitrary (Context (t m))
     , Eq (mark w), Show (mark w), Arbitrary (mark w), CoArbitrary (mark w)
       , forall u. ( Show u, Typeable u ) => Show (m u)
       , forall u. ( Show u, Typeable u ) => Show (t m u)
       , forall u. ( Arbitrary u ) => Arbitrary (t m u)
     , EqIn (t m)
     , MonadWriteOnly mark w (t m) )
  => Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (mark :: * -> *)
  -> Proxy (w :: *)
  -> Proxy (m :: * -> *)
  -> TestTree
test_MonadTrans_Monad_WriteOnly proxyT proxyMark proxyE proxyM =
  testGroup "MonadTrans Monad (WriteOnly)"
    [ testWriterMonadLaws (Proxy :: Proxy (t m)) (pContextT proxyT proxyM) (pAp proxyMark proxyE) pI pI eqIn tell (fixDraft draft)
    , testWriterMonadLaws (Proxy :: Proxy (t m)) (pContextT proxyT proxyM) (pAp proxyMark proxyE) pB pB eqIn tell (fixDraft draft)
    ]



-- Test the MonadAppendOnly laws for a concrete monad transformer over several concrete monads.
test_MonadTrans_AppendOnly
  :: forall t w mark
   . ( MonadTrans t, MonadIdentity mark
     , Typeable t, Typeable mark, Typeable w, Monoid (mark w)
     , Eq (mark w), Show (mark w), Arbitrary (mark w), CoArbitrary (mark w)
     , forall m. (Show (Context m), Typeable m) => Show (Context (t m))
     , forall m. (Arbitrary (Context m), Monad m) => Arbitrary (Context (t m))
       , forall u m
           . ( Show u, Typeable u, Typeable m
             , forall x. (Show x, Typeable x) => Show (m x) )
          => Show (t m u)
       , forall u m
           . ( Monad m, Arbitrary u
             , forall x. (Arbitrary x) => Arbitrary (m x) )
          => Arbitrary (t m u)
     , forall m. ( Monad m, EqIn m ) => EqIn (t m)
     , forall m. ( Monad m ) => MonadAppendOnly mark w (t m) )
  => Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (mark :: * -> *)
  -> Proxy (w :: *)
  -> TestTree
test_MonadTrans_AppendOnly proxyT proxyMark proxyS =
  testGroup "MonadTrans (AppendOnly)"
    [ test_MonadTrans_Monad_AppendOnly proxyT proxyMark proxyS (Proxy :: Proxy Identity)

    , test_MonadTrans_Monad_AppendOnly proxyT proxyMark proxyS (Proxy :: Proxy Maybe)

    , test_MonadTrans_Monad_AppendOnly proxyT proxyMark proxyS (Proxy :: Proxy (Except Q Bool))
    , test_MonadTrans_Monad_AppendOnly proxyT proxyMark proxyS (Proxy :: Proxy (Except Q Int))
    ]



-- Test the MonadAppendOnly laws for a concrete monad transformer and concrete monad over several value types.
test_MonadTrans_Monad_AppendOnly
  :: forall m t w mark
   . ( Functor m, Applicative m, Monad m, MonadTrans t, MonadIdentity mark
     , Typeable m, Typeable t, Typeable mark, Typeable w, Monoid (mark w)
     , Show (Context (t m)), Arbitrary (Context (t m))
     , Eq (mark w), Show (mark w), Arbitrary (mark w), CoArbitrary (mark w)
       , forall u. ( Show u, Typeable u ) => Show (m u)
       , forall u. ( Show u, Typeable u ) => Show (t m u)
       , forall u. ( Arbitrary u ) => Arbitrary (t m u)
     , EqIn (t m)
     , MonadAppendOnly mark w (t m) )
  => Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (mark :: * -> *)
  -> Proxy (w :: *)
  -> Proxy (m :: * -> *)
  -> TestTree
test_MonadTrans_Monad_AppendOnly proxyT proxyMark proxyE proxyM =
  testGroup "MonadTrans Monad (AppendOnly)"
    [ testAppendOnlyMonadLaws (Proxy :: Proxy (t m)) (pContextT proxyT proxyM) (pAp proxyMark proxyE) pI pI eqIn look jot
    , testAppendOnlyMonadLaws (Proxy :: Proxy (t m)) (pContextT proxyT proxyM) (pAp proxyMark proxyE) pB pB eqIn look jot
    ]



-- Test the MonadWriteOnce laws for a concrete monad transformer over several concrete monads.
test_MonadTrans_WriteOnce
  :: forall t w mark
   . ( MonadTrans t, MonadIdentity mark
     , Typeable t, Typeable mark, Typeable w, Monoid (mark w)
     , Eq (mark w), Show (mark w), Arbitrary (mark w), CoArbitrary (mark w)
     , forall m. (Show (Context m), Typeable m) => Show (Context (t m))
     , forall m. (Arbitrary (Context m), Monad m) => Arbitrary (Context (t m))
       , forall u m
           . ( Show u, Typeable u, Typeable m
             , forall x. (Show x, Typeable x) => Show (m x) )
          => Show (t m u)
       , forall u m
           . ( Monad m, Arbitrary u
             , forall x. (Arbitrary x) => Arbitrary (m x) )
          => Arbitrary (t m u)
     , forall m. ( Monad m, EqIn m ) => EqIn (t m)
     , forall m. ( Monad m ) => MonadWriteOnce mark w (t m) )
  => Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (mark :: * -> *)
  -> Proxy (w :: *)
  -> TestTree
test_MonadTrans_WriteOnce proxyT proxyMark proxyS =
  testGroup "MonadTrans (WriteOnce)"
    [ test_MonadTrans_Monad_WriteOnce proxyT proxyMark proxyS (Proxy :: Proxy Identity)

    , test_MonadTrans_Monad_WriteOnce proxyT proxyMark proxyS (Proxy :: Proxy Maybe)

    , test_MonadTrans_Monad_WriteOnce proxyT proxyMark proxyS (Proxy :: Proxy (Except Q Bool))
    , test_MonadTrans_Monad_WriteOnce proxyT proxyMark proxyS (Proxy :: Proxy (Except Q Int))
    ]

-- Test the MonadWriteOnce laws for a concrete monad transformer and concrete monad over several value types.
test_MonadTrans_Monad_WriteOnce
  :: forall m t w mark
   . ( Functor m, Applicative m, Monad m, MonadTrans t, MonadIdentity mark
     , Typeable m, Typeable t, Typeable mark, Typeable w, Monoid (mark w)
     , Show (Context (t m)), Arbitrary (Context (t m))
     , Eq (mark w), Show (mark w), Arbitrary (mark w), CoArbitrary (mark w)
       , forall u. ( Show u, Typeable u ) => Show (m u)
       , forall u. ( Show u, Typeable u ) => Show (t m u)
       , forall u. ( Arbitrary u ) => Arbitrary (t m u)
     , EqIn (t m)
     , MonadWriteOnce mark w (t m) )
  => Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (mark :: * -> *)
  -> Proxy (w :: *)
  -> Proxy (m :: * -> *)
  -> TestTree
test_MonadTrans_Monad_WriteOnce proxyT proxyMark proxyE proxyM =
  testGroup "MonadTrans Monad (WriteOnce)"
    [ testWriteOnceMonadLaws (Proxy :: Proxy (t m)) (pContextT proxyT proxyM) (pAp proxyMark proxyE) pI pI eqIn press etch
    , testWriteOnceMonadLaws (Proxy :: Proxy (t m)) (pContextT proxyT proxyM) (pAp proxyMark proxyE) pB pB eqIn press etch
    ]



-- Test the MonadReadOnly laws for a concrete monad transformer over several concrete monads.
test_MonadTrans_ReadOnly
  :: forall t r mark
   . ( MonadTrans t, MonadIdentity mark
     , Typeable t, Typeable mark, Typeable r
     , Eq (mark r), Show (mark r), Arbitrary (mark r), CoArbitrary (mark r)
       , forall u m
           . ( Show u, Typeable u, Typeable m )
          => Show (t m u)
       , forall u m
           . ( Monad m, Arbitrary u
             , forall x. (Arbitrary x) => Arbitrary (m x) )
          => Arbitrary (t m u)
     , forall m. (Show (Context m), Typeable m) => Show (Context (t m))
     , forall m. (Arbitrary (Context m), Monad m) => Arbitrary (Context (t m))
     , forall m
         . ( Monad m
           , EqIn m )
        => EqIn (t m)
     , forall m
         . ( Monad m )
        => MonadReadOnly mark r (t m) )
  => Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (mark :: * -> *)
  -> Proxy (r :: *)
  -> TestTree
test_MonadTrans_ReadOnly proxyT proxyMark proxyR =
  testGroup "MonadTrans (ReadOnly)"
    [ test_MonadTrans_Monad_ReadOnly proxyT proxyMark proxyR (Proxy :: Proxy Identity)

    , test_MonadTrans_Monad_ReadOnly proxyT proxyMark proxyR (Proxy :: Proxy Maybe)

    , test_MonadTrans_Monad_ReadOnly proxyT proxyMark proxyR (Proxy :: Proxy (Except Identity Bool))
    , test_MonadTrans_Monad_ReadOnly proxyT proxyMark proxyR (Proxy :: Proxy (Except Identity Int))

    , test_MonadTrans_Monad_ReadOnly proxyT proxyMark proxyR (Proxy :: Proxy (ReadOnly Identity Bool))
    , test_MonadTrans_Monad_ReadOnly proxyT proxyMark proxyR (Proxy :: Proxy (ReadOnly Identity Int))
    ]



-- Test the MonadReadOnly laws for a concrete monad transformer and concrete monad over several value types.
test_MonadTrans_Monad_ReadOnly
  :: forall m t r mark
   . ( Functor m, Applicative m, Monad m, MonadTrans t, MonadIdentity mark
     , Typeable m, Typeable t, Typeable mark, Typeable r
     , Show (Context (t m)), Arbitrary (Context (t m)), Show (Context m)
     , Eq (mark r), Show (mark r), Arbitrary (mark r), CoArbitrary (mark r)
       , forall u
           . ( Show u, Typeable u )
          => Show (t m u)
       , forall u
           . ( Arbitrary u )
          => Arbitrary (t m u)
     , EqIn (t m)
     , MonadReadOnly mark r (t m) )
  => Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (mark :: * -> *)
  -> Proxy (r :: *)
  -> Proxy (m :: * -> *)
  -> TestTree
test_MonadTrans_Monad_ReadOnly proxyT proxyMark proxyR proxyM =
  testGroup "MonadTrans Monad (ReadOnly)"
    [ testReaderMonadLaws (Proxy :: Proxy (t m)) (pContextT proxyT proxyM) (pAp proxyMark proxyR) pI pI eqIn ask local
    , testReaderMonadLaws (Proxy :: Proxy (t m)) (pContextT proxyT proxyM) (pAp proxyMark proxyR) pB pB eqIn ask local
    ]

pContextT
  :: Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (m :: * -> *)
  -> Proxy (Context (t m))
pContextT _ _ = Proxy





{-------------------}
{- MonadTransTrans -}
{-------------------}

test_all_MonadTransTrans_FAM :: TestTree
test_all_MonadTransTrans_FAM = testGroup "All MonadTransTrans (FAM)"
  [ testGroup "MonadTransTrans (IdentityTT)"
    -- IdentityT
    [ test_MonadTransTrans_MonadTrans_FAM (Proxy :: Proxy IdentityTT)
    ]

  , testGroup "MonadTransTrans (PromptTT)"
    [ test_MonadTransTrans_MonadTrans_FAM (Proxy :: Proxy (PromptTT Identity Identity))
    ]

  , testGroup "MonadTransTrans (StateTT)"
    [ test_MonadTransTrans_MonadTrans_FAM (Proxy :: Proxy (StateTT Identity Int))
    , test_MonadTransTrans_MonadTrans_FAM (Proxy :: Proxy (StateTT Identity Bool))
    ]

  , testGroup "MonadTransTrans (ReadOnlyTT)"
    [ test_MonadTransTrans_MonadTrans_FAM (Proxy :: Proxy (ReadOnlyTT Identity Int))
    , test_MonadTransTrans_MonadTrans_FAM (Proxy :: Proxy (ReadOnlyTT Identity Bool))
    ]

  , testGroup "MonadTransTrans (WriteOnlyTT)"
    [ test_MonadTransTrans_MonadTrans_FAM (Proxy :: Proxy (WriteOnlyTT Identity Int))
    , test_MonadTransTrans_MonadTrans_FAM (Proxy :: Proxy (WriteOnlyTT Identity Bool))
    ]

  , testGroup "MonadTransTrans (AppendOnlyTT)"
    [ test_MonadTransTrans_MonadTrans_FAM (Proxy :: Proxy (AppendOnlyTT Identity Int))
    , test_MonadTransTrans_MonadTrans_FAM (Proxy :: Proxy (AppendOnlyTT Identity Bool))
    ]

  , testGroup "MonadTransTrans (WriteOnceTT)"
    [ test_MonadTransTrans_MonadTrans_FAM (Proxy :: Proxy (WriteOnceTT Identity Int))
    , test_MonadTransTrans_MonadTrans_FAM (Proxy :: Proxy (WriteOnceTT Identity Bool))
    ]

  , testGroup "MonadTransTrans (ExceptTT)"
    [ test_MonadTransTrans_MonadTrans_FAM (Proxy :: Proxy (ExceptTT Identity Int))
    , test_MonadTransTrans_MonadTrans_FAM (Proxy :: Proxy (ExceptTT Identity Bool))
    ]

  , testGroup "MonadTransTrans (OverTT)"
    [ test_MonadTransTrans_MonadTrans_FAM (Proxy :: Proxy (OverTT IdentityTT (IdentityT)))

    , test_MonadTransTrans_MonadTrans_FAM (Proxy :: Proxy (OverTT IdentityTT ((HaltT Identity))))

    , test_MonadTransTrans_MonadTrans_FAM (Proxy :: Proxy (OverTT IdentityTT (ExceptT Identity Bool)))
    , test_MonadTransTrans_MonadTrans_FAM (Proxy :: Proxy (OverTT IdentityTT (ExceptT Identity Int)))
    ]

  , testGroup "MonadTransTrans (TeletypeTT)"
    [ test_MonadTransTrans_MonadTrans_FAM (Proxy :: Proxy (TeletypeTT Identity))
    , test_MonadTransTrans_MonadTrans_FAM (Proxy :: Proxy (TeletypeTT Identity))
    ]
  ]



test_all_MonadTransTrans_TT :: TestTree
test_all_MonadTransTrans_TT = testGroup "All MonadTransTrans (TT)"
  [ testMonadTransTransLaws (Proxy :: Proxy IdentityTT) (Proxy :: Proxy IdentityT) (Proxy :: Proxy Identity) (pContextTT Proxy Proxy Proxy) pB pI eqIn
  ]



test_MonadTransTrans_MonadTrans_FAM
  :: ( MonadTransTrans u, Typeable u
     , forall m t. (Typeable m, Typeable t, Show (Context (t m))) => Show (Context (u t m))
     , forall x m t
         . ( Show x, Monad m, MonadTrans t, Typeable m, Typeable t, Typeable x
           , forall y. (Show y, Typeable y) => Show (t m y) )
        => Show (u t m x)
     , forall m t. (Monad m, Arbitrary (Context (t m))) => Arbitrary (Context (u t m))
     , forall x m t
         . ( Arbitrary x, Monad m, MonadTrans t
           , forall x. (Arbitrary x) => Arbitrary (t m x) )
        => Arbitrary (u t m x)
     , forall m t
         . ( Monad m, MonadTrans t
           , EqIn (t m) )
        => EqIn (u t m) )
  => Proxy (u :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *)
  -> TestTree
test_MonadTransTrans_MonadTrans_FAM proxyU =
  testGroup "MonadTransTrans MonadTrans (FAM)"
    [ test_MonadTransTrans_Monad_FAM proxyU (Proxy :: Proxy IdentityT)

    , test_MonadTransTrans_Monad_FAM proxyU (Proxy :: Proxy (HaltT Identity))

    , test_MonadTransTrans_Monad_FAM proxyU (Proxy :: Proxy (ExceptT Identity Bool))
    , test_MonadTransTrans_Monad_FAM proxyU (Proxy :: Proxy (ExceptT Identity Int))

    , test_MonadTransTrans_Monad_FAM proxyU (Proxy :: Proxy (WriteOnlyT Identity Bool))
    , test_MonadTransTrans_Monad_FAM proxyU (Proxy :: Proxy (WriteOnlyT Identity Int))

    , test_MonadTransTrans_Monad_FAM proxyU (Proxy :: Proxy (ReadOnlyT Identity Bool))
    , test_MonadTransTrans_Monad_FAM proxyU (Proxy :: Proxy (ReadOnlyT Identity Int))

    , test_MonadTransTrans_Monad_FAM proxyU (Proxy :: Proxy (StateT Identity Bool))
    , test_MonadTransTrans_Monad_FAM proxyU (Proxy :: Proxy (StateT Identity Int))

    , test_MonadTransTrans_Monad_FAM proxyU (Proxy :: Proxy (AppendOnlyT Identity Bool))
    , test_MonadTransTrans_Monad_FAM proxyU (Proxy :: Proxy (AppendOnlyT Identity Int))

    , test_MonadTransTrans_Monad_FAM proxyU (Proxy :: Proxy (WriteOnceT Identity Bool))
    , test_MonadTransTrans_Monad_FAM proxyU (Proxy :: Proxy (WriteOnceT Identity Int))
    ]



test_MonadTransTrans_Monad_FAM
  :: ( MonadTrans t, Typeable t, MonadTransTrans u, Typeable u
     , forall x m
         . ( Show x, Monad m, Typeable m, Typeable x
           , forall y. (Show y, Typeable y) => Show (m y) )
        => Show (u t m x)
     , forall m. (Typeable m, Show (Context m)) => Show (Context (u t m))
     , forall x m
         . ( Arbitrary x, Monad m
           , forall x. (Arbitrary x) => Arbitrary (m x) )
        => Arbitrary (u t m x)
     , forall m. (Monad m, Arbitrary (Context m)) => (Arbitrary (Context (u t m)))
     , forall m
         . ( Monad m
           , EqIn m
           , EqIn (t m) )
        => EqIn (u t m)
     , forall m. ( Monad m, EqIn m ) => EqIn (t m) )
  => Proxy (u :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *)
  -> Proxy (t :: (* -> *) -> * -> *)
  -> TestTree
test_MonadTransTrans_Monad_FAM proxyU proxyT =
  testGroup "MonadTransTrans Monad (FAM)"
    [ test_MonadTransTrans_FAM proxyU proxyT (Proxy :: Proxy Identity)

    , test_MonadTransTrans_FAM proxyU proxyT (Proxy :: Proxy Maybe)

    , test_MonadTransTrans_FAM proxyU proxyT (Proxy :: Proxy (Except Identity Bool))
    , test_MonadTransTrans_FAM proxyU proxyT (Proxy :: Proxy (Except Identity Int))

    , test_MonadTransTrans_FAM proxyU proxyT (Proxy :: Proxy (WriteOnly Identity Bool))
    , test_MonadTransTrans_FAM proxyU proxyT (Proxy :: Proxy (WriteOnly Identity Int))

    , test_MonadTransTrans_FAM proxyU proxyT (Proxy :: Proxy (State Identity Bool))
    , test_MonadTransTrans_FAM proxyU proxyT (Proxy :: Proxy (State Identity Int))

    , test_MonadTransTrans_FAM proxyU proxyT (Proxy :: Proxy (ReadOnly Identity Bool))
    , test_MonadTransTrans_FAM proxyU proxyT (Proxy :: Proxy (ReadOnly Identity Int))

    , test_MonadTransTrans_FAM proxyU proxyT (Proxy :: Proxy (AppendOnly Identity Bool))
    , test_MonadTransTrans_FAM proxyU proxyT (Proxy :: Proxy (AppendOnly Identity Int))

    , test_MonadTransTrans_FAM proxyU proxyT (Proxy :: Proxy (WriteOnce Identity Bool))
    , test_MonadTransTrans_FAM proxyU proxyT (Proxy :: Proxy (WriteOnce Identity Int))
    ]



test_MonadTransTrans_FAM
  :: ( Monad m, Typeable m, MonadTrans t, Typeable t, MonadTransTrans u, Typeable u
     , Show (Context (u t m)), forall x. ( Show x, Typeable x ) => Show (u t m x)
     , Arbitrary (Context (u t m)), forall x. ( Arbitrary x ) => Arbitrary (u t m x)
     , EqIn (u t m) )
  => Proxy (u :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *)
  -> Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (m :: * -> *)
  -> TestTree
test_MonadTransTrans_FAM proxyU proxyT proxyM =
  testGroup "MonadTransTrans Monad (FAM)"
    [ testFunctorLaws2     (pTT proxyU proxyT proxyM) (pContextTT proxyU proxyT proxyM) pB pI eqIn
    , testApplicativeLaws2 (pTT proxyU proxyT proxyM) (pContextTT proxyU proxyT proxyM) pB pI eqIn
    , testMonadLaws2       (pTT proxyU proxyT proxyM) (pContextTT proxyU proxyT proxyM) pB pI eqIn
    ]





test_all_MonadTransTrans_FX :: TestTree
test_all_MonadTransTrans_FX = testGroup "MonadTransTrans (FX)"
  [ testGroup "MonadTransTrans (State)"
    [ test_MonadTransTrans_State (Proxy :: Proxy (StateTT Identity Int)) pId pI
    , test_MonadTransTrans_MonadTrans_Compose_FX
    ]
  ]

-- Test the MonadState laws for a concrete monad transformer over several concrete monads.
test_MonadTransTrans_State
  :: forall u s mark
   . ( MonadTransTrans u, MonadIdentity mark
     , Typeable u, Typeable mark, Typeable s
     , Eq (mark s), Show (mark s), Arbitrary (mark s), CoArbitrary (mark s)
     , forall m t. (Show (Context m), Show (Context (t m)), Typeable m) => Show (Context (u t m))
     , forall m t. (Arbitrary (Context m), Arbitrary (Context (t m))) => Arbitrary (Context (u t m))
     , forall x m t. ( Show x, Typeable x, Typeable m, Typeable t ) => Show (u t m x)
     , forall x m t
          . ( MonadTrans t, Monad m, Arbitrary x
          , forall z. (Arbitrary z) => Arbitrary (m z)
          , forall y. (Arbitrary y, Arbitrary (m y)) => Arbitrary (t m y) )
         => Arbitrary (u t m x)
     , forall m t. ( Monad m, MonadTrans t, EqIn m, EqIn (t m) ) => EqIn (u t m)
     , forall m t. ( Monad m, MonadTrans t ) => MonadState mark s (u t m) )
  => Proxy (u :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *)
  -> Proxy (mark :: * -> *)
  -> Proxy (s :: *)
  -> TestTree
test_MonadTransTrans_State proxyU proxyMark proxyS =
  testGroup "MonadTrans (State)"
    [ test_MonadTransTrans_MonadTrans_State proxyU (Proxy :: Proxy IdentityT) proxyMark proxyS

    , test_MonadTransTrans_MonadTrans_State proxyU (Proxy :: Proxy (HaltT Q)) proxyMark proxyS

    , test_MonadTransTrans_MonadTrans_State proxyU (Proxy :: Proxy (ReadOnlyT Q Int))   proxyMark proxyS
    , test_MonadTransTrans_MonadTrans_State proxyU (Proxy :: Proxy (WriteOnlyT Q Int))  proxyMark proxyS
    , test_MonadTransTrans_MonadTrans_State proxyU (Proxy :: Proxy (AppendOnlyT Q Int)) proxyMark proxyS
    , test_MonadTransTrans_MonadTrans_State proxyU (Proxy :: Proxy (WriteOnceT Q Int))  proxyMark proxyS
    , test_MonadTransTrans_MonadTrans_State proxyU (Proxy :: Proxy (ExceptT Q Int))     proxyMark proxyS
    ]



-- Test the MonadState laws for a concrete monad transformer and concrete monad over several value types.
test_MonadTransTrans_MonadTrans_State
  :: forall u t s mark
   . ( MonadTransTrans u, MonadTrans t, MonadIdentity mark
     , Typeable u, Typeable t, Typeable mark, Typeable s
     , forall x. (Monad x, Show (Context x), Typeable x) => Show (Context (u t x))
     , forall x. (Monad x, Arbitrary (Context x)) => Arbitrary (Context (u t x))
     , Eq (mark s), Show (mark s), Arbitrary (mark s), CoArbitrary (mark s)
       , forall m x
           . ( Monad m, Typeable m, Show x, Typeable x )
          => Show (u t m x)
       , forall m x
           . ( Monad m, Arbitrary x
           , forall z. (Arbitrary z) => Arbitrary (m z) )
          => Arbitrary (u t m x)
     , forall x. (Monad x, EqIn x) => EqIn (u t x)
     , forall x. (Monad x) => MonadState mark s (u t x) )
  => Proxy (u :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *)
  -> Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (mark :: * -> *)
  -> Proxy (s :: *)
  -> TestTree
test_MonadTransTrans_MonadTrans_State proxyU proxyT proxyMark proxyS =
  testGroup "MonadTransTrans MonadTrans (State)"
    [ test_MonadTransTrans_MonadTrans_Monad_State proxyU proxyT (Proxy :: Proxy Identity) proxyMark proxyS

    , test_MonadTransTrans_MonadTrans_Monad_State proxyU proxyT (Proxy :: Proxy (Halt Q)) proxyMark proxyS

    , test_MonadTransTrans_MonadTrans_Monad_State proxyU proxyT (Proxy :: Proxy (ReadOnly Q Int))   proxyMark proxyS
    , test_MonadTransTrans_MonadTrans_Monad_State proxyU proxyT (Proxy :: Proxy (WriteOnly Q Int))  proxyMark proxyS
    , test_MonadTransTrans_MonadTrans_Monad_State proxyU proxyT (Proxy :: Proxy (AppendOnly Q Int)) proxyMark proxyS
    , test_MonadTransTrans_MonadTrans_Monad_State proxyU proxyT (Proxy :: Proxy (WriteOnce Q Int))  proxyMark proxyS
    , test_MonadTransTrans_MonadTrans_Monad_State proxyU proxyT (Proxy :: Proxy (Except Q Int))     proxyMark proxyS
    ]



-- Test the MonadState laws for a concrete monad transformer and concrete monad over several value types.
test_MonadTransTrans_MonadTrans_Monad_State
  :: forall u t m s mark
   . ( Functor m, Applicative m, Monad m, MonadTrans t, MonadIdentity mark
     , Typeable m, Typeable t, Typeable u, Typeable mark, Typeable s
     , Show (Context (u t m)), Arbitrary (Context (u t m))
     , Eq (mark s), Show (mark s), Arbitrary (mark s), CoArbitrary (mark s)
       , forall x
           . ( Show x, Typeable x )
          => Show (u t m x)
       , forall x
           . ( Arbitrary x )
          => Arbitrary (u t m x)
     , EqIn (u t m)
     , MonadState mark s (u t m) )
  => Proxy (u :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *)
  -> Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (m :: * -> *)
  -> Proxy (mark :: * -> *)
  -> Proxy (s :: *)
  -> TestTree
test_MonadTransTrans_MonadTrans_Monad_State proxyU proxyT proxyM proxyMark proxyS =
  testGroup "MonadTransTrans MonadTrans Monad (State)"
    [ testStateMonadLaws (Proxy :: Proxy (u t m)) Proxy (pAp proxyMark proxyS) pI eqIn get put
    , testStateMonadLaws (Proxy :: Proxy (u t m)) Proxy (pAp proxyMark proxyS) pB eqIn get put
    ]



test_MonadTransTrans_MonadTrans_Compose_FX :: TestTree
test_MonadTransTrans_MonadTrans_Compose_FX = testGroup "MonadTransTrans Compose (FX)"
  [ testGroup "MonadTransTrans Compose (State)"
    [ test_MonadTrans_State (Proxy :: Proxy (StateTT Identity Int (IdentityTT IdentityT))) pId pI

    , test_MonadTrans_State (Proxy :: Proxy (StateTT Identity Int (HaltTT Q IdentityT))) pId pI

    , test_MonadTrans_State (Proxy :: Proxy (StateTT Identity Int (StateTT Q Int              IdentityT))) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (StateTT Identity Int (ReadOnlyTT Q Int           IdentityT))) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (StateTT Identity Int (WriteOnlyTT Q Int          IdentityT))) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (StateTT Identity Int (AppendOnlyTT Q Int         IdentityT))) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (StateTT Identity Int (WriteOnceTT Q Int          IdentityT))) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (StateTT Identity Int (ExceptTT Q Int             IdentityT))) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (StateTT Identity Int (PromptTT Identity Identity IdentityT))) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (StateTT Identity Int (TeletypeTT Identity        IdentityT))) pId pI

    , test_MonadTrans_State (Proxy :: Proxy (StateTT Q Int              (StateTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (ReadOnlyTT Q Int           (StateTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (WriteOnlyTT Q Int          (StateTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (AppendOnlyTT Q Int         (StateTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (WriteOnceTT Q Int          (StateTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (ExceptTT Q Int             (StateTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (PromptTT Identity Identity (StateTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (TeletypeTT Identity        (StateTT Identity Int IdentityT))) pId pI

    , test_MonadTrans_State (Proxy :: Proxy (StateTT Identity Int (StateTT Q Int              (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (StateTT Identity Int (ReadOnlyTT Q Int           (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (StateTT Identity Int (WriteOnlyTT Q Int          (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (StateTT Identity Int (AppendOnlyTT Q Int         (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (StateTT Identity Int (WriteOnceTT Q Int          (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (StateTT Identity Int (ExceptTT Q Int             (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (StateTT Identity Int (PromptTT Identity Identity (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (StateTT Identity Int (TeletypeTT Identity        (ReadOnlyT Q Bool)))) pId pI

    , test_MonadTrans_State (Proxy :: Proxy (StateTT Q Int              (StateTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (ReadOnlyTT Q Int           (StateTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (WriteOnlyTT Q Int          (StateTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (AppendOnlyTT Q Int         (StateTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (WriteOnceTT Q Int          (StateTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (ExceptTT Q Int             (StateTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (PromptTT Identity Identity (StateTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (TeletypeTT Identity        (StateTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    ]

  , testGroup "MonadTransTrans Compose (ReadOnly)"
    [ test_MonadTrans_ReadOnly (Proxy :: Proxy (ReadOnlyTT Identity Int (IdentityTT IdentityT))) pId pI

    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ReadOnlyTT Identity Int (HaltTT Q IdentityT))) pId pI

    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ReadOnlyTT Identity Int (StateTT Q Int              IdentityT))) pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ReadOnlyTT Identity Int (ReadOnlyTT Q Int           IdentityT))) pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ReadOnlyTT Identity Int (WriteOnlyTT Q Int          IdentityT))) pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ReadOnlyTT Identity Int (AppendOnlyTT Q Int         IdentityT))) pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ReadOnlyTT Identity Int (WriteOnceTT Q Int          IdentityT))) pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ReadOnlyTT Identity Int (ExceptTT Q Int             IdentityT))) pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ReadOnlyTT Identity Int (PromptTT Identity Identity IdentityT))) pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ReadOnlyTT Identity Int (TeletypeTT Identity        IdentityT))) pId pI

    , test_MonadTrans_ReadOnly (Proxy :: Proxy (StateTT Q Int              (ReadOnlyTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ReadOnlyTT Q Int           (ReadOnlyTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (WriteOnlyTT Q Int          (ReadOnlyTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (AppendOnlyTT Q Int         (ReadOnlyTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (WriteOnceTT Q Int          (ReadOnlyTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ExceptTT Q Int             (ReadOnlyTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (PromptTT Identity Identity (ReadOnlyTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (TeletypeTT Identity        (ReadOnlyTT Identity Int IdentityT))) pId pI

    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ReadOnlyTT Identity Int (StateTT Q Int              (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ReadOnlyTT Identity Int (ReadOnlyTT Q Int           (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ReadOnlyTT Identity Int (WriteOnlyTT Q Int          (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ReadOnlyTT Identity Int (AppendOnlyTT Q Int         (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ReadOnlyTT Identity Int (WriteOnceTT Q Int          (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ReadOnlyTT Identity Int (ExceptTT Q Int             (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ReadOnlyTT Identity Int (PromptTT Identity Identity (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ReadOnlyTT Identity Int (TeletypeTT Identity        (ReadOnlyT Q Bool)))) pId pI

    , test_MonadTrans_ReadOnly (Proxy :: Proxy (StateTT Q Int              (ReadOnlyTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ReadOnlyTT Q Int           (ReadOnlyTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (WriteOnlyTT Q Int          (ReadOnlyTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (AppendOnlyTT Q Int         (ReadOnlyTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (WriteOnceTT Q Int          (ReadOnlyTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (ExceptTT Q Int             (ReadOnlyTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (PromptTT Identity Identity (ReadOnlyTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_ReadOnly (Proxy :: Proxy (TeletypeTT Identity        (ReadOnlyTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    ]

  , testGroup "MonadTransTrans Compose (AppendOnly)"
    [ test_MonadTrans_AppendOnly (Proxy :: Proxy (AppendOnlyTT Identity Int (IdentityTT IdentityT))) pId pI

    , test_MonadTrans_AppendOnly (Proxy :: Proxy (AppendOnlyTT Identity Int (HaltTT Q IdentityT))) pId pI

    , test_MonadTrans_AppendOnly (Proxy :: Proxy (AppendOnlyTT Identity Int (StateTT Q Int              IdentityT))) pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (AppendOnlyTT Identity Int (ReadOnlyTT Q Int           IdentityT))) pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (AppendOnlyTT Identity Int (WriteOnlyTT Q Int          IdentityT))) pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (AppendOnlyTT Identity Int (AppendOnlyTT Q Int         IdentityT))) pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (AppendOnlyTT Identity Int (WriteOnceTT Q Int          IdentityT))) pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (AppendOnlyTT Identity Int (ExceptTT Q Int             IdentityT))) pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (AppendOnlyTT Identity Int (PromptTT Identity Identity IdentityT))) pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (AppendOnlyTT Identity Int (TeletypeTT Identity        IdentityT))) pId pI

    , test_MonadTrans_AppendOnly (Proxy :: Proxy (StateTT Q Int              (AppendOnlyTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ReadOnlyTT Q Int           (AppendOnlyTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (WriteOnlyTT Q Int          (AppendOnlyTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (AppendOnlyTT Q Int         (AppendOnlyTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (WriteOnceTT Q Int          (AppendOnlyTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ExceptTT Q Int             (AppendOnlyTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (PromptTT Identity Identity (AppendOnlyTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (TeletypeTT Identity        (AppendOnlyTT Identity Int IdentityT))) pId pI

    , test_MonadTrans_AppendOnly (Proxy :: Proxy (AppendOnlyTT Identity Int (StateTT Q Int              (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (AppendOnlyTT Identity Int (ReadOnlyTT Q Int           (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (AppendOnlyTT Identity Int (WriteOnlyTT Q Int          (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (AppendOnlyTT Identity Int (AppendOnlyTT Q Int         (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (AppendOnlyTT Identity Int (WriteOnceTT Q Int          (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (AppendOnlyTT Identity Int (ExceptTT Q Int             (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (AppendOnlyTT Identity Int (PromptTT Identity Identity (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (AppendOnlyTT Identity Int (TeletypeTT Identity        (ReadOnlyT Q Bool)))) pId pI

    , test_MonadTrans_AppendOnly (Proxy :: Proxy (StateTT Q Int              (AppendOnlyTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ReadOnlyTT Q Int           (AppendOnlyTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (WriteOnlyTT Q Int          (AppendOnlyTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (AppendOnlyTT Q Int         (AppendOnlyTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (WriteOnceTT Q Int          (AppendOnlyTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (ExceptTT Q Int             (AppendOnlyTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (PromptTT Identity Identity (AppendOnlyTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_AppendOnly (Proxy :: Proxy (TeletypeTT Identity        (AppendOnlyTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    ]

  , testGroup "MonadTransTrans Compose (WriteOnce)"
    [ test_MonadTrans_WriteOnce (Proxy :: Proxy (WriteOnceTT Identity Int (IdentityTT IdentityT))) pId pI

    , test_MonadTrans_WriteOnce (Proxy :: Proxy (WriteOnceTT Identity Int (HaltTT Q IdentityT))) pId pI

    , test_MonadTrans_WriteOnce (Proxy :: Proxy (WriteOnceTT Identity Int (StateTT Q Int              IdentityT))) pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (WriteOnceTT Identity Int (ReadOnlyTT Q Int           IdentityT))) pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (WriteOnceTT Identity Int (WriteOnlyTT Q Int          IdentityT))) pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (WriteOnceTT Identity Int (AppendOnlyTT Q Int         IdentityT))) pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (WriteOnceTT Identity Int (WriteOnceTT Q Int          IdentityT))) pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (WriteOnceTT Identity Int (ExceptTT Q Int             IdentityT))) pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (WriteOnceTT Identity Int (PromptTT Identity Identity IdentityT))) pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (WriteOnceTT Identity Int (TeletypeTT Identity        IdentityT))) pId pI

    , test_MonadTrans_WriteOnce (Proxy :: Proxy (StateTT Q Int              (WriteOnceTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ReadOnlyTT Q Int           (WriteOnceTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (WriteOnlyTT Q Int          (WriteOnceTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (AppendOnlyTT Q Int         (WriteOnceTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (WriteOnceTT Q Int          (WriteOnceTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ExceptTT Q Int             (WriteOnceTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (PromptTT Identity Identity (WriteOnceTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (TeletypeTT Identity        (WriteOnceTT Identity Int IdentityT))) pId pI

    , test_MonadTrans_WriteOnce (Proxy :: Proxy (WriteOnceTT Identity Int (StateTT Q Int              (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (WriteOnceTT Identity Int (ReadOnlyTT Q Int           (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (WriteOnceTT Identity Int (WriteOnlyTT Q Int          (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (WriteOnceTT Identity Int (AppendOnlyTT Q Int         (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (WriteOnceTT Identity Int (WriteOnceTT Q Int          (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (WriteOnceTT Identity Int (ExceptTT Q Int             (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (WriteOnceTT Identity Int (PromptTT Identity Identity (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (WriteOnceTT Identity Int (TeletypeTT Identity        (ReadOnlyT Q Bool)))) pId pI

    , test_MonadTrans_WriteOnce (Proxy :: Proxy (StateTT Q Int              (WriteOnceTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ReadOnlyTT Q Int           (WriteOnceTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (WriteOnlyTT Q Int          (WriteOnceTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (AppendOnlyTT Q Int         (WriteOnceTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (WriteOnceTT Q Int          (WriteOnceTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (ExceptTT Q Int             (WriteOnceTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (PromptTT Identity Identity (WriteOnceTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_WriteOnce (Proxy :: Proxy (TeletypeTT Identity        (WriteOnceTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    ]

  , testGroup "MonadTransTrans Compose (WriteOnly)"
    [ test_MonadTrans_WriteOnly (Proxy :: Proxy (WriteOnlyTT Identity Int (IdentityTT IdentityT))) pId pI

    , test_MonadTrans_WriteOnly (Proxy :: Proxy (WriteOnlyTT Identity Int (HaltTT Q IdentityT))) pId pI

    , test_MonadTrans_WriteOnly (Proxy :: Proxy (WriteOnlyTT Identity Int (StateTT Q Int              IdentityT))) pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (WriteOnlyTT Identity Int (ReadOnlyTT Q Int           IdentityT))) pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (WriteOnlyTT Identity Int (WriteOnlyTT Q Int          IdentityT))) pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (WriteOnlyTT Identity Int (AppendOnlyTT Q Int         IdentityT))) pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (WriteOnlyTT Identity Int (WriteOnceTT Q Int          IdentityT))) pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (WriteOnlyTT Identity Int (ExceptTT Q Int             IdentityT))) pId pI

    , test_MonadTrans_WriteOnly (Proxy :: Proxy (StateTT Q Int              (WriteOnlyTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ReadOnlyTT Q Int           (WriteOnlyTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (WriteOnlyTT Q Int          (WriteOnlyTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (AppendOnlyTT Q Int         (WriteOnlyTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (WriteOnceTT Q Int          (WriteOnlyTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ExceptTT Q Int             (WriteOnlyTT Identity Int IdentityT))) pId pI

    , test_MonadTrans_WriteOnly (Proxy :: Proxy (WriteOnlyTT Identity Int (StateTT Q Int              (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (WriteOnlyTT Identity Int (ReadOnlyTT Q Int           (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (WriteOnlyTT Identity Int (WriteOnlyTT Q Int          (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (WriteOnlyTT Identity Int (AppendOnlyTT Q Int         (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (WriteOnlyTT Identity Int (WriteOnceTT Q Int          (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (WriteOnlyTT Identity Int (ExceptTT Q Int             (ReadOnlyT Q Bool)))) pId pI

    , test_MonadTrans_WriteOnly (Proxy :: Proxy (StateTT Q Int              (WriteOnlyTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ReadOnlyTT Q Int           (WriteOnlyTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (WriteOnlyTT Q Int          (WriteOnlyTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (AppendOnlyTT Q Int         (WriteOnlyTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (WriteOnceTT Q Int          (WriteOnlyTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_WriteOnly (Proxy :: Proxy (ExceptTT Q Int             (WriteOnlyTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    ]

  , testGroup "MonadTransTrans Compose (Except)"
    [ test_MonadTrans_Except (Proxy :: Proxy (ExceptTT Identity Int (IdentityTT IdentityT))) pId pI

    , test_MonadTrans_Except (Proxy :: Proxy (ExceptTT Identity Int (HaltTT Q IdentityT))) pId pI

    , test_MonadTrans_Except (Proxy :: Proxy (ExceptTT Identity Int (StateTT Q Int      IdentityT))) pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ExceptTT Identity Int (ReadOnlyTT Q Int   IdentityT))) pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ExceptTT Identity Int (WriteOnlyTT Q Int  IdentityT))) pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ExceptTT Identity Int (AppendOnlyTT Q Int IdentityT))) pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ExceptTT Identity Int (WriteOnceTT Q Int  IdentityT))) pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ExceptTT Identity Int (ExceptTT Q Int     IdentityT))) pId pI

    , test_MonadTrans_Except (Proxy :: Proxy (StateTT Q Int      (ExceptTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ReadOnlyTT Q Int   (ExceptTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (WriteOnlyTT Q Int  (ExceptTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (AppendOnlyTT Q Int (ExceptTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (WriteOnceTT Q Int  (ExceptTT Identity Int IdentityT))) pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ExceptTT Q Int     (ExceptTT Identity Int IdentityT))) pId pI

    , test_MonadTrans_Except (Proxy :: Proxy (ExceptTT Identity Int (StateTT Q Int      (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ExceptTT Identity Int (ReadOnlyTT Q Int   (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ExceptTT Identity Int (WriteOnlyTT Q Int  (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ExceptTT Identity Int (AppendOnlyTT Q Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ExceptTT Identity Int (WriteOnceTT Q Int  (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ExceptTT Identity Int (ExceptTT Q Int     (ReadOnlyT Q Bool)))) pId pI

    , test_MonadTrans_Except (Proxy :: Proxy (StateTT Q Int      (ExceptTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ReadOnlyTT Q Int   (ExceptTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (WriteOnlyTT Q Int  (ExceptTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (AppendOnlyTT Q Int (ExceptTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (WriteOnceTT Q Int  (ExceptTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ExceptTT Q Int     (ExceptTT Identity Int (ReadOnlyT Q Bool)))) pId pI
    ]
  ]



pContextTT
  :: Proxy (u :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *)
  -> Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (m :: * -> *)
  -> Proxy (Context (u t m))
pContextTT _ _ _ = Proxy

pTT
  :: Proxy (u :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *)
  -> Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (m :: * -> *)
  -> Proxy (u t m)
pTT _ _ _ = Proxy
