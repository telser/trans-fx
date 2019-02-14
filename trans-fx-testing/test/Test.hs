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
import Control.FX.Show

import Test.Tasty.QuickCheck.Laws.FX.MonadTrans



main :: IO ()
main = do
  setEnv "TASTY_NUM_THREADS" "6"
  setEnv "TASTY_QUICKCHECK_TESTS" "100"
  setEnv "TASTY_HIDE_SUCCESSES" "TRUE"
  defaultMain $ testGroup "Laws"
    [ testGroup "Monad"
      [ test_all_Monad_FAM
      , test_all_Monad_FX
      ]

    , testGroup "MonadTrans"
      [ test_all_MonadTrans_FAM
      , test_all_MonadTrans_T
      , test_all_MonadTrans_FX
      ]

    , testGroup "MonadTransTrans"
      [ test_all_MonadTransTrans_FAM
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

instance (Semigroup w) => Semigroup (Identity w) where
  (Identity x) <> (Identity y) = Identity (x <> y)
instance (Monoid w) => Monoid (Identity w) where
  mempty = Identity mempty
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



{---------}
{- Monad -}
{---------}

-- Test Functor, Applicative, and Monad laws for several concrete monads.
test_all_Monad_FAM :: TestTree
test_all_Monad_FAM = testGroup "All Monad (FAM)"
  [ test_Monad_FAM (Proxy :: Proxy Identity) pU

  , test_Monad_FAM (Proxy :: Proxy (Except Identity Bool)) pU
  , test_Monad_FAM (Proxy :: Proxy (Except Identity Int))  pU

  , test_Monad_FAM (Proxy :: Proxy (Writer Identity Bool)) pU
  , test_Monad_FAM (Proxy :: Proxy (Writer Identity Int))  pU

  , test_Monad_FAM (Proxy :: Proxy (State Identity Bool)) pB
  , test_Monad_FAM (Proxy :: Proxy (State Identity Int))  pI

  , test_Monad_FAM (Proxy :: Proxy (Reader Identity Bool)) pB
  , test_Monad_FAM (Proxy :: Proxy (Reader Identity Int))  pI

  , test_Monad_FAM (Proxy :: Proxy (Tagged Bool)) pU
  , test_Monad_FAM (Proxy :: Proxy (Tagged Int))  pU
  ]



-- Test effect class laws for several concrete monads.
test_all_Monad_FX :: TestTree
test_all_Monad_FX = testGroup "All Monad (FX)"
  [ testStateMonadLaws (Proxy :: Proxy (State Identity Bool)) pB (pAp pId pB) pI eqIn get put
  , testStateMonadLaws (Proxy :: Proxy (State Identity Int))  pI (pAp pId pI) pI eqIn get put

  , testReaderMonadLaws (Proxy :: Proxy (Reader Identity Bool)) pB (pAp pId pB) pB pI eqIn ask local
  , testReaderMonadLaws (Proxy :: Proxy (Reader Identity Int))  pI (pAp pId pI) pB pI eqIn ask local

  , testWriterMonadLaws (Proxy :: Proxy (Writer Identity Bool)) pU (pAp pId pB) pB pI eqIn tell (fixDraft draft)
  , testWriterMonadLaws (Proxy :: Proxy (Writer Identity Int))  pU (pAp pId pI) pB pI eqIn tell (fixDraft draft)

  , testErrorMonadLaws (Proxy :: Proxy (Except Identity Bool)) pU (pAp pId pB) pB pI eqIn throw catch
  , testErrorMonadLaws (Proxy :: Proxy (Except Identity Int))  pU (pAp pId pI) pB pI eqIn throw catch
  ]



-- Test Functor, Applicative, and Monad instances for a concrete monad.
test_Monad_FAM
  :: ( Functor m, Applicative m, Monad m, Typeable m
     , Show w
       , forall u
           . ( Show u
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





{--------------}
{- MonadTrans -}
{--------------}

-- Test Functor, Applicative, and Monad laws for several concrete monad transformers over several concrete monads.
test_all_MonadTrans_FAM :: TestTree
test_all_MonadTrans_FAM = testGroup "All MonadTrans (FAM)"
  [ test_MonadTrans_FAM (Proxy :: Proxy IdentityT) pU

  , test_MonadTrans_FAM (Proxy :: Proxy MaybeT) pU

  , test_MonadTrans_FAM (Proxy :: Proxy (ExceptT Identity Bool)) pU
  , test_MonadTrans_FAM (Proxy :: Proxy (ExceptT Identity Int))  pU

  , test_MonadTrans_FAM (Proxy :: Proxy (WriterT Identity Bool)) pU
  , test_MonadTrans_FAM (Proxy :: Proxy (WriterT Identity Int))  pU

  , test_MonadTrans_FAM (Proxy :: Proxy (StateT Identity Bool)) pB
  , test_MonadTrans_FAM (Proxy :: Proxy (StateT Identity Int))  pI

  , test_MonadTrans_FAM (Proxy :: Proxy (ReaderT Identity Bool)) pB
  , test_MonadTrans_FAM (Proxy :: Proxy (ReaderT Identity Int))  pI

  , testGroup "ComposeT"
    [ test_MonadTrans_ComposeT_FAM (Proxy :: Proxy IdentityT) pU

    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy MaybeT) pU

    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (ExceptT Identity Bool)) pU
    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (ExceptT Identity Int))  pU

    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (WriterT Identity Bool)) pU
    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (WriterT Identity Int))  pU

    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (ReaderT Identity Bool)) pB
    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (ReaderT Identity Int))  pI

    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (StateT Identity Bool)) pB
    , test_MonadTrans_ComposeT_FAM (Proxy :: Proxy (StateT Identity Int))  pI
    ]
  ]



-- Test MonadTrans laws for several concrete monad transformers over several concrete monads.
test_all_MonadTrans_T :: TestTree
test_all_MonadTrans_T = testGroup "All MonadTrans (T)"
  [ test_MonadTrans_T (Proxy :: Proxy IdentityT) pU

  , test_MonadTrans_T (Proxy :: Proxy MaybeT) pU

  , test_MonadTrans_T (Proxy :: Proxy (ExceptT Identity Bool)) pU
  , test_MonadTrans_T (Proxy :: Proxy (ExceptT Identity Int))  pU

  , test_MonadTrans_T (Proxy :: Proxy (WriterT Identity Bool)) pU
  , test_MonadTrans_T (Proxy :: Proxy (WriterT Identity Int))  pU

  , test_MonadTrans_T (Proxy :: Proxy (StateT Identity Bool)) pB
  , test_MonadTrans_T (Proxy :: Proxy (StateT Identity Int))  pI

  , test_MonadTrans_T (Proxy :: Proxy (ReaderT Identity Bool)) pB
  , test_MonadTrans_T (Proxy :: Proxy (ReaderT Identity Int))  pI
  ]



-- Test effect class laws for several concrete monad transformers over several concrete monads.
test_all_MonadTrans_FX :: TestTree
test_all_MonadTrans_FX = testGroup "All MonadTrans (FX)"
  [ testGroup "State"
    [ test_MonadTrans_State (Proxy :: Proxy (StateT Identity Bool)) pB pId pB

    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (StateT Identity Bool) (IdentityT))) (pT2 pB pU) pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (StateT Identity Int)  (IdentityT))) (pT2 pI pU) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (StateT Identity Bool) (MaybeT)))    (pT2 pB pU) pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (StateT Identity Int)  (MaybeT)))    (pT2 pI pU) pId pI

    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (IdentityT) (StateT Identity Bool))) (pT2 pU pB) pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (IdentityT) (StateT Identity Int)))  (pT2 pU pI) pId pI
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (MaybeT)    (StateT Identity Bool))) (pT2 pU pB) pId pB
    , test_MonadTrans_State (Proxy :: Proxy (ComposeT (MaybeT)    (StateT Identity Int)))  (pT2 pU pI) pId pI
    ]

  , testGroup "Except"
    [ test_MonadTrans_Except (Proxy :: Proxy (ExceptT Identity Bool)) pU pId pB

    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ExceptT Identity Bool) (IdentityT))) (pT2 pU pU) pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ExceptT Identity Int)  (IdentityT))) (pT2 pU pU) pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ExceptT Identity Bool) (MaybeT)))    (pT2 pU pU) pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (ExceptT Identity Int)  (MaybeT)))    (pT2 pU pU) pId pI

    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (IdentityT) (ExceptT Identity Bool))) (pT2 pU pU) pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (IdentityT) (ExceptT Identity Int)))  (pT2 pU pU) pId pI
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (MaybeT)    (ExceptT Identity Bool))) (pT2 pU pU) pId pB
    , test_MonadTrans_Except (Proxy :: Proxy (ComposeT (MaybeT)    (ExceptT Identity Int)))  (pT2 pU pU) pId pI
    ]

  , testGroup "Writer"
    [ test_MonadTrans_Writer (Proxy :: Proxy (WriterT Identity Bool)) pU pId pB

    , test_MonadTrans_Writer (Proxy :: Proxy (ComposeT (WriterT Identity Bool) (IdentityT))) (pT2 pU pU) pId pB
    , test_MonadTrans_Writer (Proxy :: Proxy (ComposeT (WriterT Identity Int)  (IdentityT))) (pT2 pU pU) pId pI
    , test_MonadTrans_Writer (Proxy :: Proxy (ComposeT (WriterT Identity Bool) (MaybeT)))    (pT2 pU pU) pId pB
    , test_MonadTrans_Writer (Proxy :: Proxy (ComposeT (WriterT Identity Int)  (MaybeT)))    (pT2 pU pU) pId pI

    , test_MonadTrans_Writer (Proxy :: Proxy (ComposeT (IdentityT) (WriterT Identity Bool))) (pT2 pU pU) pId pB
    , test_MonadTrans_Writer (Proxy :: Proxy (ComposeT (IdentityT) (WriterT Identity Int)))  (pT2 pU pU) pId pI
    , test_MonadTrans_Writer (Proxy :: Proxy (ComposeT (MaybeT)    (WriterT Identity Bool))) (pT2 pU pU) pId pB
    , test_MonadTrans_Writer (Proxy :: Proxy (ComposeT (MaybeT)    (WriterT Identity Int)))  (pT2 pU pU) pId pI
    ]

  , testGroup "Reader"
    [ test_MonadTrans_Reader (Proxy :: Proxy (ReaderT Identity Bool)) pB pId pB

    , test_MonadTrans_Reader (Proxy :: Proxy (ComposeT (ReaderT Identity Bool) (IdentityT))) (pT2 pB pU) pId pB
    , test_MonadTrans_Reader (Proxy :: Proxy (ComposeT (ReaderT Identity Int)  (IdentityT))) (pT2 pI pU) pId pI
    , test_MonadTrans_Reader (Proxy :: Proxy (ComposeT (ReaderT Identity Bool) (MaybeT)))    (pT2 pB pU) pId pB
    , test_MonadTrans_Reader (Proxy :: Proxy (ComposeT (ReaderT Identity Int)  (MaybeT)))    (pT2 pI pU) pId pI

    , test_MonadTrans_Reader (Proxy :: Proxy (ComposeT (IdentityT) (ReaderT Identity Bool))) (pT2 pU pB) pId pB
    , test_MonadTrans_Reader (Proxy :: Proxy (ComposeT (IdentityT) (ReaderT Identity Int)))  (pT2 pU pI) pId pI
    , test_MonadTrans_Reader (Proxy :: Proxy (ComposeT (MaybeT)    (ReaderT Identity Bool))) (pT2 pU pB) pId pB
    , test_MonadTrans_Reader (Proxy :: Proxy (ComposeT (MaybeT)    (ReaderT Identity Int)))  (pT2 pU pI) pId pI
    ]
  ]



-- Test Functor, Applicative, and Monad instances for a concrete monad transformer over several concrete monads.
test_MonadTrans_FAM
  :: forall t wt
   . ( MonadTrans t, Typeable t
     , Show wt
       , forall u m
           . ( Show u
             , forall x. (Show x) => Show (m x) )
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

    , test_MonadTrans_Monad_FAM proxyT proxyWT (Proxy :: Proxy (State Identity Bool)) pB
    , test_MonadTrans_Monad_FAM proxyT proxyWT (Proxy :: Proxy (State Identity Int))  pI

    , test_MonadTrans_Monad_FAM proxyT proxyWT (Proxy :: Proxy (Reader Identity Bool)) pB
    , test_MonadTrans_Monad_FAM proxyT proxyWT (Proxy :: Proxy (Reader Identity Int))  pI

    , test_MonadTrans_Monad_FAM proxyT proxyWT (Proxy :: Proxy (Writer Identity Bool)) pU
    , test_MonadTrans_Monad_FAM proxyT proxyWT (Proxy :: Proxy (Writer Identity Int))  pU

    , test_MonadTrans_Monad_FAM proxyT proxyWT (Proxy :: Proxy (Except Identity Bool)) pU
    , test_MonadTrans_Monad_FAM proxyT proxyWT (Proxy :: Proxy (Except Identity Int))  pU
    ]



-- Test Functor, Applicative, and Monad instances for a concrete monad transformer over a specific concrete monad.
test_MonadTrans_Monad_FAM
  :: forall m t wm wt
   . ( Functor m, Applicative m, Monad m, Typeable m, MonadTrans t, Typeable t
     , Show wt, Show wm
       , forall u
           . ( Show u )
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
           . ( Monad m, MonadTrans t2, Show u
             , forall x. (Show x) => Show (t2 m x) )
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

  , test_MonadTrans_ComposeT_Monad_FAM proxyT1 proxyWT1 (Proxy :: Proxy MaybeT) pU

  , test_MonadTrans_ComposeT_Monad_FAM proxyT1 proxyWT1 (Proxy :: Proxy (ExceptT Identity Bool)) pU
  , test_MonadTrans_ComposeT_Monad_FAM proxyT1 proxyWT1 (Proxy :: Proxy (ExceptT Identity Int))  pU

  , test_MonadTrans_ComposeT_Monad_FAM proxyT1 proxyWT1 (Proxy :: Proxy (WriterT Identity Bool)) pU
  , test_MonadTrans_ComposeT_Monad_FAM proxyT1 proxyWT1 (Proxy :: Proxy (WriterT Identity Int))  pU

  , test_MonadTrans_ComposeT_Monad_FAM proxyT1 proxyWT1 (Proxy :: Proxy (ReaderT Identity Bool)) pB
  , test_MonadTrans_ComposeT_Monad_FAM proxyT1 proxyWT1 (Proxy :: Proxy (ReaderT Identity Int))  pI

  , test_MonadTrans_ComposeT_Monad_FAM proxyT1 proxyWT1 (Proxy :: Proxy (StateT Identity Bool)) pB
  , test_MonadTrans_ComposeT_Monad_FAM proxyT1 proxyWT1 (Proxy :: Proxy (StateT Identity Int))  pI
  ]



-- Test Functor, Applicative, and Monad laws for the composite of two concrete monad transformers over several concrete monads.
test_MonadTrans_ComposeT_Monad_FAM
  :: forall t1 wt1 t2 wt2
   . ( MonadTrans t1, Typeable t1, MonadTrans t2, Typeable t2
     , Show wt1, Show wt2
       , forall u m
           . ( Monad m, Show u
             , forall x. (Show x) => Show (m x) )
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

    , test_Monad_FAM (Proxy :: Proxy (ComposeT t1 t2 (State Identity Bool))) (pT2 (pT2 proxyWT1 proxyWT2) pB)
    , test_Monad_FAM (Proxy :: Proxy (ComposeT t1 t2 (State Identity Int)))  (pT2 (pT2 proxyWT1 proxyWT2) pI)

    , test_Monad_FAM (Proxy :: Proxy (ComposeT t1 t2 (Reader Identity Bool))) (pT2 (pT2 proxyWT1 proxyWT2) pB)
    , test_Monad_FAM (Proxy :: Proxy (ComposeT t1 t2 (Reader Identity Int)))  (pT2 (pT2 proxyWT1 proxyWT2) pI)
    ]



-- Test the MonadTrans laws for a concrete monad transformer over several concrete monads.
test_MonadTrans_T
  :: forall t wt
   . ( MonadTrans t, Typeable t
     , Show wt
       , forall u m
           . ( Show u
             , forall x. (Show x) => Show (m x) )
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

      , testMonadTransLaws2 proxyT (Proxy :: Proxy (State Identity Bool)) (pT2 proxyWT pB) pB pI eqIn
      , testMonadTransLaws2 proxyT (Proxy :: Proxy (State Identity Int))  (pT2 proxyWT pI) pB pI eqIn

      , testMonadTransLaws2 proxyT (Proxy :: Proxy (Reader Identity Bool)) (pT2 proxyWT pB) pB pI eqIn
      , testMonadTransLaws2 proxyT (Proxy :: Proxy (Reader Identity Int))  (pT2 proxyWT pI) pB pI eqIn

      , testMonadTransLaws2 proxyT (Proxy :: Proxy (Writer Identity Bool)) (pT2 proxyWT pU) pB pI eqIn
      , testMonadTransLaws2 proxyT (Proxy :: Proxy (Writer Identity Int))  (pT2 proxyWT pU) pB pI eqIn

      , testMonadTransLaws2 proxyT (Proxy :: Proxy (Except Identity Bool)) (pT2 proxyWT pU) pB pI eqIn
      , testMonadTransLaws2 proxyT (Proxy :: Proxy (Except Identity Int))  (pT2 proxyWT pU) pB pI eqIn
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
           . ( Show u )
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

    , test_MonadTrans_Monad_State proxyT proxyWT proxyMark proxyS (Proxy :: Proxy (Except Identity Bool)) pU
    , test_MonadTrans_Monad_State proxyT proxyWT proxyMark proxyS (Proxy :: Proxy (Except Identity Int))  pU
    ]



-- Test the MonadState laws for a concrete monad transformer and concrete monad over several value types.
test_MonadTrans_Monad_State
  :: forall m t wm wt s mark
   . ( Functor m, Applicative m, Monad m, MonadTrans t, MonadIdentity mark
     , Typeable m, Typeable t, Typeable mark, Typeable s
     , Eq (mark s), Show (mark s), Arbitrary (mark s), CoArbitrary (mark s)
     , Show wt, Show wm
       , forall u
           . ( Show u )
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
           . ( Show u, forall x. (Show x) => Show (m x) )
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

    , test_MonadTrans_Monad_Except proxyT proxyWT proxyMark proxyS (Proxy :: Proxy (Except Q Bool)) pU
    , test_MonadTrans_Monad_Except proxyT proxyWT proxyMark proxyS (Proxy :: Proxy (Except Q Int))  pU
    ]



-- Test the MonadError laws for a concrete monad transformer and concrete monad over several value types.
test_MonadTrans_Monad_Except
  :: forall m t wm wt e mark
   . ( Functor m, Applicative m, Monad m, MonadTrans t, MonadIdentity mark
     , Typeable m, Typeable t, Typeable mark, Typeable e
     , Eq (mark e), Show (mark e), Arbitrary (mark e), CoArbitrary (mark e)
     , Show wt, Show wm
       , forall u
           . ( Show u, forall x. (Show x) => Show (m x) )
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
test_MonadTrans_Writer
  :: forall t wt w mark
   . ( MonadTrans t, MonadIdentity mark
     , Typeable t, Typeable mark, Typeable w, Monoid (mark w)
     , Eq (mark w), Show (mark w), Arbitrary (mark w), CoArbitrary (mark w)
     , Show wt
       , forall u m
           . ( Show u, Show u, forall x. (Show x) => Show (m x) )
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
        => MonadWriter mark w (t m) )
  => Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (wt :: *)
  -> Proxy (mark :: * -> *)
  -> Proxy (w :: *)
  -> TestTree
test_MonadTrans_Writer proxyT proxyWT proxyMark proxyS =
  testGroup ""
    [ test_MonadTrans_Monad_Writer proxyT proxyWT proxyMark proxyS (Proxy :: Proxy Identity) pU

    , test_MonadTrans_Monad_Writer proxyT proxyWT proxyMark proxyS (Proxy :: Proxy Maybe) pU

    , test_MonadTrans_Monad_Writer proxyT proxyWT proxyMark proxyS (Proxy :: Proxy (Except Q Bool)) pU
    , test_MonadTrans_Monad_Writer proxyT proxyWT proxyMark proxyS (Proxy :: Proxy (Except Q Int))  pU
    ]



-- Test the MonadWriter laws for a concrete monad transformer and concrete monad over several value types.
test_MonadTrans_Monad_Writer
  :: forall m t wm wt w mark
   . ( Functor m, Applicative m, Monad m, MonadTrans t, MonadIdentity mark
     , Typeable m, Typeable t, Typeable mark, Typeable w, Monoid (mark w)
     , Eq (mark w), Show (mark w), Arbitrary (mark w), CoArbitrary (mark w)
     , Show wt, Show wm
       , forall u. ( Show u ) => Show (m u)
       , forall u. ( Show u ) => Show (t m u)
     , Arbitrary wt, Arbitrary wm
       , forall u
           . ( Arbitrary u )
          => Arbitrary (t m u)
     , forall u
         . ( Eq u )
        => EqIn (wt,wm) (t m u)
     , MonadWriter mark w (t m) )
  => Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (wt :: *)
  -> Proxy (mark :: * -> *)
  -> Proxy (w :: *)
  -> Proxy (m :: * -> *)
  -> Proxy (wm :: *)
  -> TestTree
test_MonadTrans_Monad_Writer proxyT proxyWT proxyMark proxyE proxyM proxyWM =
  testGroup "MonadTrans (FAM)"
    [ testWriterMonadLaws (Proxy :: Proxy (t m)) (pT2 proxyWT proxyWM) (pAp proxyMark proxyE) pI pI eqIn tell (fixDraft draft)
    , testWriterMonadLaws (Proxy :: Proxy (t m)) (pT2 proxyWT proxyWM) (pAp proxyMark proxyE) pB pB eqIn tell (fixDraft draft)
    ]



-- Test the MonadReader laws for a concrete monad transformer over several concrete monads.
test_MonadTrans_Reader
  :: forall t wt r mark
   . ( MonadTrans t, MonadIdentity mark
     , Typeable t, Typeable mark, Typeable r
     , Eq (mark r), Show (mark r), Arbitrary (mark r), CoArbitrary (mark r)
     , Show wt
       , forall u m
           . ( Show u )
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
        => MonadReader mark r (t m) )
  => Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (wt :: *)
  -> Proxy (mark :: * -> *)
  -> Proxy (r :: *)
  -> TestTree
test_MonadTrans_Reader proxyT proxyWT proxyMark proxyR =
  testGroup ""
    [ test_MonadTrans_Monad_Reader proxyT proxyWT proxyMark proxyR (Proxy :: Proxy Identity) pU

    , test_MonadTrans_Monad_Reader proxyT proxyWT proxyMark proxyR (Proxy :: Proxy Maybe) pU

    , test_MonadTrans_Monad_Reader proxyT proxyWT proxyMark proxyR (Proxy :: Proxy (Except Identity Bool)) pU
    , test_MonadTrans_Monad_Reader proxyT proxyWT proxyMark proxyR (Proxy :: Proxy (Except Identity Int))  pU
    ]



-- Test the MonadReader laws for a concrete monad transformer and concrete monad over several value types.
test_MonadTrans_Monad_Reader
  :: forall m t wm wt r mark
   . ( Functor m, Applicative m, Monad m, MonadTrans t, MonadIdentity mark
     , Typeable m, Typeable t, Typeable mark, Typeable r
     , Eq (mark r), Show (mark r), Arbitrary (mark r), CoArbitrary (mark r)
     , Show wt, Show wm
       , forall u
           . ( Show u )
          => Show (t m u)
     , Arbitrary wt, Arbitrary wm
       , forall u
           . ( Arbitrary u )
          => Arbitrary (t m u)
     , forall u
         . ( Eq u )
        => EqIn (wt,wm) (t m u)
     , MonadReader mark r (t m) )
  => Proxy (t :: (* -> *) -> * -> *)
  -> Proxy (wt :: *)
  -> Proxy (mark :: * -> *)
  -> Proxy (r :: *)
  -> Proxy (m :: * -> *)
  -> Proxy (wm :: *)
  -> TestTree
test_MonadTrans_Monad_Reader proxyT proxyWT proxyMark proxyR proxyM proxyWM =
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
    [ test_MonadTransTrans_FAM (Proxy :: Proxy (IdentityTT IdentityT Identity)) (pT2 pU (pT2 pU pU))
    , test_MonadTransTrans_FAM (Proxy :: Proxy (IdentityTT IdentityT Maybe))    (pT2 pU (pT2 pU pU))

    , test_MonadTransTrans_FAM (Proxy :: Proxy (IdentityTT (ReaderT Identity Bool) Identity)) (pT2 pU (pT2 pB pU))
    , test_MonadTransTrans_FAM (Proxy :: Proxy (IdentityTT (ReaderT Identity Int)  Identity)) (pT2 pU (pT2 pI pU))

    , test_MonadTransTrans_FAM (Proxy :: Proxy (IdentityTT (ReaderT Identity Bool) Maybe)) (pT2 pU (pT2 pB pU))
    , test_MonadTransTrans_FAM (Proxy :: Proxy (IdentityTT (ReaderT Identity Int)  Maybe)) (pT2 pU (pT2 pI pU))

    , test_MonadTransTrans_FAM (Proxy :: Proxy (IdentityTT (ExceptT Identity Bool) Identity)) (pT2 pU (pT2 pU pU))
    , test_MonadTransTrans_FAM (Proxy :: Proxy (IdentityTT (ExceptT Identity Int)  Identity)) (pT2 pU (pT2 pU pU))

    , test_MonadTransTrans_FAM (Proxy :: Proxy (IdentityTT (ExceptT Identity Bool) Maybe)) (pT2 pU (pT2 pU pU))
    , test_MonadTransTrans_FAM (Proxy :: Proxy (IdentityTT (ExceptT Identity Int)  Maybe)) (pT2 pU (pT2 pU pU))
    ]

  , testGroup "PromptTT"
    [ test_MonadTransTrans_FAM (Proxy :: Proxy (PromptTT Identity IdentityT Identity)) (pT2 (pEval pId pId) (pT2 pU pU))
    , test_MonadTransTrans_FAM (Proxy :: Proxy (PromptTT Identity IdentityT Maybe))    (pT2 (pEval pId pMb) (pT2 pU pU))
    ]
  ]



test_MonadTransTrans_FAM
  :: ( Monad m, Typeable m, MonadTrans t, Typeable t
     , Show w
       , forall u 
           . ( Show u )
          => Show (t m u)
     , Arbitrary w
       , forall u
           . ( Arbitrary u )
          => Arbitrary (t m u)
     , forall u. (Eq u) => EqIn w (t m u) )
  => Proxy (t m :: * -> *)
  -> Proxy (w :: *)
  -> TestTree
test_MonadTransTrans_FAM proxyTM proxyW =
  testGroup "Monad (FAM)"
    [ testFunctorLaws2     proxyTM proxyW pB pI eqIn
    , testApplicativeLaws2 proxyTM proxyW pB pI eqIn
    , testMonadLaws2       proxyTM proxyW pB pI eqIn
    ]
