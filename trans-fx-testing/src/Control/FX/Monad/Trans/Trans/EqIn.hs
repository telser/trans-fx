{-#
  LANGUAGE
    GADTs,
    Rank2Types,
    KindSignatures,
    FlexibleInstances,
    QuantifiedConstraints,
    MultiParamTypeClasses
#-}

module Control.FX.Monad.Trans.Trans.EqIn where

import Test.QuickCheck (Arbitrary(..))

import Control.FX.Monad.Trans
import Control.FX.Monad.Trans.Trans
import Control.FX.EqIn.Class

instance
  ( EqIn h (t m a)
  ) => EqIn ((),h) (IdentityTT t m a)
  where
    eqIn ((),h) (IdentityTT x) (IdentityTT y) = eqIn h x y

instance Show (Eval p m) where
  show _ = "Eval"

instance
  ( Monad m, MonadTrans t, EqIn h (t m a)
  ) => EqIn (Eval p m, h) (PromptTT p t m a)
  where
    eqIn (Eval eval, h) x y =
      eqIn h (runPromptTT eval x) (runPromptTT eval y)
