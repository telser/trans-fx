-- | Module      : Control.FX
--   Description : Root module for the trans-fx-core effect framework
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

module Control.FX (
  -- * Effect Classes
    MonadIdentity(..)
  , MonadHalt(..)
  , MonadExcept(..)
  , MonadState(..)
  , MonadWriteOnly(..)
  , MonadReadOnly(..)
  , MonadAppendOnly(..)
  , MonadWriteOnce(..)
  , MonadPrompt(..)


  -- * Concrete Types
  -- ** Monads
  , Identity(..)
  , Compose(..)
  , ReadOnly(..)
  , State(..)
  , Except(..)
  , WriteOnly(..)
  , Halt(..)
  , AppendOnly(..)
  , WriteOnce(..)

  -- ** Transformers
  , IdentityT(..)
  , runIdentityT
  , ComposableT(..)
  , ComposeT(..)
  , ReadOnlyT(..)
  , runReadOnlyT
  , StateT(..)
  , runStateT
  , HaltT(..)
  , runHaltT
  , ExceptT(..)
  , runExceptT
  , WriteOnlyT(..)
  , runWriteOnlyT
  , WriteOnceT(..)
  , runWriteOnceT
  , AppendOnlyT(..)
  , runAppendOnlyT

  -- ** Transformer Transformers
  , IdentityTT(..)
  , runIdentityTT
  , PromptTT(..)
  , runPromptTT
  , Eval(..)
  , OverTT(..)
  , runOverTT
  , OverableT(..)
  , StateTT(..)
  , runStateTT
  , ReadOnlyTT(..)
  , runReadOnlyTT
  , WriteOnlyTT(..)
  , runWriteOnlyTT
  , ExceptTT(..)
  , runExceptTT
  , HaltTT(..)
  , runHaltTT
  , AppendOnlyTT(..)
  , runAppendOnlyTT
  , WriteOnceTT(..)
  , runWriteOnceTT


  -- * Values In Context
  -- ** Monads
  , RunMonad(..)
  , Input(..)
  , Output(..)

  -- ** Transformers
  , RunMonadTrans(..)
  , InputT(..)
  , OutputT(..)

  -- ** Transformer Transformers
  , RunMonadTransTrans(..)
  , InputTT(..)
  , OutputTT(..)

  -- ** Equality
  , EqIn(..)
  , Context(..)


  -- * Specialized Lifts
  , Catch
  , LiftCatch(..)
  , LiftCatchT(..)
  , Draft
  , LiftDraft(..)
  , LiftDraftT(..)
  , Local
  , LiftLocal(..)
  , LiftLocalT(..)


  -- * Misc
  , Commutant(..)
  , Central(..)
  , Bifunctor(..)

  , Wrap(..)
  , Renaming(..)

  , Pair(..)

  , LeftZero(..)
  , RightZero(..)
  , IsMaybe(..)

  , MonadTrans(..)
  , MonadFunctor(..)
  , MonadTransTrans(..)
) where

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans
import Control.FX.Monad.Trans.Trans
