module Control.FX.Data (
  -- * Structure Classes
    IsStack(..)


  -- * Effect Classes
  , MonadStack(..)


  -- * Concrete Types
  -- ** Monads
  , Stack(..)
  , runStack

  -- ** Transformers
  , StackT(..)
  , runStackT

  -- ** Transformer Transformers
  , StackTT(..)
  , runStackTT


  -- * Values in Context
  -- ** Monads
  , Input(..)
  , Output(..)

  -- ** Transformers
  , InputT(..)
  , OutputT(..)

  -- ** Transformer Transformers
  , InputTT(..)
  , OutputTT(..)

  -- ** Equality
  , Context(..)
) where

import Control.FX.Structure.Stack
import Control.FX.Monad.Data.Class
import Control.FX.Monad.Data.Stack
import Control.FX.Monad.Trans.Data.StackT
import Control.FX.Monad.Trans.Trans.Data.StackTT
