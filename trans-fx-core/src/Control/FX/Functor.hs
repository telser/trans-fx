-- | Module      : Control.FX.Functor
--   Description : Root Functor module for the trans-fx-core effect framework
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

module Control.FX.Functor (
    module Control.FX.Functor.Class
  , module Control.FX.Functor.Pair
  , module Control.FX.Functor.Tag
  , module Control.FX.Functor.LeftZero
  , module Control.FX.Functor.RightZero
) where

import Control.FX.Functor.Class
import Control.FX.Functor.Pair
import Control.FX.Functor.Tag
import Control.FX.Functor.LeftZero
import Control.FX.Functor.RightZero
