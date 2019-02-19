-- | Module      : Control.FX.IO.Monad.Trans.Trans.Class
--   Description : Basic IO related monadic effect classes
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.IO.Monad.Trans.Trans.Class (
    MonadTeletype(..)
) where



import Control.FX



-- | Class representing monads which can interact with a teletype-style interface
class
  ( Monad m, MonadIdentity mark
  ) => MonadTeletype mark m
  where
    -- | Read a line of input
    readLine :: m (mark String)

    -- | Print a line of output
    printLine :: mark String -> m ()
