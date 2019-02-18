{-#
  LANGUAGE
    MultiParamTypeClasses
#-}

module Control.FX.IO.Monad.Trans.Trans.Class (
    MonadTeletype(..)
) where

import Control.FX

class
  ( Monad m, MonadIdentity mark
  ) => MonadTeletype mark m
  where
    readLine :: m (mark String)

    printLine :: mark String -> m ()
