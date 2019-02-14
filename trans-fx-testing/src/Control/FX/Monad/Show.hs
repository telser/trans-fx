module Control.FX.Monad.Show where

import Control.FX.Monad

instance Show (Reader mark r a) where
  show _ = "Reader"

instance Show (State mark s a) where
  show _ = "State"
