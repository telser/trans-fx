module Control.FX.Monad.Show where

import Control.FX.Monad

instance Show (ReadOnly mark r a) where
  show _ = "ReadOnly"

instance Show (State mark s a) where
  show _ = "State"
