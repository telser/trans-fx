module Control.FX.Monad.Trans.Trans.Show where

import Control.FX.Monad.Trans.Trans

instance Show (PromptTT p t m a) where
  show _ = "PromptTT"
