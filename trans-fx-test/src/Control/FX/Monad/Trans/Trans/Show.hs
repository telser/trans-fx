module Control.FX.Monad.Trans.Trans.Show where

import Control.FX.Monad.Trans.Trans

instance Show (PromptTT mark p t m a) where
  show _ = "PromptTT"

instance Show (OverTT u w t m a) where
  show _ = "OverTT"
