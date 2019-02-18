{-#
  LANGUAGE
    UndecidableInstances
#-}

module Control.FX.Monad.Trans.Show where

import Control.FX.Monad
import Control.FX.Monad.Trans

instance
  ( Show (m (Maybe a))
  ) => Show (MaybeT m a)
  where
    show (MaybeT x) = "MaybeT " ++ show x

instance Show (ReadOnlyT mark r m a) where
  show _ = "ReadOnlyT"

instance Show (StateT mark s m a) where
  show _ = "StateT"

instance
  ( Show (t1 (t2 m) a)
  ) => Show (ComposeT t1 t2 m a)
  where
    show (ComposeT x) = "ComposeT " ++ show x
