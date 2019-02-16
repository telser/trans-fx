{-# LANGUAGE KindSignatures #-}

module Control.FX.Monad.Unit where

import Data.Typeable (Typeable)

data Unit (a :: * -> *)
  = Unit
  deriving (Eq, Show, Typeable)

data Dub
  (z :: (* -> *) -> *)
  (m :: * -> *)
    = Dub (z m) (z m)

data Sing
  (z :: (* -> *) -> *)
  (y :: *)
  (m :: * -> *)
    = Sing (z m) y
