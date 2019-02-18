{-# LANGUAGE KindSignatures #-}

module Control.FX.Monad.Unit where

import Data.Typeable (Typeable)

data Unit (a :: * -> *)
  = Unit
  deriving (Eq, Show, Typeable)

data Dub
  (z1 :: (* -> *) -> *)
  (z2 :: (* -> *) -> *)
  (m :: * -> *)
    = Dub (z1 m) (z2 m)

data Sing
  (z :: (* -> *) -> *)
  (y :: *)
  (m :: * -> *)
    = Sing (z m) y
