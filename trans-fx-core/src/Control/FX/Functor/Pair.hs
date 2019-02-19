-- | Module      : Control.FX.Functor.Pair
--   Description : Pair type
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE InstanceSigs   #-}
{-# LANGUAGE KindSignatures #-}

module Control.FX.Functor.Pair (
    Pair(..)
) where



import Data.Typeable (Typeable)

import Control.FX.Functor.Class



-- | Tuple type, isomorphic to @(a,b)@. This is here so we can have a partially applied tuple type @Pair a@ without syntax hacks.
data Pair
  (a1 :: *)
  (a2 :: *)
    = Pair
       { slot1 :: a1, slot2 :: a2
       } deriving (Eq, Show, Typeable)

instance Functor (Pair a1) where
  fmap f (Pair a1 a2) = Pair a1 (f a2)

instance Commutant (Pair a1) where
  commute (Pair a1 x) = fmap (Pair a1) x
