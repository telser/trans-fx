{-# LANGUAGE InstanceSigs   #-}
{-# LANGUAGE KindSignatures #-}

module Control.FX.Functor.Yield (
    Yield(..)
) where

import Data.Typeable (Typeable)

data Yield
  (res :: *)
  (a :: *)
    = Yield res a
    deriving (Typeable)

instance
  Functor (Yield res)
  where
    fmap
      :: (a -> b)
      -> Yield res a
      -> Yield res b
    fmap f (Yield res a) = Yield res (f a)
