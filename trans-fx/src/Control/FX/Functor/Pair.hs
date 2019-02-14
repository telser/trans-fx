{-#
  LANGUAGE
    InstanceSigs,
    KindSignatures
#-}

module Control.FX.Functor.Pair (
    Pair(..)
  , toTup
  , fromTup
) where

import Data.Typeable (Typeable)

import Control.FX.Functor.Class

data Pair
  (a1 :: *)
  (a2 :: *)
    = Pair
       { slot1 :: a1, slot2 :: a2
       } deriving (Eq, Show, Typeable)

toTup :: Pair a b -> (a,b)
toTup (Pair a b) = (a,b)

fromTup :: (a,b) -> Pair a b
fromTup (a,b) = Pair a b

instance Functor (Pair a1) where
  fmap f (Pair a1 a2) = Pair a1 (f a2)

instance Central (Pair a1) where
  commute :: (Applicative g) => Pair a1 (g a) -> g (Pair a1 a)
  commute (Pair a1 x) = fmap (Pair a1) x
