{-#
  LANGUAGE
    FlexibleInstances,
    MultiParamTypeClasses
#-}

module Control.FX.Functor.EqIn where

import Control.FX.Functor
import Control.FX.EqIn.Class

instance
  ( Eq a
  ) => EqIn () (LeftZero a)
  where
    eqIn () x y = x == y

instance
  ( Eq a
  ) => EqIn () (RightZero a)
  where
    eqIn () x y = x == y

instance
  ( Eq a, Eq b
  ) => EqIn () (Pair a b)
  where
    eqIn () x y = x == y
