{-#
  LANGUAGE
    FlexibleInstances,
    MultiParamTypeClasses
#-}

module Control.FX.Monad.EqIn where

import Control.FX.Monad
import Control.FX.EqIn.Class

instance
  ( Eq a
  ) => EqIn () (Identity a)
  where
    eqIn () x y = x == y

instance
  ( Eq a
  ) => EqIn () (Maybe a)
  where
    eqIn () x y = x == y

instance
  ( Eq a, Eq b
  ) => EqIn () (Either a b)
  where
    eqIn () x y = x == y

instance
  ( Eq a, Eq b
  ) => EqIn () (Except mark a b)
  where
    eqIn () x y = x == y

instance
  ( Eq a, Eq b
  ) => EqIn () (Writer mark a b)
  where
    eqIn () x y = x == y

instance
  ( Eq a, Eq b
  ) => EqIn () (Tagged a b)
  where
    eqIn () x y = x == y

instance
  ( Eq a
  ) => EqIn r (Reader mark r a)
  where
    eqIn r (Reader x) (Reader y) =
      (x r) == (y r)

instance
  ( Eq a, Eq s
  ) => EqIn s (State mark s a)
  where
    eqIn s (State x) (State y) =
      (x s) == (y s)
