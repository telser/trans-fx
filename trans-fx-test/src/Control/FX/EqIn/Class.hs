{-#
  LANGUAGE
    MultiParamTypeClasses
#-}

module Control.FX.EqIn.Class where

class EqIn w a where
  eqIn :: w -> a -> a -> Bool
