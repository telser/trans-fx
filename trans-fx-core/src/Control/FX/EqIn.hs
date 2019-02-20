-- | Module      : Control.FX.EqIn
--   Description : Class of types which can be compared for equality in a context
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.EqIn where

-- | Class representing types which can be compared
-- for equality within an environment. Instances
-- should satisfy the following laws:
--
-- > (1) eqIn env x x  ===  True
-- >
-- > (2) eqIn env x y  ===  eqIn env y x
-- >
-- > (3) if (eqIn env x y) && (eqIn env y z) then eqIn env x z else True
class EqIn env a where
  eqIn :: env -> a -> a -> Bool

instance
  ( Eq a
  ) => EqIn () (Maybe a)
  where
    eqIn
      :: ()
      -> Maybe a
      -> Maybe a
      -> Bool
    eqIn () = (==)

instance
  ( Eq a, Eq b
  ) => EqIn () (Either a b)
  where
    eqIn
      :: ()
      -> Either a b
      -> Either a b
      -> Bool
    eqIn () = (==)
