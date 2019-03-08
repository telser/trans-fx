-- | Module      : Control.FX.EqIn
--   Description : Class of types which can be compared for equality in a context
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.EqIn (
    EqIn(..)
  , Context(..)
) where



import Data.Typeable (Typeable, typeOf)



-- | Class representing types which can be compared
-- for equality within an environment. Instances
-- should satisfy the following laws:
--
-- > (1) eqIn env x x  ===  True
-- >
-- > (2) eqIn env x y  ===  eqIn env y x
-- >
-- > (3) if (eqIn env x y) && (eqIn env y z) then eqIn env x z else True
class EqIn (t :: * -> *)
  where
    data Context t

    eqIn
      :: (Eq a)
      => Context t
      -> t a
      -> t a
      -> Bool



instance
  EqIn Maybe
  where
    data Context Maybe
      = MaybeCtx
          { unMaybeCtx :: ()
          } deriving (Eq, Show)

    eqIn
      :: (Eq a)
      => Context Maybe
      -> Maybe a
      -> Maybe a
      -> Bool
    eqIn _ = (==)



instance
  ( Eq a
  ) => EqIn (Either a)
  where
    data Context (Either a)
       = EitherCtx
          { unEitherCtx :: ()
          } deriving (Eq, Show)

    eqIn
      :: (Eq b)
      => Context (Either a)
      -> Either a b
      -> Either a b
      -> Bool
    eqIn _ = (==)
