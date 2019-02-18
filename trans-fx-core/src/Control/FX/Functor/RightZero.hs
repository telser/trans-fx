-- | Module      : Control.FX.Functor.RightZero
--   Description : Right zero semigroup with identity on a type
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE InstanceSigs   #-}
{-# LANGUAGE KindSignatures #-}

module Control.FX.Functor.RightZero (
    RightZero(..)
) where



import Data.Typeable (Typeable)

import Control.FX.Functor.Class



-- | Type representing the right zero semigroup on @a@ with
-- an identity attached. As a functor @RightZero@ is isomorphic
-- to @Maybe@.
data RightZero
  (a :: *)
    = RightZero a | RightUnit
    deriving (Eq, Show, Typeable)

instance Functor RightZero where
  fmap
    :: (a -> b)
    -> RightZero a
    -> RightZero b
  fmap f x = case x of
    RightZero a -> RightZero (f a)
    RightUnit   -> RightUnit

instance Applicative RightZero where
  pure
    :: a
    -> RightZero a
  pure = RightZero

  (<*>)
    :: RightZero (a -> b)
    -> RightZero a
    -> RightZero b
  f' <*> x' =
    case f' of
      RightUnit   -> RightUnit
      RightZero f -> case x' of
        RightUnit   -> RightUnit
        RightZero x -> RightZero (f x)

instance Semigroup (RightZero a) where
  (<>)
    :: RightZero a
    -> RightZero a
    -> RightZero a
  x <> y =
    case y of
      RightUnit -> x
      _ -> y

instance Monoid (RightZero a) where
  mempty
    :: RightZero a
  mempty = RightUnit

  mappend
    :: RightZero a
    -> RightZero a
    -> RightZero a
  mappend = (<>)

instance Commutant RightZero where
  commute
    :: ( Applicative f )
    => RightZero (f a) -> f (RightZero a)
  commute x =
    case x of
      RightUnit   -> pure RightUnit
      RightZero x -> RightZero <$> x
