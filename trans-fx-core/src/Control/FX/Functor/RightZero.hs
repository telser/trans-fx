-- | Module      : Control.FX.Functor.RightZero
--   Description : Right zero semigroup with identity on a type
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Functor.RightZero (
    RightZero(..)
  , Context(..)
) where



import Data.Typeable (Typeable)

import Control.FX.EqIn
import Control.FX.Functor.Class



-- | Type representing the right zero semigroup on @a@ with
-- an identity attached. As a functor @RightZero@ is isomorphic
-- to @Maybe@.
data RightZero
  (a :: *)
    = RightZero a | RightUnit
    deriving (Eq, Show, Typeable)



instance
  IsMaybe RightZero
  where
    fromMaybe
      :: Maybe a
      -> RightZero a
    fromMaybe x = case x of
      Nothing -> RightUnit
      Just a  -> RightZero a

    toMaybe
      :: RightZero a
      -> Maybe a
    toMaybe x = case x of
      RightUnit   -> Nothing
      RightZero a -> Just a



instance
  Functor RightZero
  where
    fmap
      :: (a -> b)
      -> RightZero a
      -> RightZero b
    fmap f x = case x of
      RightZero a -> RightZero (f a)
      RightUnit   -> RightUnit

instance
  Applicative RightZero
  where
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

instance
  Semigroup (RightZero a)
  where
    (<>)
      :: RightZero a
      -> RightZero a
      -> RightZero a
    x <> y =
      case y of
        RightUnit -> x
        _ -> y

instance
  Monoid (RightZero a)
  where
    mempty
      :: RightZero a
    mempty = RightUnit

    mappend
      :: RightZero a
      -> RightZero a
      -> RightZero a
    mappend = (<>)

instance
  Commutant RightZero
  where
    commute
      :: ( Applicative f )
      => RightZero (f a) -> f (RightZero a)
    commute x =
      case x of
        RightUnit   -> pure RightUnit
        RightZero x -> RightZero <$> x

instance
  EqIn RightZero
  where
    newtype Context RightZero
      = RightZeroCtx
          { unRightZeroCtx :: ()
          } deriving (Eq, Show)

    eqIn
      :: (Eq a)
      => Context RightZero
      -> RightZero a
      -> RightZero a
      -> Bool
    eqIn _ = (==)
