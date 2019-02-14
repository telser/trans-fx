{-#
  LANGUAGE
    InstanceSigs
#-}

module Control.FX.Functor.Tag (
    Tag(..)
) where

import Data.Typeable (Typeable)

import Control.FX.Functor.Class

data Tag a
  = Blank | Tag a
  deriving (Eq, Show, Typeable)

instance Semigroup (Tag a) where
  x <> y =
    case x of
      Blank -> y
      Tag a -> Tag a

instance Monoid (Tag a) where
  mempty = Blank
  mappend = (<>)

instance Functor Tag where
  fmap f x =
    case x of
      Blank -> Blank
      Tag a -> Tag (f a)

instance Central Tag where
  commute :: (Applicative f) => Tag (f a) -> f (Tag a)
  commute x =
    case x of
      Blank -> pure Blank
      Tag y -> fmap Tag y
