{-#
  LANGUAGE
    InstanceSigs,
    KindSignatures
#-}

module Control.FX.Functor.LeftZero (
    LeftZero(..)
) where

import Data.Typeable (Typeable)

import Control.FX.Functor.Class

data LeftZero
  (a :: *)
    = LeftZero a | LeftUnit
    deriving (Eq, Show, Typeable)

instance Functor LeftZero where
  fmap f x = case x of
    LeftZero a -> LeftZero (f a)
    LeftUnit   -> LeftUnit

instance Applicative LeftZero where
  pure = LeftZero

  f' <*> x' =
    case f' of
      LeftUnit   -> LeftUnit
      LeftZero f -> case x' of
        LeftUnit   -> LeftUnit
        LeftZero x -> LeftZero (f x)

instance Semigroup (LeftZero a) where
  x <> y =
    case x of
      LeftUnit -> y
      _ -> x

instance Monoid (LeftZero a) where
  mempty  = LeftUnit
  mappend = (<>)

instance Commutant LeftZero where
  commute
    :: ( Applicative f )
    => LeftZero (f a) -> f (LeftZero a)
  commute x =
    case x of
      LeftUnit   -> pure LeftUnit
      LeftZero x -> LeftZero <$> x
