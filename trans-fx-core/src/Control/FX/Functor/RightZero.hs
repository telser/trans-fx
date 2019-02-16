{-#
  LANGUAGE
    InstanceSigs,
    KindSignatures
#-}

module Control.FX.Functor.RightZero (
    RightZero(..)
) where

import Data.Typeable (Typeable)

import Control.FX.Functor.Class

data RightZero
  (a :: *)
    = RightZero a | RightUnit
    deriving (Eq, Show, Typeable)

instance Functor RightZero where
  fmap f x = case x of
    RightZero a -> RightZero (f a)
    RightUnit   -> RightUnit

instance Applicative RightZero where
  pure = RightZero

  f' <*> x' =
    case f' of
      RightUnit   -> RightUnit
      RightZero f -> case x' of
        RightUnit   -> RightUnit
        RightZero x -> RightZero (f x)

instance Semigroup (RightZero a) where
  x <> y =
    case y of
      RightUnit -> x
      _ -> y

instance Monoid (RightZero a) where
  mempty  = RightUnit
  mappend = (<>)

instance Commutant RightZero where
  commute
    :: ( Applicative f )
    => RightZero (f a) -> f (RightZero a)
  commute x =
    case x of
      RightUnit   -> pure RightUnit
      RightZero x -> RightZero <$> x
