{-#
  LANGUAGE
    InstanceSigs,
    KindSignatures,
    FlexibleInstances,
    MultiParamTypeClasses
#-}

module Control.FX.Monad.Identity (
    Identity(..)
  , runIdentity
) where

import Data.Typeable (Typeable)

import Control.FX.Functor
import Control.FX.Monad.Class

data Identity (a :: *)
  = Identity
      { unIdentity :: a
      } deriving (Eq, Typeable)

instance (Show a) => Show (Identity a) where
  show (Identity a) = "Identity " ++ show a

instance Functor Identity where
  fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure :: a -> Identity a
  pure = Identity

  (<*>) :: Identity (a -> b) -> Identity a -> Identity b
  (Identity f) <*> (Identity x) =
    Identity (f x)

instance Monad Identity where
  return :: a -> Identity a
  return = Identity

  (>>=) :: Identity a -> (a -> Identity b) -> Identity b
  (Identity x) >>= f = f x

instance Commutant Identity where
  commute
    :: ( Applicative f )
    => Identity (f a) -> f (Identity a)
  commute (Identity x) = Identity <$> x

instance Central Identity

instance RunMonad () Identity Identity where
  run () = id

runIdentity :: Identity a -> a
runIdentity = unIdentity . run ()

instance MonadIdentity Identity where
  unwrap = unIdentity
