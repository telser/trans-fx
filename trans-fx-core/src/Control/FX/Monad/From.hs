{-#
  LANGUAGE
    InstanceSigs,
    KindSignatures,
    FlexibleInstances,
    MultiParamTypeClasses
#-}

module Control.FX.Monad.From (
    From(..)
  , runFrom
) where

import Data.Typeable (Typeable)

import Control.FX.Functor
import Control.FX.Monad.Class
import Control.FX.Monad.Identity

data From
  (d :: *)
  (a :: *)
    = From
        { unFrom :: d -> a
        } deriving (Typeable)

instance Functor (From d) where
  fmap :: (a -> b) -> From d a -> From d b
  fmap f (From k) = From (f . k)

instance Applicative (From d) where
  pure :: a -> From d a
  pure = From . const

  (From f) <*> (From x) =
    From $ \d -> (f d) (x d)

instance Monad (From d) where
  return :: a -> From d a
  return = From . const

  (From x) >>= f =
    From $ \d -> (unFrom . f . x) d d

instance RunMonad d (From d) Identity where
  run :: d -> From d a -> Identity a
  run d (From f) = Identity (f d)

runFrom :: d -> From d a -> a
runFrom d (From f) = f d
