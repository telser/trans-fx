{-#
  LANGUAGE
    InstanceSigs,
    KindSignatures,
    FlexibleInstances,
    MultiParamTypeClasses
#-}

module Control.FX.Monad.Apply (
    Apply(..)
  , runApply
) where

import Data.Typeable (Typeable)

import Control.FX.Functor
import Control.FX.Monad.Class

newtype Apply
  (m :: * -> *)
  (a :: *)
    = Apply
        { unApply :: m a
        } deriving (Typeable)

instance
  ( Functor m
  ) => Functor (Apply m)
  where
    fmap :: (a -> b) -> Apply m a -> Apply m b
    fmap f (Apply x) = Apply (fmap f x)

instance
  ( Applicative m
  ) => Applicative (Apply m)
  where
    pure :: a -> Apply m a
    pure = Apply . pure

    (Apply f) <*> (Apply x) =
      Apply (f <*> x)

instance
  ( Monad m
  ) => Monad (Apply m)
  where
    return = Apply . return

    (Apply x) >>= f =
      Apply (x >>= (unApply . f))

instance
  ( Commutant c
  ) => Commutant (Apply c)
  where
    commute :: (Applicative f) => Apply c (f a) -> f (Apply c a)
    commute = fmap Apply . commute . unApply

instance
  ( Central c
  ) => Central (Apply c)

instance
  ( RunMonad z m f
  ) => RunMonad z (Apply m) f
  where
    run :: z -> Apply m a -> f a
    run z (Apply x) = run z x

runApply
  :: (RunMonad z m f)
  => z -> Apply m a -> f a
runApply = run
