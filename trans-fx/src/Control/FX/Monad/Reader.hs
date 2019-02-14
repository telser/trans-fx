{-#
  LANGUAGE
    InstanceSigs,
    KindSignatures,
    FlexibleInstances,
    MultiParamTypeClasses
#-}

module Control.FX.Monad.Reader (
    Reader(..)
  , runReader
) where

import Data.Typeable (Typeable)

import Control.FX.Functor
import Control.FX.Monad.Class
import Control.FX.Monad.Identity

newtype Reader
  (k :: * -> *)
  (r :: *)
  (a :: *)
    = Reader
        { unReader :: r -> a
        } deriving (Typeable)

instance Functor (Reader mark r) where
  fmap :: (a -> b) -> Reader mark r a -> Reader mark r b
  fmap f (Reader x) = Reader (f . x)

instance Applicative (Reader mark r) where
  pure :: a -> Reader mark r a
  pure = Reader . const

  (<*>) :: Reader mark r (a -> b) -> Reader mark r a -> Reader mark r b
  (Reader f) <*> (Reader x) =
    Reader $ \r -> (f r) (x r)

instance Monad (Reader mark r) where
  return :: a -> Reader mark r a
  return x = Reader $ \_ -> x

  (>>=) :: Reader mark r a -> (a -> Reader mark r b) -> Reader mark r b
  (Reader x) >>= f = Reader $ \r ->
    let a = x r
    in unReader (f a) r

instance RunMonad r (Reader mark r) Identity where
  run r (Reader x) = Identity (x r)

runReader :: r -> Reader mark r a -> a
runReader r = unIdentity . run r



{- Effect Class -}

instance
  ( MonadIdentity mark
  ) => MonadReader mark r (Reader mark r)
  where
    ask :: Reader mark r (mark r)
    ask = Reader pure

    local f (Reader x) = Reader $ \r ->
      x (unwrap $ f $ pure r)
