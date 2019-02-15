{-#
  LANGUAGE
    InstanceSigs,
    KindSignatures,
    FlexibleInstances,
    MultiParamTypeClasses
#-}

module Control.FX.Monad.ReadOnly (
    ReadOnly(..)
  , runReadOnly
) where

import Data.Typeable (Typeable)

import Control.FX.Functor
import Control.FX.Monad.Class
import Control.FX.Monad.Identity

newtype ReadOnly
  (k :: * -> *)
  (r :: *)
  (a :: *)
    = ReadOnly
        { unReadOnly :: r -> a
        } deriving (Typeable)

instance Functor (ReadOnly mark r) where
  fmap :: (a -> b) -> ReadOnly mark r a -> ReadOnly mark r b
  fmap f (ReadOnly x) = ReadOnly (f . x)

instance Applicative (ReadOnly mark r) where
  pure :: a -> ReadOnly mark r a
  pure = ReadOnly . const

  (<*>) :: ReadOnly mark r (a -> b) -> ReadOnly mark r a -> ReadOnly mark r b
  (ReadOnly f) <*> (ReadOnly x) =
    ReadOnly $ \r -> (f r) (x r)

instance Monad (ReadOnly mark r) where
  return :: a -> ReadOnly mark r a
  return x = ReadOnly $ \_ -> x

  (>>=) :: ReadOnly mark r a -> (a -> ReadOnly mark r b) -> ReadOnly mark r b
  (ReadOnly x) >>= f = ReadOnly $ \r ->
    let a = x r
    in unReadOnly (f a) r

instance RunMonad r (ReadOnly mark r) Identity where
  run r (ReadOnly x) = Identity (x r)

runReadOnly :: r -> ReadOnly mark r a -> a
runReadOnly r = unIdentity . run r



{- Effect Class -}

instance
  ( MonadIdentity mark
  ) => MonadReadOnly mark r (ReadOnly mark r)
  where
    ask :: ReadOnly mark r (mark r)
    ask = ReadOnly pure

    local f (ReadOnly x) = ReadOnly $ \r ->
      x (unwrap $ f $ pure r)
