-- | Module      : Control.FX.Monad.ReadOnly
--   Description : Concrete read-only state monad
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.ReadOnly (
    ReadOnly(..)
  , runReadOnly
) where



import Data.Typeable (Typeable)

import Control.FX.Functor
import Control.FX.Monad.Class
import Control.FX.Monad.Identity



-- | Concrete read-only state monad
newtype ReadOnly
  (k :: * -> *)
  (r :: *)
  (a :: *)
    = ReadOnly
        { unReadOnly :: r -> a
        } deriving (Typeable)

instance Functor (ReadOnly mark r) where
  fmap
    :: (a -> b)
    -> ReadOnly mark r a
    -> ReadOnly mark r b
  fmap f (ReadOnly x) = ReadOnly (f . x)

instance Applicative (ReadOnly mark r) where
  pure
    :: a
    -> ReadOnly mark r a
  pure = ReadOnly . const

  (<*>)
    :: ReadOnly mark r (a -> b)
    -> ReadOnly mark r a
    -> ReadOnly mark r b
  (ReadOnly f) <*> (ReadOnly x) =
    ReadOnly $ \r -> (f r) (x r)

instance Monad (ReadOnly mark r) where
  return
    :: a
    -> ReadOnly mark r a
  return x = ReadOnly $ \_ -> x

  (>>=)
    :: ReadOnly mark r a
    -> (a -> ReadOnly mark r b)
    -> ReadOnly mark r b
  (ReadOnly x) >>= f = ReadOnly $ \r ->
    let a = x r
    in unReadOnly (f a) r

instance
  ( MonadIdentity mark
  ) => RunMonad (mark r) (ReadOnly mark r) Identity where
  run
    :: mark r
    -> ReadOnly mark r a
    -> Identity a
  run r (ReadOnly x) = Identity (x (unwrap r))

-- | Run a @ReadOnly mark r a@ inside the read-only context @r@, producing an @a@. 
runReadOnly
  :: ( MonadIdentity mark )
  => mark r
  -> ReadOnly mark r a
  -> a
runReadOnly r = unIdentity . run r



{- Effect Class -}

instance
  ( MonadIdentity mark
  ) => MonadReadOnly mark r (ReadOnly mark r)
  where
    ask
      :: ReadOnly mark r (mark r)
    ask = ReadOnly pure

    local
      :: (mark r -> mark r)
      -> ReadOnly mark r a
      -> ReadOnly mark r a
    local f (ReadOnly x) = ReadOnly $ \r ->
      x (unwrap $ f $ pure r)
