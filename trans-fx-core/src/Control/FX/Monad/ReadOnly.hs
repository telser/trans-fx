-- | Module      : Control.FX.Monad.ReadOnly
--   Description : Concrete read-only state monad
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.ReadOnly (
    ReadOnly(..)
) where



import Data.Typeable (Typeable, typeOf)

import Control.FX.EqIn

import Control.FX.Functor
import Control.FX.Monad.Class
import Control.FX.Monad.Identity



-- | Concrete read-only state monad
newtype ReadOnly
  (mark :: * -> *)
  (r :: *)
  (a :: *)
    = ReadOnly
        { unReadOnly :: r -> a
        } deriving (Typeable)

type instance Context (ReadOnly mark r)
  = mark r

instance
  ( MonadIdentity mark
  ) => EqIn (ReadOnly mark r)
  where
    eqIn
      :: (Eq a)
      => mark r
      -> ReadOnly mark r a
      -> ReadOnly mark r a
      -> Bool
    eqIn r (ReadOnly x) (ReadOnly y) =
      (x $ unwrap r) == (y $ unwrap r)



instance
  ( Typeable r, Typeable a, Typeable mark
  ) => Show (ReadOnly mark r a)
  where
    show
      :: ReadOnly mark r a
      -> String
    show = show . typeOf

instance
  ( MonadIdentity mark
  ) => Functor (ReadOnly mark r)
  where
    fmap
      :: (a -> b)
      -> ReadOnly mark r a
      -> ReadOnly mark r b
    fmap f (ReadOnly x) = ReadOnly (f . x)

instance
  ( MonadIdentity mark
  ) => Applicative (ReadOnly mark r)
  where
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

instance
  ( MonadIdentity mark
  ) => Monad (ReadOnly mark r)
  where
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
  ( MonadIdentity mark, Commutant mark
  ) => RunMonad (mark r) (ReadOnly mark r) mark
  where
    run
      :: mark r
      -> ReadOnly mark r a
      -> mark a
    run r (ReadOnly x) = pure (x (unwrap r))



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
