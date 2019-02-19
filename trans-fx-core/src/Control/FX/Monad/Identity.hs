-- | Module      : Control.FX.Monad.Identity
--   Description : Concrete identity monad
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.Identity (
    Identity(..)
  , runIdentity
) where



import Data.Typeable (Typeable)

import Control.FX.Functor
import Control.FX.Monad.Class



-- | Concrete identity monad
data Identity
  (a :: *)
  = Identity
      { unIdentity :: a -- ^ Extract a pure value
      } deriving (Eq, Show, Typeable)

instance Functor Identity where
  fmap
    :: (a -> b)
    -> Identity a
    -> Identity b
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure
    :: a
    -> Identity a
  pure = Identity

  (<*>)
    :: Identity (a -> b)
    -> Identity a
    -> Identity b
  (Identity f) <*> (Identity x) =
    Identity (f x)

instance Monad Identity where
  return
    :: a
    -> Identity a
  return = Identity

  (>>=)
    :: Identity a
    -> (a -> Identity b)
    -> Identity b
  (Identity x) >>= f = f x

instance Commutant Identity where
  commute
    :: ( Applicative f )
    => Identity (f a) -> f (Identity a)
  commute (Identity x) = Identity <$> x

instance Central Identity



instance RunMonad () Identity Identity where
  run
    :: ()
    -> Identity a
    -> Identity a
  run () = id

-- | Run an @Identity a@, producing a pure value.
runIdentity
  :: Identity a
  -> a
runIdentity = unIdentity . run ()

instance MonadIdentity Identity where
  unwrap
    :: Identity a
    -> a
  unwrap = unIdentity
