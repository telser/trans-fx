-- | Module      : Control.FX.Monad.Identity
--   Description : Concrete identity monad
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.Identity (
    Identity(..)
  , Context(..)
  , Input(..)
  , Output(..)
) where



import Data.Typeable (Typeable)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad.Class



-- | Concrete identity monad
data Identity
  (a :: *)
  = Identity
      { unIdentity :: a -- ^ Extract a pure value
      } deriving (Eq, Show, Typeable)



instance
  Functor Identity
  where
    fmap
      :: (a -> b)
      -> Identity a
      -> Identity b
    fmap f (Identity x) = Identity (f x)

instance
  Applicative Identity
  where
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

instance
  Monad Identity
  where
    return
      :: a
      -> Identity a
    return = Identity

    (>>=)
      :: Identity a
      -> (a -> Identity b)
      -> Identity b
    (Identity x) >>= f = f x

instance
  Commutant Identity
  where
    commute
      :: ( Applicative f )
      => Identity (f a)
      -> f (Identity a)
    commute (Identity x) = Identity <$> x

instance Central Identity

instance
  ( Semigroup a
  ) => Semigroup (Identity a)
  where
    (<>)
      :: Identity a
      -> Identity a
      -> Identity a
    (Identity a) <> (Identity b) =
      Identity (a <> b)

instance
  ( Monoid a
  ) => Monoid (Identity a)
    where
      mempty
        :: Identity a
      mempty = Identity mempty

      mappend
        :: Identity a
        -> Identity a
        -> Identity a
      mappend = (<>)





instance
  EqIn Identity
  where
    data Context Identity
      = IdentityCtx
          { unIdentityCtx :: ()
          } deriving (Eq, Show, Typeable)

    eqIn
      :: (Eq a)
      => Context Identity
      -> Identity a
      -> Identity a
      -> Bool
    eqIn _ = (==)



instance
  RunMonad Identity
  where
    data Input Identity
      = IdentityIn
          { unIdentityIn :: ()
          } deriving (Eq, Show, Typeable)

    data Output Identity a
      = IdentityOut
          { unIdentityOut :: Identity a
          } deriving (Eq, Show, Typeable)

    run
      :: Input Identity
      -> Identity a
      -> Output Identity a
    run _ = IdentityOut





{- Effect Class -}

instance
  MonadIdentity Identity
  where
    unwrap
      :: Identity a
      -> a
    unwrap = unIdentity
