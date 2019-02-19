-- | Module      : Control.FX.Monad.Apply
--   Description : Concrete application monad
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.Apply (
    Apply(..)
  , runApply
) where



import Data.Typeable (Typeable)

import Control.FX.Functor
import Control.FX.Monad.Class



-- | Concrete application monad
newtype Apply
  (m :: * -> *)
  (a :: *)
    = Apply
        { unApply :: m a
        } deriving (Typeable)

deriving instance
  ( Show (m a)
  ) => Show (Apply m a)

instance
  ( Functor m
  ) => Functor (Apply m)
  where
    fmap
      :: (a -> b)
      -> Apply m a
      -> Apply m b
    fmap f (Apply x) = Apply (fmap f x)

instance
  ( Applicative m
  ) => Applicative (Apply m)
  where
    pure
      :: a
      -> Apply m a
    pure = Apply . pure

    (Apply f) <*> (Apply x) =
      Apply (f <*> x)

instance
  ( Monad m
  ) => Monad (Apply m)
  where
    return
      :: a
      -> Apply m a
    return = Apply . return

    (>>=)
      :: Apply m a
      -> (a -> Apply m b)
      -> Apply m b
    (Apply x) >>= f =
      Apply (x >>= (unApply . f))

instance
  ( Commutant c
  ) => Commutant (Apply c)
  where
    commute
      :: ( Applicative f )
      => Apply c (f a)
      -> f (Apply c a)
    commute = fmap Apply . commute . unApply

instance
  ( Central c
  ) => Central (Apply c)

instance
  ( RunMonad z m f
  ) => RunMonad z (Apply m) f
  where
    run
      :: z
      -> Apply m a
      -> f a
    run z (Apply x) = run z x

-- | Run the applied monad with context @z@, producing an @f a@
runApply
  :: (RunMonad z m f)
  => z
  -> Apply m a
  -> f a
runApply = run
