-- | Module      : Control.FX.Monad.Trans.Class
--   Description : Monad transformer classes
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.FX.Monad.Trans.Class (
    MonadTrans(..)
  , MonadFunctor(..)
) where



import Control.FX.Functor
import Control.FX.Monad





instance
  MonadTrans Apply
  where
    lift
      :: ( Monad m )
      => m a
      -> Apply m a
    lift = Apply

instance
  ( Central c
  ) => MonadTrans (Flip c)
  where
    lift
      :: ( Monad m )
      => m a
      -> Flip c m a
    lift = Flip . commute . return

-- | Class representing monad functors
class
  ( MonadTrans t
  ) => MonadFunctor t
  where
    hoist
      :: ( Monad m, Monad n )
      => (forall u. m u -> n u)
      -> t m a
      -> t n a
