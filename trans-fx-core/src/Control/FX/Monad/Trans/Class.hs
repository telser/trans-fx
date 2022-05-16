-- | Module      : Control.FX.Monad.Trans.Class
--   Description : Monad transformer classes
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE QuantifiedConstraints  #-}

module Control.FX.Monad.Trans.Class (
    MonadFunctor(..)
) where



import Control.FX.Monad



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
