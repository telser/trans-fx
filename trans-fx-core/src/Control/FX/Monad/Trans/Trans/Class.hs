-- | Module      : Control.FX.Monad.Trans.Trans.Class
--   Description : Monad transformer transformer classes
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.FX.Monad.Trans.Trans.Class (
    MonadTransTrans(..)
  , MonadTransFunctor(..)
  , RunMonadTransTrans(..)
) where



import Data.Typeable (Typeable)

import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans



class
  ( forall t. (MonadTrans t) => MonadTrans (u t)
  , forall t m. (Monad m, MonadTrans t) => Monad (u t m)
  ) => MonadTransTrans
    (u :: ((* -> *) -> (* -> *)) -> (* -> *) -> * -> *)
  where
    liftT
      :: (Monad m, MonadTrans t)
      => t m a -> u t m a

instance MonadTransTrans ApplyT where
  liftT = ApplyT

class
  ( MonadTransTrans u
  , forall t. (MonadFunctor t) => MonadFunctor (u t)
  ) => MonadTransFunctor u
  where
    hoistT
      :: ( Monad m, MonadFunctor t1, MonadFunctor t2 )
      => (forall n w. (Monad n) => t1 n w -> t2 n w)
      -> u t1 m a
      -> u t2 m a

    raiseT
      :: ( Monad m1, Monad m2, MonadFunctor t )
      => (forall w. m1 w -> m2 w)
      -> u t m1 a
      -> u t m2 a

instance
  MonadTransFunctor ApplyT
  where
    hoistT f = ApplyT . f . unApplyT
    raiseT f = ApplyT . hoist f . unApplyT



-- | Class representing monad transformer transformers which can be "run" in a context @z m@, producing a value in context @t m (f a)@.
class
  ( MonadTransTrans u, Commutant f
  ) => RunMonadTransTrans z u f | u -> z f
  where
    runTT
      :: (Monad m, MonadTrans t)
      => z m -> u t m a -> t m (f a)
