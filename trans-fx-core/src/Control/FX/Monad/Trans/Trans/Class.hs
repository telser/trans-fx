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

  -- * Specialized Lifts
  , LiftCatchT(..)
  , LiftDraftT(..)
  , LiftLocalT(..)
  , LiftCoroutineT(..)

  , Val(..)
) where



import Data.Typeable (Typeable)

import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans



-- | Class representing monad transformer transformers
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

-- | Class representing monad functor functors; need to figure out what this means
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





{- Specialized Lifts -}

-- | Class representing monad transformer transformers through
-- which @catch@ from @MonadExcept@ can be lifted.
class
  ( MonadTransTrans u, RunMonadTransTrans z u f
  ) => LiftCatchT z u f
  where
    liftCatchT
      :: ( Monad m, MonadTrans t )
      => (forall x. Catch e (t m) (f x))
      -> (forall x. Catch e (u t m) x)



-- | Class representing monad transformer transformers through
-- which @draft@ from @MonadWriteOnly@ can be lifted.
class
  ( MonadTransTrans u, RunMonadTransTrans z u f
  ) => LiftDraftT z u f
  where
    liftDraftT
      :: ( Monad m, MonadTrans t, Monoid w )
      => (forall x. Draft w (t m) (f x))
      -> (forall x. Draft w (u t m) x)



-- | Class representing monad transformer transformers through
-- which @local@ from @MonadReadOnly@ can be lifted.
class
  ( MonadTransTrans u, RunMonadTransTrans z u f
  ) => LiftLocalT z u f
  where
    liftLocalT
      :: ( Monad m, MonadTrans t )
      => (forall x. Local r (t m) (f x))
      -> (forall x. Local r (u t m) x)


class
  ( MonadTransTrans u, RunMonadTransTrans z u f
  ) => LiftCoroutineT z u f
  where
    liftSuspendT
      :: ( Monad m, MonadTrans t, Functor sus, MonadIdentity mark )
      => (forall x. Suspend mark sus (t m) (f x))
      -> (forall x. Suspend mark sus (u t m) x)

    liftResumeT
      :: ( Monad m, MonadTrans t, Functor sus, MonadIdentity mark )
      => (forall x. Suspend mark sus (t m) (f x))
      -> (forall x. Resume mark sus (t m) (f x))
      -> (forall x. Resume mark sus (u t m) x)



newtype Val
  (a :: *)
  (m :: * -> *)
    = Val { unVal :: a }
