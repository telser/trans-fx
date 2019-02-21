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
  , RunMonadTrans(..)

  -- * Specialized Lifts
  , Catch, LiftCatch(..)
  , Draft, LiftDraft(..)
  , Local, LiftLocal(..)
) where



import Control.FX.Functor
import Control.FX.Monad



-- | Class representing monad transformers
class
  ( forall m. (Monad m) => Monad (t m)
  ) => MonadTrans
    (t :: (* -> *) -> * -> *)
  where
    -- | Lift a computation from the inner monad to the transformed monad
    lift
      :: ( Monad m )
      => m a
      -> t m a

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

-- | Class representing monad transformers which can be run in a context @z@, producting a value in a context @f@
class
  ( MonadTrans t, Commutant f
  ) => RunMonadTrans z t f | t -> z f
  where
    runT
      :: ( Monad m )
      => z
      -> t m a
      -> m (f a)



{- Specialized Lifts -}

-- | The signature of @catch@ from @MonadExcept@
type Catch e m a = m a -> (e -> m a) -> m a

-- | Class representing monad transformers through which
-- @catch@ from @MonadExcept@ can be lifted. Instances
-- should satisfy the following law:
--
-- > (1) lift (catch x h) === liftCatch catch (lift x) (lift . h)
class
  ( MonadTrans t, RunMonadTrans z t f
  ) => LiftCatch z t f
  where
    liftCatch
      :: ( Monad m )
      => Catch e m (f a)
      -> Catch e (t m) a



-- | The signature of @draft@ from @MonadWriteOnly@
type Draft w m a = m a -> m (Pair w a)

-- | Class representing monad transformers through which
-- @draft@ from @MonadWriteOnly@ can be lifted. Instances
-- should satisfy the following law:
--
-- > (1) liftDraft draft (lift x) === lift (draft x)
class
  ( MonadTrans t, RunMonadTrans z t f
  ) => LiftDraft z t f
  where
    liftDraft
      :: ( Monad m )
      => Draft w m (f a)
      -> Draft w (t m) a



-- | The signature of @local@ from @MonadReadOnly@
type Local r m a = (r -> r) -> m a -> m a

-- | Class representing monad transformers through which @local@ from @MonadReadOnly@ can be lifted
class
  ( MonadTrans t, RunMonadTrans z t f
  ) => LiftLocal z t f
  where
    liftLocal
      :: ( Monad m )
      => Local r m (f a)
      -> Local r (t m) a
