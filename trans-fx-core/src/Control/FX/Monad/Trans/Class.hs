{-#
  LANGUAGE
    Rank2Types,
    KindSignatures,
    QuantifiedConstraints,
    MultiParamTypeClasses,
    FunctionalDependencies
#-}

module Control.FX.Monad.Trans.Class (
    MonadTrans(lift)
  , MonadFunctor(hoist)
  , RunMonadTrans(runT)

  , Catch, LiftCatch(..)
  , Draft, LiftDraft(..)
  , Local, LiftLocal(..)
) where

import Control.FX.Functor
import Control.FX.Monad

class
  ( forall m. (Monad m) => Monad (t m)
  ) => MonadTrans
    (t :: (* -> *) -> * -> *)
  where
    lift
      :: (Monad m)
      => m a -> t m a

instance MonadTrans Apply where
  lift = Apply

instance
  ( Monad m, Central m
  ) => MonadTrans (Flip m)
  where
    lift = Flip . commute . return

class
  ( MonadTrans t
  ) => MonadFunctor t
  where
    hoist
      :: (Monad m, Monad n)
      => (forall u. m u -> n u) -> t m a -> t n a

class
  ( MonadTrans t, Commutant f
  ) => RunMonadTrans z t f | t -> z f
  where
    runT
      :: (Monad m)
      => z -> t m a -> m (f a)



{- Specialized Lifts -}

type Catch e m a = m a -> (e -> m a) -> m a

class
  ( MonadTrans t, RunMonadTrans z t f
  ) => LiftCatch z t f
  where
    liftCatch
      :: (Monad m)
      => Catch e m (f a) -> Catch e (t m) a



type Draft w m a = m a -> m (Pair w a)

class
  ( MonadTrans t, RunMonadTrans z t f
  ) => LiftDraft z t f
  where
    liftDraft
      :: (Monad m)
      => Draft w m (f a) -> Draft w (t m) a



type Local r m a = (r -> r) -> m a -> m a

class
  ( MonadTrans t, RunMonadTrans z t f
  ) => LiftLocal z t f
  where
    liftLocal
      :: (Monad m)
      => Local r m (f a) -> Local r (t m) a
