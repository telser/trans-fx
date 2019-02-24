{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.FX.Monad.Trans.Trans.StateTT (
    StateTT(..)
  , runStateTT
) where



import Data.Typeable (Typeable)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans
import Control.FX.Monad.Trans.Trans.Class



newtype StateTT
  (mark :: * -> *)
  (s :: *)
  (t :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = StateTT
        { unStateTT :: StateT mark s (t m) a
        } deriving
          ( Show, Typeable, Functor, Applicative, Monad )

instance
  ( MonadTrans t, MonadIdentity mark
  ) => MonadTrans (StateTT mark s t)
  where
    lift
      :: ( Monad m )
      => m a
      -> StateTT mark s t m a
    lift = StateTT . lift . lift

instance
  ( MonadFunctor t, MonadIdentity mark
  ) => MonadFunctor (StateTT mark s t)
  where
    hoist
      :: ( Monad m, Monad n )
      => (forall u. m u -> n u)
      -> StateTT mark s t m a
      -> StateTT mark s t n a
    hoist f = StateTT . hoist (hoist f) . unStateTT

instance
  ( MonadIdentity mark
  ) => MonadTransTrans (StateTT mark s)
  where
    liftT
      :: ( Monad m, MonadTrans t )
      => t m a
      -> StateTT mark s t m a
    liftT = StateTT . lift

instance
  ( MonadIdentity mark
  ) => RunMonadTransTrans (Val (mark s)) (StateTT mark s) (Pair (mark s))
  where
    runTT
      :: ( Monad m, MonadTrans t )
      => Val (mark s) m
      -> StateTT mark s t m a
      -> t m (Pair (mark s) a)
    runTT (Val s) = runT s . unStateTT

runStateTT
  :: ( MonadIdentity mark, Monad m, MonadTrans t )
  => mark s
  -> StateTT mark s t m a
  -> t m (Pair (mark s) a)
runStateTT s = runTT (Val s)

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, Eq a, Eq s
  , forall x. (Eq x) => EqIn h (t m x)
  ) => EqIn (Val (mark s) m, h) (StateTT mark s t m a)
  where
    eqIn
      :: (Val (mark s) m, h)
      -> StateTT mark s t m a
      -> StateTT mark s t m a
      -> Bool
    eqIn (v, h) x y =
      eqIn h (runTT v x) (runTT v y)





{- Specialized Lifts -}

instance
  ( MonadIdentity mark
  ) => LiftCatchT (Val (mark s)) (StateTT mark s) (Pair (mark s))
  where
    liftCatchT
      :: ( Monad m, MonadTrans t )
      => (forall x. Catch e (t m) (Pair (mark s) x))
      -> (forall x. Catch e (StateTT mark s t m) x)
    liftCatchT catch x h = StateTT $
      liftCatch catch (unStateTT x) (unStateTT . h)

instance
  ( MonadIdentity mark
  ) => LiftDraftT (Val (mark s)) (StateTT mark s) (Pair (mark s))
  where
    liftDraftT
      :: ( Monad m, MonadTrans t, Monoid w )
      => (forall x. Draft w (t m) (Pair (mark s) x))
      -> (forall x. Draft w (StateTT mark s t m) x)
    liftDraftT draft = StateTT . liftDraft draft . unStateTT

instance
  ( MonadIdentity mark
  ) => LiftLocalT (Val (mark s)) (StateTT mark s) (Pair (mark s))
  where
    liftLocalT
      :: ( Monad m, MonadTrans t )
      => (forall x. Local r (t m) (Pair (mark s) x))
      -> (forall x. Local r (StateTT mark s t m) x)
    liftLocalT local f =
      StateTT . liftLocal local f . unStateTT





{- Effect Classes -}

instance {-# OVERLAPPING #-}
  ( Monad m, MonadTrans t, MonadIdentity mark
  ) => MonadState mark s (StateTT mark s t m)
  where
    get
      :: StateTT mark s t m (mark s)
    get = StateTT get

    put
      :: mark s
      -> StateTT mark s t m ()
    put = StateTT . put

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadState mark s (t x)
  ) => MonadState mark s (StateTT mark1 s1 t m)
  where
    get
      :: StateTT mark1 s1 t m (mark s)
    get = StateTT $ lift get

    put
      :: mark s
      -> StateTT mark1 s1 t m ()
    put = StateTT . lift . put

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadExcept mark e (t x)
  ) => MonadExcept mark e (StateTT mark1 s t m)
  where
    throw
      :: mark e
      -> StateTT mark1 s t m a
    throw = StateTT . lift . throw

    catch
      :: StateTT mark1 s t m a
      -> (mark e -> StateTT mark1 s t m a)
      -> StateTT mark1 s t m a
    catch = liftCatchT catch

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1, Monoid w
  , forall x. (Monad x) => MonadWriteOnly mark w (t x)
  ) => MonadWriteOnly mark w (StateTT mark1 s t m)
  where
    tell
      :: mark w
      -> StateTT mark1 s t m ()
    tell = StateTT . lift . tell

    draft
      :: StateTT mark1 s t m a
      -> StateTT mark1 s t m (Pair (mark w) a)
    draft = liftDraftT draft

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadReadOnly mark r (t x)
  ) => MonadReadOnly mark r (StateTT mark1 s t m)
  where
    ask
      :: StateTT mark1 s t m (mark r)
    ask = StateTT $ lift ask

    local
      :: (mark r -> mark r)
      -> StateTT mark1 s t m a
      -> StateTT mark1 s t m a
    local = liftLocalT local

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadPrompt mark p (t x)
  ) => MonadPrompt mark p (StateTT mark1 s t m)
  where
    prompt
      :: mark (p a)
      -> StateTT mark1 s t m (mark a)
    prompt = StateTT . lift . prompt

instance
  ( Monad m, MonadTrans t, MonadIdentity mark1, MonadIdentity mark
  , forall x. (Monad x) => MonadHalt mark (t x)
  ) => MonadHalt mark (StateTT mark1 s t m)
  where
    halt
      :: mark ()
      -> StateTT mark1 s t m a
    halt = StateTT . lift . halt
