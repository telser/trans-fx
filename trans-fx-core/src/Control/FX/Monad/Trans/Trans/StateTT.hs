{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.FX.Monad.Trans.Trans.StateTT (
    StateTT(..)
  , runStateTT
  , Context(..)
  , InputTT(..)
  , OutputTT(..)
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
  ( Monad m, MonadTrans t, MonadIdentity mark, Eq s
  , EqIn (t m)
  ) => EqIn (StateTT mark s t m)
  where
    newtype Context (StateTT mark s t m)
      = StateTTCtx
          { unStateTTCtx :: (mark s, Context (t m))
          } deriving (Typeable)

    eqIn
      :: (Eq a)
      => Context (StateTT mark s t m)
      -> StateTT mark s t m a
      -> StateTT mark s t m a
      -> Bool
    eqIn (StateTTCtx (v,h)) x y =
      eqIn h
        (fmap unStateTTOut $ runTT (StateTTIn v) x)
        (fmap unStateTTOut $ runTT (StateTTIn v) y)

deriving instance
  ( Eq (mark s), Eq (Context (t m))
  ) => Eq (Context (StateTT mark s t m))

deriving instance
  ( Show (mark s), Show (Context (t m))
  ) => Show (Context (StateTT mark s t m))



instance
  ( MonadIdentity mark
  ) => RunMonadTransTrans (StateTT mark s)
  where
    newtype InputTT (StateTT mark s) m
      = StateTTIn
          { unStateTTIn :: mark s
          } deriving (Typeable)

    newtype OutputTT (StateTT mark s) a
      = StateTTOut
          { unStateTTOut :: Pair (mark s) a
          } deriving (Typeable)

    runTT
      :: ( Monad m, MonadTrans t )
      => InputTT (StateTT mark s) m
      -> StateTT mark s t m a
      -> t m (OutputTT (StateTT mark s) a)
    runTT (StateTTIn s) =
      fmap (StateTTOut . unStateTOut)
        . runT (StateTIn s) . unStateTT

deriving instance
  ( Eq (mark s)
  ) => Eq (InputTT (StateTT mark s) m)

deriving instance
  ( Show (mark s)
  ) => Show (InputTT (StateTT mark s) m)

deriving instance
  ( Eq (mark s), Eq a
  ) => Eq (OutputTT (StateTT mark s) a)

deriving instance
  ( Show (mark s), Show a
  ) => Show (OutputTT (StateTT mark s) a)

runStateTT
  :: ( MonadIdentity mark, Monad m, MonadTrans t )
  => mark s
  -> StateTT mark s t m a
  -> t m (Pair (mark s) a)
runStateTT s = fmap unStateTTOut . runTT (StateTTIn s)





{- Specialized Lifts -}

instance
  ( MonadIdentity mark
  ) => LiftCatchT (StateTT mark s)
  where
    liftCatchT
      :: forall m t e
       . ( Monad m, MonadTrans t )
      => (forall x. Catch e (t m) (OutputTT (StateTT mark s) x))
      -> (forall x. Catch e (StateTT mark s t m) x)
    liftCatchT catch x h =
      let
        catch' :: Catch e (t m) (OutputT (StateT mark s) x)
        catch' y g =
          fmap (StateTOut . unStateTTOut) $ catch
            (fmap (StateTTOut . unStateTOut) y)
            (fmap (StateTTOut . unStateTOut) . g)
      in
        StateTT $
          liftCatch catch' (unStateTT x) (unStateTT . h)

instance
  ( MonadIdentity mark
  ) => LiftDraftT (StateTT mark s)
  where
    liftDraftT
      :: forall m t w
       . ( Monad m, MonadTrans t, Monoid w )
      => (forall x. Draft w (t m) (OutputTT (StateTT mark s) x))
      -> (forall x. Draft w (StateTT mark s t m) x)
    liftDraftT draft =
      let
        draft' :: Draft w (t m) (OutputT (StateT mark s) x)
        draft' =
          fmap (fmap (StateTOut . unStateTTOut))
            . draft . fmap (StateTTOut . unStateTOut)
      in
        StateTT . liftDraft draft' . unStateTT

instance
  ( MonadIdentity mark
  ) => LiftLocalT (StateTT mark s)
  where
    liftLocalT
      :: forall m t r
       . ( Monad m, MonadTrans t )
      => (forall x. Local r (t m) (OutputTT (StateTT mark s) x))
      -> (forall x. Local r (StateTT mark s t m) x)
    liftLocalT local f =
      let
        local' :: Local r (t m) (OutputT (StateT mark s) x)
        local' g =
          fmap (StateTOut . unStateTTOut)
            . local g . fmap (StateTTOut . unStateTOut)
      in
        StateTT . liftLocal local' f . unStateTT





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
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1, Monoid w
  , forall x. (Monad x) => MonadAppendOnly mark w (t x)
  ) => MonadAppendOnly mark w (StateTT mark1 s t m)
  where
    jot
      :: mark w
      -> StateTT mark1 s t m ()
    jot = StateTT . lift . jot

    look
      :: StateTT mark1 s t m (mark w)
    look = StateTT $ lift look

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadWriteOnce mark w (t x)
  ) => MonadWriteOnce mark w (StateTT mark1 s t m)
  where
    etch
      :: mark w
      -> StateTT mark1 s t m Bool
    etch = StateTT . lift . etch

    press
      :: StateTT mark1 s t m (Maybe (mark w))
    press = StateTT $ lift press

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
