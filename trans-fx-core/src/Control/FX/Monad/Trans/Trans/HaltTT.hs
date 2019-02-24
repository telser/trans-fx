{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.FX.Monad.Trans.Trans.HaltTT (
    HaltTT(..)
  , runHaltTT
) where



import Data.Typeable (Typeable)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans
import Control.FX.Monad.Trans.Trans.Class



newtype HaltTT
  (mark :: * -> *)
  (t :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = HaltTT
        { unHaltTT :: HaltT mark (t m) a
        } deriving
          ( Typeable, Functor, Applicative, Monad )

instance
  ( MonadTrans t, MonadIdentity mark
  ) => MonadTrans (HaltTT mark t)
  where
    lift
      :: ( Monad m )
      => m a
      -> HaltTT mark t m a
    lift = HaltTT . lift . lift

instance
  ( MonadFunctor t, MonadIdentity mark
  ) => MonadFunctor (HaltTT mark t)
  where
    hoist
      :: ( Monad m, Monad n )
      => (forall u. m u -> n u)
      -> HaltTT mark t m a
      -> HaltTT mark t n a
    hoist f = HaltTT . hoist (hoist f) . unHaltTT

instance
  ( MonadIdentity mark
  ) => MonadTransTrans (HaltTT mark)
  where
    liftT
      :: ( Monad m, MonadTrans t )
      => t m a
      -> HaltTT mark t m a
    liftT = HaltTT . lift

instance
  ( MonadIdentity mark
  ) => RunMonadTransTrans (Val (mark ())) (HaltTT mark) (Halt mark)
  where
    runTT
      :: ( Monad m, MonadTrans t )
      => Val (mark ()) m
      -> HaltTT mark t m a
      -> t m (Halt mark a)
    runTT (Val x) = runT x . unHaltTT

runHaltTT
  :: ( Monad m, MonadTrans t, MonadIdentity mark )
  => HaltTT mark t m a
  -> t m (Halt mark a)
runHaltTT = runTT (Val (pure ()))

instance
  ( Monad m, MonadTrans t, Eq a, MonadIdentity mark
  , forall x. (Eq x) => EqIn h (t m x)
  ) => EqIn (Val (mark ()) m, h) (HaltTT mark t m a)
  where
    eqIn
      :: (Val (mark ()) m, h)
      -> HaltTT mark t m a
      -> HaltTT mark t m a
      -> Bool
    eqIn (v, h) x y =
      eqIn h (runTT v x) (runTT v y)





{- Specialized Lifts -}

instance
  ( MonadIdentity mark
  ) => LiftCatchT (Val (mark ())) (HaltTT mark) (Halt mark)
  where
    liftCatchT
      :: ( Monad m, MonadTrans t )
      => (forall x. Catch e (t m) (Halt mark x))
      -> (forall x. Catch e (HaltTT mark t m) x)
    liftCatchT catch x h = HaltTT $
      liftCatch catch (unHaltTT x) (unHaltTT . h)

instance
  ( MonadIdentity mark
  ) => LiftDraftT (Val (mark ())) (HaltTT mark) (Halt mark)
  where
    liftDraftT
      :: ( Monad m, MonadTrans t, Monoid w )
      => (forall x. Draft w (t m) (Halt mark x))
      -> (forall x. Draft w (HaltTT mark t m) x)
    liftDraftT draft = HaltTT . liftDraft draft . unHaltTT

instance
  ( MonadIdentity mark
  ) => LiftLocalT (Val (mark ())) (HaltTT mark) (Halt mark)
  where
    liftLocalT
      :: ( Monad m, MonadTrans t )
      => (forall x. Local r (t m) (Halt mark x))
      -> (forall x. Local r (HaltTT mark t m) x)
    liftLocalT local f =
      HaltTT . liftLocal local f . unHaltTT





{- Effect Classes -}

instance {-# OVERLAPPING #-}
  ( Monad m, MonadTrans t, MonadIdentity mark
  ) => MonadHalt mark (HaltTT mark t m)
  where
    halt
      :: mark ()
      -> HaltTT mark t m a
    halt = HaltTT . halt

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadHalt mark (t x)
  ) => MonadHalt mark (HaltTT mark1 t m)
  where
    halt
      :: mark ()
      -> HaltTT mark1 t m a
    halt = HaltTT . lift . halt

instance
  ( Monad m, MonadTrans t, MonadIdentity mark1
  , MonadState mark s (t m)
  ) => MonadState mark s (HaltTT mark1 t m)
  where
    get
      :: HaltTT mark1 t m (mark s)
    get = HaltTT $ lift get

    put
      :: mark s
      -> HaltTT mark1 t m ()
    put = HaltTT . lift . put

instance
  ( Monad m, MonadTrans t, MonadIdentity mark1
  , MonadExcept mark e (t m)
  ) => MonadExcept mark e (HaltTT mark1 t m)
  where
    throw
      :: mark e
      -> HaltTT mark1 t m a
    throw = HaltTT . lift . throw

    catch
      :: HaltTT mark1 t m a
      -> (mark e -> HaltTT mark1 t m a)
      -> HaltTT mark1 t m a
    catch = liftCatchT catch

instance
  ( Monad m, MonadTrans t, Monoid w, MonadIdentity mark1
  , MonadWriteOnly mark w (t m)
  ) => MonadWriteOnly mark w (HaltTT mark1 t m)
  where
    tell
      :: mark w
      -> HaltTT mark1 t m ()
    tell = HaltTT . lift . tell

    draft
      :: HaltTT mark1 t m a
      -> HaltTT mark1 t m (Pair (mark w) a)
    draft = liftDraftT draft

instance
  ( Monad m, MonadTrans t, MonadIdentity mark1
  , MonadReadOnly mark r (t m)
  ) => MonadReadOnly mark r (HaltTT mark1 t m)
  where
    ask
      :: HaltTT mark1 t m (mark r)
    ask = HaltTT $ lift ask

    local
      :: (mark r -> mark r)
      -> HaltTT mark1 t m a
      -> HaltTT mark1 t m a
    local = liftLocalT local

instance
  ( Monad m, MonadTrans t, MonadIdentity mark1
  , MonadPrompt mark p (t m)
  ) => MonadPrompt mark p (HaltTT mark1 t m)
  where
    prompt
      :: mark (p a)
      -> HaltTT mark1 t m (mark a)
    prompt = HaltTT . lift . prompt
