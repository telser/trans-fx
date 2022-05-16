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
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.FX.Monad.Trans.Trans.HaltTT (
    HaltTT(..)
  , runHaltTT
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



newtype HaltTT
  (mark :: * -> *)
  (t :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = HaltTT
        { unHaltTT :: HaltT mark (t m) a
        } deriving
          ( Typeable, Functor, Applicative, Monad )

deriving instance
  ( Show (t m (Halt mark a))
  ) => Show (HaltTT mark t m a)

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
  ( Monad m, MonadTrans t, MonadIdentity mark
  , EqIn (t m)
  ) => EqIn (HaltTT mark t m)
  where
    newtype Context (HaltTT mark t m)
      = HaltTTCtx
          { unHaltTTCtx :: (mark (), Context (t m))
          } deriving (Typeable)

    eqIn
      :: (Eq a)
      => Context (HaltTT mark t m)
      -> HaltTT mark t m a
      -> HaltTT mark t m a
      -> Bool
    eqIn (HaltTTCtx (v,h)) x y =
      eqIn h
        (fmap unHaltTTOut $ runTT (HaltTTIn v) x)
        (fmap unHaltTTOut $ runTT (HaltTTIn v) y)

deriving instance
  ( Eq (mark ()), Eq (Context (t m))
  ) => Eq (Context (HaltTT mark t m))

deriving instance
  ( Show (mark ()), Show (Context (t m))
  ) => Show (Context (HaltTT mark t m))



instance
  ( MonadIdentity mark
  ) => RunMonadTransTrans (HaltTT mark)
  where
    newtype InputTT (HaltTT mark) m
      = HaltTTIn
          { unHaltTTIn :: mark ()
          } deriving (Typeable)

    newtype OutputTT (HaltTT mark) a
      = HaltTTOut
          { unHaltTTOut :: Halt mark a
          } deriving (Typeable)

    runTT
      :: ( Monad m, MonadTrans t )
      => InputTT (HaltTT mark) m
      -> HaltTT mark t m a
      -> t m (OutputTT (HaltTT mark) a)
    runTT (HaltTTIn x) =
      fmap (HaltTTOut . unHaltTOut)
        . runT (HaltTIn x) . unHaltTT

deriving instance
  ( Eq (mark ())
  ) => Eq (InputTT (HaltTT mark) m)

deriving instance
  ( Show (mark ())
  ) => Show (InputTT (HaltTT mark) m)

deriving instance
  ( Eq (mark ()), Eq a
  ) => Eq (OutputTT (HaltTT mark) a)

deriving instance
  ( Show (mark ()), Show a
  ) => Show (OutputTT (HaltTT mark) a)

runHaltTT
  :: ( Monad m, MonadTrans t, MonadIdentity mark )
  => HaltTT mark t m a
  -> t m (Halt mark a)
runHaltTT = fmap unHaltTTOut . runTT (HaltTTIn (pure ()))





{- Specialized Lifts -}

instance
  ( MonadIdentity mark
  ) => LiftCatchT (HaltTT mark)
  where
    liftCatchT
      :: forall m t e
       . ( Monad m, MonadTrans t )
      => (forall x. Catch e (t m) (OutputTT (HaltTT mark) x))
      -> (forall x. Catch e (HaltTT mark t m) x)
    liftCatchT catch x h =
      let
        catch' :: Catch e (t m) (OutputT (HaltT mark) x)
        catch' y g =
          fmap (HaltTOut . unHaltTTOut) $ catch
            (fmap (HaltTTOut . unHaltTOut) y)
            (fmap (HaltTTOut . unHaltTOut) . g)
      in
        HaltTT $ liftCatch catch' (unHaltTT x) (unHaltTT . h)

instance
  ( MonadIdentity mark
  ) => LiftDraftT (HaltTT mark)
  where
    liftDraftT
      :: forall m t w
       . ( Monad m, MonadTrans t, Monoid w )
      => (forall x. Draft w (t m) (OutputTT (HaltTT mark) x))
      -> (forall x. Draft w (HaltTT mark t m) x)
    liftDraftT draft =
      let
        draft' :: Draft w (t m) (OutputT (HaltT mark) x)
        draft' =
          fmap (fmap (HaltTOut . unHaltTTOut))
            . draft . fmap (HaltTTOut . unHaltTOut)
      in
        HaltTT . liftDraft draft' . unHaltTT

instance
  ( MonadIdentity mark
  ) => LiftLocalT (HaltTT mark)
  where
    liftLocalT
      :: forall m t r
       . ( Monad m, MonadTrans t )
      => (forall x. Local r (t m) (OutputTT (HaltTT mark) x))
      -> (forall x. Local r (HaltTT mark t m) x)
    liftLocalT local f =
      let
        local' :: Local r (t m) (OutputT (HaltT mark) x)
        local' g =
          fmap (HaltTOut . unHaltTTOut)
            . local g . fmap (HaltTTOut . unHaltTOut)
      in
        HaltTT . liftLocal local' f . unHaltTT





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
  ( Monad m, MonadTrans t, Monoid w, MonadIdentity mark1
  , MonadAppendOnly mark w (t m)
  ) => MonadAppendOnly mark w (HaltTT mark1 t m)
  where
    jot
      :: mark w
      -> HaltTT mark1 t m ()
    jot = HaltTT . lift . jot

    look
      :: HaltTT mark1 t m (mark w)
    look = HaltTT $ lift look

instance
  ( Monad m, MonadTrans t, MonadIdentity mark1
  , MonadWriteOnce mark w (t m)
  ) => MonadWriteOnce mark w (HaltTT mark1 t m)
  where
    etch
      :: mark w
      -> HaltTT mark1 t m Bool
    etch = HaltTT . lift . etch

    press
      :: HaltTT mark1 t m (Maybe (mark w))
    press = HaltTT $ lift press

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
