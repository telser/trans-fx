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

module Control.FX.Monad.Trans.Trans.WriteOnceTT (
    WriteOnceTT(..)
  , runWriteOnceTT
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



newtype WriteOnceTT
  (mark :: * -> *)
  (w :: *)
  (t :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = WriteOnceTT
        { unWriteOnceTT :: WriteOnceT mark w (t m) a
        } deriving
          ( Show, Typeable, Functor, Applicative, Monad )

instance
  ( MonadTrans t, MonadIdentity mark
  ) => MonadTrans (WriteOnceTT mark w t)
  where
    lift
      :: ( Monad m )
      => m a
      -> WriteOnceTT mark w t m a
    lift = WriteOnceTT . lift . lift

instance
  ( MonadFunctor t, MonadIdentity mark
  ) => MonadFunctor (WriteOnceTT mark w t)
  where
    hoist
      :: ( Monad m, Monad n )
      => (forall u. m u -> n u)
      -> WriteOnceTT mark w t m a
      -> WriteOnceTT mark w t n a
    hoist f = WriteOnceTT . hoist (hoist f) . unWriteOnceTT

instance
  ( MonadIdentity mark
  ) => MonadTransTrans (WriteOnceTT mark w)
  where
    liftT
      :: ( Monad m, MonadTrans t )
      => t m a
      -> WriteOnceTT mark w t m a
    liftT = WriteOnceTT . lift





instance
  ( Monad m, MonadTrans t, MonadIdentity mark, Eq w
  , EqIn (t m)
  ) => EqIn (WriteOnceTT mark w t m)
  where
    newtype Context (WriteOnceTT mark w t m)
      = WriteOnceTTCtx
          { unWriteOnceTTCtx :: (mark (), Context (t m))
          } deriving (Typeable)

    eqIn
      :: (Eq a)
      => Context (WriteOnceTT mark w t m)
      -> WriteOnceTT mark w t m a
      -> WriteOnceTT mark w t m a
      -> Bool
    eqIn (WriteOnceTTCtx (v,h)) x y =
      eqIn h
        (fmap unWriteOnceTTOut $ runTT (WriteOnceTTIn v) x)
        (fmap unWriteOnceTTOut $ runTT (WriteOnceTTIn v) y)

deriving instance
  ( Eq (mark ()), Eq (Context (t m))
  ) => Eq (Context (WriteOnceTT mark w t m))

deriving instance
  ( Show (mark ()), Show (Context (t m))
  ) => Show (Context (WriteOnceTT mark w t m))



instance
  ( MonadIdentity mark
  ) => RunMonadTransTrans (WriteOnceTT mark w)
  where
    newtype InputTT (WriteOnceTT mark w) m
      = WriteOnceTTIn
          { unWriteOnceTTIn :: mark ()
          } deriving (Typeable)

    newtype OutputTT (WriteOnceTT mark w) a
      = WriteOnceTTOut
          { unWriteOnceTTOut :: Pair (mark (Maybe w)) a
          } deriving (Typeable)

    runTT
      :: ( Monad m, MonadTrans t )
      => InputTT (WriteOnceTT mark w) m
      -> WriteOnceTT mark w t m a
      -> t m (OutputTT (WriteOnceTT mark w) a)
    runTT (WriteOnceTTIn w) =
      fmap (WriteOnceTTOut . unWriteOnceTOut)
        . runT (WriteOnceTIn w) . unWriteOnceTT

deriving instance
  ( Eq (mark ())
  ) => Eq (InputTT (WriteOnceTT mark w) m)

deriving instance
  ( Show (mark ())
  ) => Show (InputTT (WriteOnceTT mark w) m)

deriving instance
  ( Eq (mark (Maybe w)), Eq a
  ) => Eq (OutputTT (WriteOnceTT mark w) a)

deriving instance
  ( Show (mark (Maybe w)), Show a
  ) => Show (OutputTT (WriteOnceTT mark w) a)

runWriteOnceTT
  :: ( MonadIdentity mark, Monad m, MonadTrans t )
  => mark ()
  -> WriteOnceTT mark w t m a
  -> t m (Pair (mark (Maybe w)) a)
runWriteOnceTT w = fmap unWriteOnceTTOut . runTT (WriteOnceTTIn w)











{- Specialized Lifts -}

instance
  ( MonadIdentity mark
  ) => LiftCatchT (WriteOnceTT mark w)
  where
    liftCatchT
      :: forall m t e
       . ( Monad m, MonadTrans t )
      => (forall x. Catch e (t m) (OutputTT (WriteOnceTT mark w) x))
      -> (forall x. Catch e (WriteOnceTT mark w t m) x)
    liftCatchT catch x h =
      let
        catch' :: Catch e (t m) (OutputT (WriteOnceT mark w) x)
        catch' y g =
          fmap (WriteOnceTOut . unWriteOnceTTOut) $ catch
            (fmap (WriteOnceTTOut . unWriteOnceTOut) y)
            (fmap (WriteOnceTTOut . unWriteOnceTOut) . g)
      in
        WriteOnceTT $ liftCatch catch' (unWriteOnceTT x) (unWriteOnceTT . h)

instance
  ( MonadIdentity mark
  ) => LiftDraftT (WriteOnceTT mark w)
  where
    liftDraftT
      :: forall m t w1
       . ( Monad m, MonadTrans t, Monoid w1 )
      => (forall x. Draft w1 (t m) (OutputTT (WriteOnceTT mark w) x))
      -> (forall x. Draft w1 (WriteOnceTT mark w t m) x)
    liftDraftT draft =
      let
        draft' :: Draft w1 (t m) (OutputT (WriteOnceT mark w) x)
        draft' =
          fmap (fmap (WriteOnceTOut . unWriteOnceTTOut))
            . draft . fmap (WriteOnceTTOut . unWriteOnceTOut)
      in
        WriteOnceTT . liftDraft draft' . unWriteOnceTT

instance
  ( MonadIdentity mark
  ) => LiftLocalT (WriteOnceTT mark w)
  where
    liftLocalT
      :: forall m t r
       . ( Monad m, MonadTrans t )
      => (forall x. Local r (t m) (OutputTT (WriteOnceTT mark w) x))
      -> (forall x. Local r (WriteOnceTT mark w t m) x)
    liftLocalT local f =
      let
        local' :: Local r (t m) (OutputT (WriteOnceT mark w) x)
        local' g =
          fmap (WriteOnceTOut . unWriteOnceTTOut)
            . local g . fmap (WriteOnceTTOut . unWriteOnceTOut)
      in
        WriteOnceTT . liftLocal local' f . unWriteOnceTT





{- Effect Classes -}

instance {-# OVERLAPPING #-}
  ( Monad m, MonadTrans t, MonadIdentity mark
  ) => MonadWriteOnce mark w (WriteOnceTT mark w t m)
  where
    press
      :: WriteOnceTT mark w t m (Maybe (mark w))
    press = WriteOnceTT press

    etch
      :: mark w
      -> WriteOnceTT mark w t m Bool
    etch = WriteOnceTT . etch

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadWriteOnce mark w (t x)
  ) => MonadWriteOnce mark w (WriteOnceTT mark1 w1 t m)
  where
    press
      :: WriteOnceTT mark1 w1 t m (Maybe (mark w))
    press = WriteOnceTT $ lift press

    etch
      :: mark w
      -> WriteOnceTT mark1 w1 t m Bool
    etch = WriteOnceTT . lift . etch

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadAppendOnly mark w (t x), Monoid w
  ) => MonadAppendOnly mark w (WriteOnceTT mark1 w1 t m)
  where
    look
      :: WriteOnceTT mark1 w1 t m (mark w)
    look = WriteOnceTT $ lift look

    jot
      :: mark w
      -> WriteOnceTT mark1 w1 t m ()
    jot = WriteOnceTT . lift . jot

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadState mark s (t x)
  ) => MonadState mark s (WriteOnceTT mark1 w t m)
  where
    get
      :: WriteOnceTT mark1 w t m (mark s)
    get = WriteOnceTT $ lift get

    put
      :: mark s
      -> WriteOnceTT mark1 w t m ()
    put = WriteOnceTT . lift . put

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadExcept mark e (t x)
  ) => MonadExcept mark e (WriteOnceTT mark1 w t m)
  where
    throw
      :: mark e
      -> WriteOnceTT mark1 w t m a
    throw = WriteOnceTT . lift . throw

    catch
      :: WriteOnceTT mark1 w t m a
      -> (mark e -> WriteOnceTT mark1 w t m a)
      -> WriteOnceTT mark1 w t m a
    catch = liftCatchT catch

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadWriteOnly mark w (t x), Monoid w
  ) => MonadWriteOnly mark w (WriteOnceTT mark1 w1 t m)
  where
    tell
      :: mark w
      -> WriteOnceTT mark1 w1 t m ()
    tell = WriteOnceTT . lift . tell

    draft
      :: WriteOnceTT mark1 w1 t m a
      -> WriteOnceTT mark1 w1 t m (Pair (mark w) a)
    draft = liftDraftT draft

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadReadOnly mark r (t x)
  ) => MonadReadOnly mark r (WriteOnceTT mark1 w t m)
  where
    ask
      :: WriteOnceTT mark1 w t m (mark r)
    ask = WriteOnceTT $ lift ask

    local
      :: (mark r -> mark r)
      -> WriteOnceTT mark1 w t m a
      -> WriteOnceTT mark1 w t m a
    local = liftLocalT local

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadPrompt mark p (t x)
  ) => MonadPrompt mark p (WriteOnceTT mark1 w t m)
  where
    prompt
      :: mark (p a)
      -> WriteOnceTT mark1 w t m (mark a)
    prompt = WriteOnceTT . lift . prompt

instance
  ( Monad m, MonadTrans t, MonadIdentity mark1, MonadIdentity mark
  , forall x. (Monad x) => MonadHalt mark (t x)
  ) => MonadHalt mark (WriteOnceTT mark1 w t m)
  where
    halt
      :: mark ()
      -> WriteOnceTT mark1 w t m a
    halt = WriteOnceTT . lift . halt
