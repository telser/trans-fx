{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.FX.Monad.Trans.Trans.ExceptTT (
    ExceptTT(..)
  , runExceptTT
) where



import Data.Typeable (Typeable)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans
import Control.FX.Monad.Trans.Trans.Class



newtype ExceptTT
  (mark :: * -> *)
  (e :: *)
  (t :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = ExceptTT
        { unExceptTT :: ExceptT mark e (t m) a
        } deriving
          ( Typeable, Functor, Applicative, Monad )

instance
  ( MonadTrans t, MonadIdentity mark
  ) => MonadTrans (ExceptTT mark e t)
  where
    lift
      :: ( Monad m )
      => m a
      -> ExceptTT mark e t m a
    lift = ExceptTT . lift . lift

instance
  ( MonadFunctor t, MonadIdentity mark
  ) => MonadFunctor (ExceptTT mark e t)
  where
    hoist
      :: ( Monad m, Monad n )
      => (forall u. m u -> n u)
      -> ExceptTT mark e t m a
      -> ExceptTT mark e t n a
    hoist f = ExceptTT . hoist (hoist f) . unExceptTT

instance
  ( MonadIdentity mark
  ) => MonadTransTrans (ExceptTT mark e)
  where
    liftT
      :: ( Monad m, MonadTrans t )
      => t m a
      -> ExceptTT mark e t m a
    liftT = ExceptTT . lift

instance
  ( MonadIdentity mark
  ) => RunMonadTransTrans (Val (mark ())) (ExceptTT mark e) (Except mark e)
  where
    runTT
      :: ( Monad m, MonadTrans t )
      => Val (mark ()) m
      -> ExceptTT mark e t m a
      -> t m (Except mark e a)
    runTT (Val s) = runT s . unExceptTT

runExceptTT
  :: ( MonadIdentity mark, Monad m, MonadTrans t )
  => mark ()
  -> ExceptTT mark e t m a
  -> t m (Except mark e a)
runExceptTT s = runTT (Val s)

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, Eq a, Eq e
  , forall x. (Eq x) => EqIn h (t m x)
  ) => EqIn (Val (mark ()) m, h) (ExceptTT mark e t m a)
  where
    eqIn
      :: (Val (mark ()) m, h)
      -> ExceptTT mark e t m a
      -> ExceptTT mark e t m a
      -> Bool
    eqIn (v, h) x y =
      eqIn h (runTT v x) (runTT v y)





{- Specialized Lifts -}

instance
  ( MonadIdentity mark
  ) => LiftCatchT (Val (mark ())) (ExceptTT mark e) (Except mark e)
  where
    liftCatchT
      :: ( Monad m, MonadTrans t )
      => (forall x. Catch e1 (t m) (Except mark e x))
      -> (forall x. Catch e1 (ExceptTT mark e t m) x)
    liftCatchT catch x h = ExceptTT $
      liftCatch catch (unExceptTT x) (unExceptTT . h)

instance
  ( MonadIdentity mark
  ) => LiftDraftT (Val (mark ())) (ExceptTT mark e) (Except mark e)
  where
    liftDraftT
      :: ( Monad m, MonadTrans t, Monoid w )
      => (forall x. Draft w (t m) (Except mark e x))
      -> (forall x. Draft w (ExceptTT mark e t m) x)
    liftDraftT draft = ExceptTT . liftDraft draft . unExceptTT

instance
  ( MonadIdentity mark
  ) => LiftLocalT (Val (mark ())) (ExceptTT mark e) (Except mark e)
  where
    liftLocalT
      :: ( Monad m, MonadTrans t )
      => (forall x. Local r (t m) (Except mark e x))
      -> (forall x. Local r (ExceptTT mark e t m) x)
    liftLocalT local f =
      ExceptTT . liftLocal local f . unExceptTT





{- Effect Classes -}

instance {-# OVERLAPPING #-}
  ( Monad m, MonadTrans t, MonadIdentity mark
  ) => MonadExcept mark e (ExceptTT mark e t m)
  where
    throw
      :: mark e
      -> ExceptTT mark e t m a
    throw = ExceptTT . throw

    catch
      :: ExceptTT mark e t m a
      -> (mark e -> ExceptTT mark e t m a)
      -> ExceptTT mark e t m a
    catch x h = ExceptTT $ catch (unExceptTT x) (unExceptTT . h)

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadExcept mark e (t x)
  ) => MonadExcept mark e (ExceptTT mark1 e1 t m)
  where
    throw
      :: mark e
      -> ExceptTT mark1 e1 t m a
    throw = ExceptTT . lift . throw

    catch
      :: ExceptTT mark1 e1 t m a
      -> (mark e -> ExceptTT mark1 e1 t m a)
      -> ExceptTT mark1 e1 t m a
    catch = liftCatchT catch

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadState mark s (t x)
  ) => MonadState mark s (ExceptTT mark1 e1 t m)
  where
    get
      :: ExceptTT mark1 e1 t m (mark s)
    get = ExceptTT $ lift get

    put
      :: mark s
      -> ExceptTT mark1 e1 t m ()
    put = ExceptTT . lift . put

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1, Monoid w
  , forall x. (Monad x) => MonadWriteOnly mark w (t x)
  ) => MonadWriteOnly mark w (ExceptTT mark1 e t m)
  where
    tell
      :: mark w
      -> ExceptTT mark1 e t m ()
    tell = ExceptTT . lift . tell

    draft
      :: ExceptTT mark1 e t m a
      -> ExceptTT mark1 e t m (Pair (mark w) a)
    draft = liftDraftT draft

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1, Monoid w
  , forall x. (Monad x) => MonadAppendOnly mark w (t x)
  ) => MonadAppendOnly mark w (ExceptTT mark1 e t m)
  where
    jot
      :: mark w
      -> ExceptTT mark1 e t m ()
    jot = ExceptTT . lift . jot

    look
      :: ExceptTT mark1 e t m (mark w)
    look = ExceptTT $ lift look

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadReadOnly mark r (t x)
  ) => MonadReadOnly mark r (ExceptTT mark1 e t m)
  where
    ask
      :: ExceptTT mark1 e t m (mark r)
    ask = ExceptTT $ lift ask

    local
      :: (mark r -> mark r)
      -> ExceptTT mark1 e t m a
      -> ExceptTT mark1 e t m a
    local = liftLocalT local

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadPrompt mark p (t x)
  ) => MonadPrompt mark p (ExceptTT mark1 e t m)
  where
    prompt
      :: mark (p a)
      -> ExceptTT mark1 e t m (mark a)
    prompt = ExceptTT . lift . prompt

instance
  ( Monad m, MonadTrans t, MonadIdentity mark1, MonadIdentity mark
  , forall x. (Monad x) => MonadHalt mark (t x)
  ) => MonadHalt mark (ExceptTT mark1 e t m)
  where
    halt
      :: mark ()
      -> ExceptTT mark1 e t m a
    halt = ExceptTT . lift . halt
