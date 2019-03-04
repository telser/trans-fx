{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.FX.Monad.Trans.Trans.WriteOnlyTT (
    WriteOnlyTT(..)
  , runWriteOnlyTT
) where



import Data.Typeable (Typeable)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans
import Control.FX.Monad.Trans.Trans.Class



newtype WriteOnlyTT
  (mark :: * -> *)
  (w :: *)
  (t :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = WriteOnlyTT
        { unWriteOnlyTT :: WriteOnlyT mark w (t m) a
        } deriving
          ( Typeable, Functor, Applicative, Monad )

deriving instance
  ( Show (t m (WriteOnly mark w a))
  ) => Show (WriteOnlyTT mark w t m a)

instance
  ( MonadTrans t, MonadIdentity mark, Monoid w
  ) => MonadTrans (WriteOnlyTT mark w t)
  where
    lift
      :: ( Monad m )
      => m a
      -> WriteOnlyTT mark w t m a
    lift = WriteOnlyTT . lift . lift

instance
  ( MonadFunctor t, MonadIdentity mark, Monoid w
  ) => MonadFunctor (WriteOnlyTT mark w t)
  where
    hoist
      :: ( Monad m, Monad n )
      => (forall u. m u -> n u)
      -> WriteOnlyTT mark w t m a
      -> WriteOnlyTT mark w t n a
    hoist f = WriteOnlyTT . hoist (hoist f) . unWriteOnlyTT

instance
  ( MonadIdentity mark, Monoid w
  ) => MonadTransTrans (WriteOnlyTT mark w)
  where
    liftT
      :: ( Monad m, MonadTrans t )
      => t m a
      -> WriteOnlyTT mark w t m a
    liftT = WriteOnlyTT . lift

instance
  ( MonadIdentity mark, Monoid w
  ) => RunMonadTransTrans (Val (mark ())) (WriteOnlyTT mark w) (Pair (mark w))
  where
    runTT
      :: ( Monad m, MonadTrans t )
      => Val (mark ()) m
      -> WriteOnlyTT mark w t m a
      -> t m (Pair (mark w) a)
    runTT (Val w) = runT w . unWriteOnlyTT

runWriteOnlyTT
  :: ( MonadIdentity mark, Monad m, MonadTrans t, Monoid w )
  => mark ()
  -> WriteOnlyTT mark w t m a
  -> t m (Pair (mark w) a)
runWriteOnlyTT w = runTT (Val w)

type instance Context (WriteOnlyTT mark w t m)
  = (Val (mark ()) m, Context (t m))

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, Eq w
  , EqIn (t m), Monoid w
  ) => EqIn (WriteOnlyTT mark w t m)
  where
    eqIn
      :: (Eq a)
      => (Val (mark ()) m, Context (t m))
      -> WriteOnlyTT mark w t m a
      -> WriteOnlyTT mark w t m a
      -> Bool
    eqIn (v, h) x y =
      eqIn h (runTT v x) (runTT v y)





{- Specialized Lifts -}

instance
  ( MonadIdentity mark, Monoid w
  ) => LiftCatchT (Val (mark ())) (WriteOnlyTT mark w) (Pair (mark w))
  where
    liftCatchT
      :: ( Monad m, MonadTrans t )
      => (forall x. Catch e (t m) (Pair (mark w) x))
      -> (forall x. Catch e (WriteOnlyTT mark w t m) x)
    liftCatchT catch x h = WriteOnlyTT $
      liftCatch catch (unWriteOnlyTT x) (unWriteOnlyTT . h)

instance
  ( MonadIdentity mark, Monoid w
  ) => LiftDraftT (Val (mark ())) (WriteOnlyTT mark w) (Pair (mark w))
  where
    liftDraftT
      :: ( Monad m, MonadTrans t, Monoid w, Monoid w2 )
      => (forall x. Draft w2 (t m) (Pair (mark w) x))
      -> (forall x. Draft w2 (WriteOnlyTT mark w t m) x)
    liftDraftT draft = WriteOnlyTT . liftDraft draft . unWriteOnlyTT

instance
  ( MonadIdentity mark, Monoid w
  ) => LiftLocalT (Val (mark ())) (WriteOnlyTT mark w) (Pair (mark w))
  where
    liftLocalT
      :: ( Monad m, MonadTrans t )
      => (forall x. Local r (t m) (Pair (mark w) x))
      -> (forall x. Local r (WriteOnlyTT mark w t m) x)
    liftLocalT local f =
      WriteOnlyTT . liftLocal local f . unWriteOnlyTT





{- Effect Classes -}

instance {-# OVERLAPPING #-}
  ( Monad m, MonadTrans t, MonadIdentity mark, Monoid w
  ) => MonadWriteOnly mark w (WriteOnlyTT mark w t m)
  where
    tell
      :: mark w
      -> WriteOnlyTT mark w t m ()
    tell = WriteOnlyTT . tell

    draft
      :: WriteOnlyTT mark w t m a
      -> WriteOnlyTT mark w t m (Pair (mark w) a)
    draft = WriteOnlyTT . draft . unWriteOnlyTT

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1, Monoid w, Monoid w1
  , forall x. (Monad x) => MonadWriteOnly mark w (t x)
  ) => MonadWriteOnly mark w (WriteOnlyTT mark1 w1 t m)
  where
    tell
      :: mark w
      -> WriteOnlyTT mark1 w1 t m ()
    tell = WriteOnlyTT . lift . tell

    draft
      :: WriteOnlyTT mark1 w1 t m a
      -> WriteOnlyTT mark1 w1 t m (Pair (mark w) a)
    draft = liftDraftT draft

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1, Monoid w, Monoid w1
  , forall x. (Monad x) => MonadAppendOnly mark w (t x)
  ) => MonadAppendOnly mark w (WriteOnlyTT mark1 w1 t m)
  where
    jot
      :: mark w
      -> WriteOnlyTT mark1 w1 t m ()
    jot = WriteOnlyTT . lift . jot

    look
      :: WriteOnlyTT mark1 w1 t m (mark w)
    look = WriteOnlyTT $ lift look

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1, Monoid w
  , forall x. (Monad x) => MonadState mark s (t x)
  ) => MonadState mark s (WriteOnlyTT mark1 w t m)
  where
    get
      :: WriteOnlyTT mark1 w t m (mark s)
    get = WriteOnlyTT $ lift get

    put
      :: mark s
      -> WriteOnlyTT mark1 w t m ()
    put = WriteOnlyTT . lift . put

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1, Monoid w
  , forall x. (Monad x) => MonadExcept mark e (t x)
  ) => MonadExcept mark e (WriteOnlyTT mark1 w t m)
  where
    throw
      :: mark e
      -> WriteOnlyTT mark1 w t m a
    throw = WriteOnlyTT . lift . throw

    catch
      :: WriteOnlyTT mark1 w t m a
      -> (mark e -> WriteOnlyTT mark1 w t m a)
      -> WriteOnlyTT mark1 w t m a
    catch = liftCatchT catch

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1, Monoid w
  , forall x. (Monad x) => MonadReadOnly mark r (t x)
  ) => MonadReadOnly mark r (WriteOnlyTT mark1 w t m)
  where
    ask
      :: WriteOnlyTT mark1 w t m (mark r)
    ask = WriteOnlyTT $ lift ask

    local
      :: (mark r -> mark r)
      -> WriteOnlyTT mark1 w t m a
      -> WriteOnlyTT mark1 w t m a
    local = liftLocalT local

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1, Monoid w
  , forall x. (Monad x) => MonadPrompt mark p (t x)
  ) => MonadPrompt mark p (WriteOnlyTT mark1 w t m)
  where
    prompt
      :: mark (p a)
      -> WriteOnlyTT mark1 w t m (mark a)
    prompt = WriteOnlyTT . lift . prompt

instance
  ( Monad m, MonadTrans t, MonadIdentity mark1, Monoid w, MonadIdentity mark
  , forall x. (Monad x) => MonadHalt mark (t x)
  ) => MonadHalt mark (WriteOnlyTT mark1 w t m)
  where
    halt
      :: mark ()
      -> WriteOnlyTT mark1 w t m a
    halt = WriteOnlyTT . lift . halt
