{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.FX.Monad.Trans.Trans.ReadOnlyTT (
    ReadOnlyTT(..)
  , runReadOnlyTT
) where



import Data.Typeable (Typeable)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans
import Control.FX.Monad.Trans.Trans.Class



newtype ReadOnlyTT
  (mark :: * -> *)
  (r :: *)
  (t :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = ReadOnlyTT
        { unReadOnlyTT :: ReadOnlyT mark r (t m) a
        } deriving
          ( Show, Typeable, Functor, Applicative, Monad )

instance
  ( MonadTrans t, MonadIdentity mark
  ) => MonadTrans (ReadOnlyTT mark r t)
  where
    lift
      :: ( Monad m )
      => m a
      -> ReadOnlyTT mark r t m a
    lift = ReadOnlyTT . lift . lift

instance
  ( MonadFunctor t, MonadIdentity mark
  ) => MonadFunctor (ReadOnlyTT mark r t)
  where
    hoist
      :: ( Monad m, Monad n )
      => (forall u. m u -> n u)
      -> ReadOnlyTT mark r t m a
      -> ReadOnlyTT mark r t n a
    hoist f = ReadOnlyTT . hoist (hoist f) . unReadOnlyTT

instance
  ( MonadIdentity mark
  ) => MonadTransTrans (ReadOnlyTT mark r)
  where
    liftT
      :: ( Monad m, MonadTrans t )
      => t m a
      -> ReadOnlyTT mark r t m a
    liftT = ReadOnlyTT . lift

instance
  ( MonadIdentity mark, Commutant mark
  ) => RunMonadTransTrans (Val (mark r)) (ReadOnlyTT mark r) mark
  where
    runTT
      :: ( Monad m, MonadTrans t )
      => Val (mark r) m
      -> ReadOnlyTT mark r t m a
      -> t m (mark a)
    runTT (Val r) = runT r . unReadOnlyTT

runReadOnlyTT
  :: ( MonadIdentity mark, Commutant mark, Monad m, MonadTrans t )
  => mark r
  -> ReadOnlyTT mark r t m a
  -> t m (mark a)
runReadOnlyTT r = runTT (Val r)

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, Commutant mark, Eq a
  , forall x. (Eq x) => EqIn h (t m x)
  ) => EqIn (Val (mark r) m, h) (ReadOnlyTT mark r t m a)
  where
    eqIn
      :: (Val (mark r) m, h)
      -> ReadOnlyTT mark r t m a
      -> ReadOnlyTT mark r t m a
      -> Bool
    eqIn (v, h) x y =
      eqIn h (runTT v x) (runTT v y)





{- Specialized Lifts -}

instance
  ( MonadIdentity mark, Commutant mark
  ) => LiftCatchT (Val (mark r)) (ReadOnlyTT mark r) mark
  where
    liftCatchT
      :: ( Monad m, MonadTrans t )
      => (forall x. Catch e (t m) (mark x))
      -> (forall x. Catch e (ReadOnlyTT mark r t m) x)
    liftCatchT catch x h = ReadOnlyTT $
      liftCatch catch (unReadOnlyTT x) (unReadOnlyTT . h)

instance
  ( MonadIdentity mark, Commutant mark
  ) => LiftDraftT (Val (mark r)) (ReadOnlyTT mark r) mark
  where
    liftDraftT
      :: ( Monad m, MonadTrans t, Monoid w )
      => (forall x. Draft w (t m) (mark x))
      -> (forall x. Draft w (ReadOnlyTT mark r t m) x)
    liftDraftT draft = ReadOnlyTT . liftDraft draft . unReadOnlyTT

instance
  ( MonadIdentity mark, Commutant mark
  ) => LiftLocalT (Val (mark r)) (ReadOnlyTT mark r) mark
  where
    liftLocalT
      :: ( Monad m, MonadTrans t )
      => (forall x. Local r2 (t m) (mark x))
      -> (forall x. Local r2 (ReadOnlyTT mark r t m) x)
    liftLocalT local f =
      ReadOnlyTT . liftLocal local f . unReadOnlyTT





{- Effect Classes -}

instance {-# OVERLAPPING #-}
  ( Monad m, MonadTrans t, MonadIdentity mark
  ) => MonadReadOnly mark r (ReadOnlyTT mark r t m)
  where
    ask
      :: ReadOnlyTT mark r t m (mark r)
    ask = ReadOnlyTT ask

    local
      :: (mark r -> mark r)
      -> ReadOnlyTT mark r t m a
      -> ReadOnlyTT mark r t m a
    local f = ReadOnlyTT . local f . unReadOnlyTT

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1, Commutant mark1
  , forall x. (Monad x) => MonadReadOnly mark r (t x)
  ) => MonadReadOnly mark r (ReadOnlyTT mark1 r1 t m)
  where
    ask
      :: ReadOnlyTT mark1 r1 t m (mark r)
    ask = ReadOnlyTT $ lift ask

    local
      :: (mark r -> mark r)
      -> ReadOnlyTT mark1 r1 t m a
      -> ReadOnlyTT mark1 r1 t m a
    local = liftLocalT local

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadState mark s (t x)
  ) => MonadState mark s (ReadOnlyTT mark1 r t m)
  where
    get
      :: ReadOnlyTT mark1 r t m (mark s)
    get = ReadOnlyTT $ lift get

    put
      :: mark s
      -> ReadOnlyTT mark1 r t m ()
    put = ReadOnlyTT . lift . put

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1, Commutant mark1
  , forall x. (Monad x) => MonadExcept mark e (t x)
  ) => MonadExcept mark e (ReadOnlyTT mark1 r t m)
  where
    throw
      :: mark e
      -> ReadOnlyTT mark1 r t m a
    throw = ReadOnlyTT . lift . throw

    catch
      :: ReadOnlyTT mark1 r t m a
      -> (mark e -> ReadOnlyTT mark1 r t m a)
      -> ReadOnlyTT mark1 r t m a
    catch = liftCatchT catch

instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadIdentity mark1, Monoid w, Commutant mark1
  , forall x. (Monad x) => MonadAppendOnly mark w (t x)
  ) => MonadAppendOnly mark w (ReadOnlyTT mark1 r t m)
  where
    jot
      :: mark w
      -> ReadOnlyTT mark1 r t m ()
    jot = ReadOnlyTT . lift . jot

    look
      :: ReadOnlyTT mark1 r1 t m (mark w)
    look = ReadOnlyTT $ lift look

instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadIdentity mark1, Monoid w, Commutant mark1
  , forall x. (Monad x) => MonadWriteOnly mark w (t x)
  ) => MonadWriteOnly mark w (ReadOnlyTT mark1 r t m)
  where
    tell
      :: mark w
      -> ReadOnlyTT mark1 r t m ()
    tell = ReadOnlyTT . lift . tell

    draft
      :: ReadOnlyTT mark1 r t m a
      -> ReadOnlyTT mark1 r t m (Pair (mark w) a)
    draft = liftDraftT draft

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadPrompt mark p (t x)
  ) => MonadPrompt mark p (ReadOnlyTT mark1 r t m)
  where
    prompt
      :: mark (p a)
      -> ReadOnlyTT mark1 r t m (mark a)
    prompt = ReadOnlyTT . lift . prompt

instance
  ( Monad m, MonadTrans t, MonadIdentity mark1, MonadIdentity mark
  , forall x. (Monad x) => MonadHalt mark (t x)
  ) => MonadHalt mark (ReadOnlyTT mark1 r t m)
  where
    halt
      :: mark ()
      -> ReadOnlyTT mark1 r t m a
    halt = ReadOnlyTT . lift . halt
