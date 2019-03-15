{-# LANGUAGE Rank2Types                 #-}
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

module Control.FX.Monad.Trans.Trans.ReadOnlyTT (
    ReadOnlyTT(..)
  , runReadOnlyTT
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
  ( Monad m, MonadTrans t, MonadIdentity mark, Commutant mark
  , EqIn (t m)
  ) => EqIn (ReadOnlyTT mark r t m)
  where
    newtype Context (ReadOnlyTT mark r t m)
      = ReadOnlyTTCtx
          { unReadOnlyTTCtx :: (mark r, Context (t m))
          } deriving (Typeable)

    eqIn
      :: (Eq a)
      => Context (ReadOnlyTT mark r t m)
      -> ReadOnlyTT mark r t m a
      -> ReadOnlyTT mark r t m a
      -> Bool
    eqIn (ReadOnlyTTCtx (v,h)) x y =
      eqIn h
        (fmap unReadOnlyTTOut $ runTT (ReadOnlyTTIn v) x)
        (fmap unReadOnlyTTOut $ runTT (ReadOnlyTTIn v) y)

deriving instance
  ( Eq (mark r), Eq (Context (t m))
  ) => Eq (Context (ReadOnlyTT mark r t m))

deriving instance
  ( Show (mark r), Show (Context (t m))
  ) => Show (Context (ReadOnlyTT mark r t m))



instance
  ( MonadIdentity mark, Commutant mark
  ) => RunMonadTransTrans (ReadOnlyTT mark r)
  where
    newtype InputTT (ReadOnlyTT mark r) m
      = ReadOnlyTTIn
          { unReadOnlyTTIn :: mark r
          } deriving (Typeable)

    newtype OutputTT (ReadOnlyTT mark r) a
      = ReadOnlyTTOut
          { unReadOnlyTTOut :: mark a
          } deriving (Typeable)

    runTT
      :: ( Monad m, MonadTrans t )
      => InputTT (ReadOnlyTT mark r) m
      -> ReadOnlyTT mark r t m a
      -> t m (OutputTT (ReadOnlyTT mark r) a)
    runTT (ReadOnlyTTIn r) =
      fmap (ReadOnlyTTOut . unReadOnlyTOut)
        . runT (ReadOnlyTIn r) . unReadOnlyTT

deriving instance
  ( Eq (mark r)
  ) => Eq (InputTT (ReadOnlyTT mark r) m)

deriving instance
  ( Show (mark r)
  ) => Show (InputTT (ReadOnlyTT mark r) m)

deriving instance
  ( Eq (mark a)
  ) => Eq (OutputTT (ReadOnlyTT mark r) a)

deriving instance
  ( Show (mark a)
  ) => Show (OutputTT (ReadOnlyTT mark r) a)

runReadOnlyTT
  :: ( MonadIdentity mark, Commutant mark, Monad m, MonadTrans t )
  => mark r
  -> ReadOnlyTT mark r t m a
  -> t m (mark a)
runReadOnlyTT r = fmap unReadOnlyTTOut . runTT (ReadOnlyTTIn r)







{- Specialized Lifts -}

instance
  ( MonadIdentity mark, Commutant mark
  ) => LiftCatchT (ReadOnlyTT mark r)
  where
    liftCatchT
      :: forall m t e
       . ( Monad m, MonadTrans t )
      => (forall x. Catch e (t m) (OutputTT (ReadOnlyTT mark r) x))
      -> (forall x. Catch e (ReadOnlyTT mark r t m) x)
    liftCatchT catch x h =
      let
        catch' :: Catch e (t m) (OutputT (ReadOnlyT mark r) x)
        catch' y g =
          fmap (ReadOnlyTOut . unReadOnlyTTOut) $ catch
            (fmap (ReadOnlyTTOut . unReadOnlyTOut) y)
            (fmap (ReadOnlyTTOut . unReadOnlyTOut) . g)
      in
        ReadOnlyTT $ liftCatch catch' (unReadOnlyTT x) (unReadOnlyTT . h)

instance
  ( MonadIdentity mark, Commutant mark
  ) => LiftDraftT (ReadOnlyTT mark r)
  where
    liftDraftT
      :: forall m t w
       . ( Monad m, MonadTrans t, Monoid w )
      => (forall x. Draft w (t m) (OutputTT (ReadOnlyTT mark r) x))
      -> (forall x. Draft w (ReadOnlyTT mark r t m) x)
    liftDraftT draft =
      let
        draft' :: Draft w (t m) (OutputT (ReadOnlyT mark r) x)
        draft' =
          fmap (fmap (ReadOnlyTOut . unReadOnlyTTOut))
            . draft . fmap (ReadOnlyTTOut . unReadOnlyTOut)
      in
        ReadOnlyTT . liftDraft draft' . unReadOnlyTT

instance
  ( MonadIdentity mark, Commutant mark
  ) => LiftLocalT (ReadOnlyTT mark r)
  where
    liftLocalT
      :: forall m t r2
       . ( Monad m, MonadTrans t )
      => (forall x. Local r2 (t m) (OutputTT (ReadOnlyTT mark r) x))
      -> (forall x. Local r2 (ReadOnlyTT mark r t m) x)
    liftLocalT local f =
      let
        local' :: Local r2 (t m) (OutputT (ReadOnlyT mark r) x)
        local' g =
          fmap (ReadOnlyTOut . unReadOnlyTTOut)
            . local g . fmap (ReadOnlyTTOut . unReadOnlyTOut)
      in
        ReadOnlyTT . liftLocal local' f . unReadOnlyTT





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
  , MonadIdentity mark1, Commutant mark1
  , forall x. (Monad x) => MonadWriteOnce mark w (t x)
  ) => MonadWriteOnce mark w (ReadOnlyTT mark1 r t m)
  where
    etch
      :: mark w
      -> ReadOnlyTT mark1 r t m Bool
    etch = ReadOnlyTT . lift . etch

    press
      :: ReadOnlyTT mark1 r1 t m (Maybe (mark w))
    press = ReadOnlyTT $ lift press

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
