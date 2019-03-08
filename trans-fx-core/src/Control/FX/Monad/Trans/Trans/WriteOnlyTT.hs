{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.FX.Monad.Trans.Trans.WriteOnlyTT (
    WriteOnlyTT(..)
  , runWriteOnlyTT
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
  ( Monad m, MonadTrans t, MonadIdentity mark, Eq w
  , EqIn (t m), Monoid w
  ) => EqIn (WriteOnlyTT mark w t m)
  where
    newtype Context (WriteOnlyTT mark w t m)
      = WriteOnlyTTCtx
          { unWriteOnlyTTCtx :: (mark (), Context (t m))
          } deriving (Typeable)

    eqIn
      :: (Eq a)
      => Context (WriteOnlyTT mark w t m)
      -> WriteOnlyTT mark w t m a
      -> WriteOnlyTT mark w t m a
      -> Bool
    eqIn (WriteOnlyTTCtx (v,h)) x y =
      eqIn h
        (fmap unWriteOnlyTTOut $ runTT (WriteOnlyTTIn v) x)
        (fmap unWriteOnlyTTOut $ runTT (WriteOnlyTTIn v) y)

deriving instance
  ( Eq (mark ()), Eq (Context (t m))
  ) => Eq (Context (WriteOnlyTT mark w t m))

deriving instance
  ( Show (mark ()), Show (Context (t m))
  ) => Show (Context (WriteOnlyTT mark w t m))



instance
  ( MonadIdentity mark, Monoid w
  ) => RunMonadTransTrans (WriteOnlyTT mark w)
  where
    newtype InputTT (WriteOnlyTT mark w) m
      = WriteOnlyTTIn
          { unWriteOnlyTTIn :: mark ()
          } deriving (Typeable)

    newtype OutputTT (WriteOnlyTT mark w) a
      = WriteOnlyTTOut
          { unWriteOnlyTTOut :: Pair (mark w) a
          } deriving (Typeable)

    runTT
      :: ( Monad m, MonadTrans t )
      => InputTT (WriteOnlyTT mark w) m
      -> WriteOnlyTT mark w t m a
      -> t m (OutputTT (WriteOnlyTT mark w) a)
    runTT (WriteOnlyTTIn w) =
      fmap (WriteOnlyTTOut . unWriteOnlyTOut)
        . runT (WriteOnlyTIn w) . unWriteOnlyTT

deriving instance
  ( Eq (mark ())
  ) => Eq (InputTT (WriteOnlyTT mark w) m)

deriving instance
  ( Show (mark ())
  ) => Show (InputTT (WriteOnlyTT mark w) m)

deriving instance
  ( Eq (mark w), Eq a
  ) => Eq (OutputTT (WriteOnlyTT mark w) a)

deriving instance
  ( Show (mark w), Show a
  ) => Show (OutputTT (WriteOnlyTT mark w) a)

runWriteOnlyTT
  :: ( MonadIdentity mark, Monad m, MonadTrans t, Monoid w )
  => mark ()
  -> WriteOnlyTT mark w t m a
  -> t m (Pair (mark w) a)
runWriteOnlyTT w = fmap unWriteOnlyTTOut . runTT (WriteOnlyTTIn w)





{- Specialized Lifts -}

instance
  ( MonadIdentity mark, Monoid w
  ) => LiftCatchT (WriteOnlyTT mark w)
  where
    liftCatchT
      :: forall m t e
       . ( Monad m, MonadTrans t )
      => (forall x. Catch e (t m) (OutputTT (WriteOnlyTT mark w) x))
      -> (forall x. Catch e (WriteOnlyTT mark w t m) x)
    liftCatchT catch x h =
      let
        catch' :: Catch e (t m) (OutputT (WriteOnlyT mark w) x)
        catch' y g =
          fmap (WriteOnlyTOut . unWriteOnlyTTOut) $ catch
            (fmap (WriteOnlyTTOut . unWriteOnlyTOut) y)
            (fmap (WriteOnlyTTOut . unWriteOnlyTOut) . g)
      in
        WriteOnlyTT $ liftCatch catch' (unWriteOnlyTT x) (unWriteOnlyTT . h)

instance
  ( MonadIdentity mark, Monoid w
  ) => LiftDraftT (WriteOnlyTT mark w)
  where
    liftDraftT
      :: forall m t w w2
       . ( Monad m, MonadTrans t, Monoid w, Monoid w2 )
      => (forall x. Draft w2 (t m) (OutputTT (WriteOnlyTT mark w) x))
      -> (forall x. Draft w2 (WriteOnlyTT mark w t m) x)
    liftDraftT draft =
      let
        draft' :: Draft w2 (t m) (OutputT (WriteOnlyT mark w) x)
        draft' =
          fmap (fmap (WriteOnlyTOut . unWriteOnlyTTOut))
            . draft . fmap (WriteOnlyTTOut . unWriteOnlyTOut)
      in
        WriteOnlyTT . liftDraft draft' . unWriteOnlyTT

instance
  ( MonadIdentity mark, Monoid w
  ) => LiftLocalT (WriteOnlyTT mark w)
  where
    liftLocalT
      :: forall m t r
       . ( Monad m, MonadTrans t )
      => (forall x. Local r (t m) (OutputTT (WriteOnlyTT mark w) x))
      -> (forall x. Local r (WriteOnlyTT mark w t m) x)
    liftLocalT local f =
      let
        local' :: Local r (t m) (OutputT (WriteOnlyT mark w) x)
        local' g =
          fmap (WriteOnlyTOut . unWriteOnlyTTOut)
            . local g . fmap (WriteOnlyTTOut . unWriteOnlyTOut)
      in
        WriteOnlyTT . liftLocal local' f . unWriteOnlyTT





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
