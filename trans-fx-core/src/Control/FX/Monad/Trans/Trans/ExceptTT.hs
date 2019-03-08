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

module Control.FX.Monad.Trans.Trans.ExceptTT (
    ExceptTT(..)
  , runExceptTT
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

deriving instance
  ( Show (t m (Except mark e a))
  ) => Show (ExceptTT mark e t m a)

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
  ( Monad m, MonadTrans t, MonadIdentity mark, Eq e
  , EqIn (t m)
  ) => EqIn (ExceptTT mark e t m)
  where
    newtype Context (ExceptTT mark e t m)
      = ExceptTTCtx
          { unExceptTTCtx :: (mark (), Context (t m))
          } deriving (Typeable)

    eqIn
      :: (Eq a)
      => Context (ExceptTT mark e t m)
      -> ExceptTT mark e t m a
      -> ExceptTT mark e t m a
      -> Bool
    eqIn (ExceptTTCtx (v,h)) x y =
      eqIn h
        (fmap unExceptTTOut $ runTT (ExceptTTIn v) x)
        (fmap unExceptTTOut $ runTT (ExceptTTIn v) y)

deriving instance
  ( Eq (mark ()), Eq (Context (t m))
  ) => Eq (Context (ExceptTT mark e t m))

deriving instance
  ( Show (mark ()), Show (Context (t m))
  ) => Show (Context (ExceptTT mark e t m))



instance
  ( MonadIdentity mark
  ) => RunMonadTransTrans (ExceptTT mark e)
  where
    newtype InputTT (ExceptTT mark e) m
      = ExceptTTIn
          { unExceptTTIn :: mark ()
          } deriving (Typeable)

    newtype OutputTT (ExceptTT mark e) a
      = ExceptTTOut
          { unExceptTTOut :: Except mark e a
          } deriving (Typeable)

    runTT
      :: ( Monad m, MonadTrans t )
      => InputTT (ExceptTT mark e) m
      -> ExceptTT mark e t m a
      -> t m (OutputTT (ExceptTT mark e) a)
    runTT (ExceptTTIn s) =
      fmap (ExceptTTOut . unExceptTOut)
        . runT (ExceptTIn s) . unExceptTT

deriving instance
  ( Eq (mark ())
  ) => Eq (InputTT (ExceptTT mark e) m)

deriving instance
  ( Show (mark ())
  ) => Show (InputTT (ExceptTT mark e) m)

deriving instance
  ( Eq e, Eq a
  ) => Eq (OutputTT (ExceptTT mark e) a)

deriving instance
  ( Show e, Show a
  ) => Show (OutputTT (ExceptTT mark e) a)

runExceptTT
  :: ( MonadIdentity mark, Monad m, MonadTrans t )
  => mark ()
  -> ExceptTT mark e t m a
  -> t m (Except mark e a)
runExceptTT s = fmap unExceptTTOut . runTT (ExceptTTIn s)





{- Specialized Lifts -}

instance
  ( MonadIdentity mark
  ) => LiftCatchT (ExceptTT mark e)
  where
    liftCatchT
      :: forall m t e1
       . ( Monad m, MonadTrans t )
      => (forall x. Catch e1 (t m) (OutputTT (ExceptTT mark e) x))
      -> (forall x. Catch e1 (ExceptTT mark e t m) x)
    liftCatchT catch x h =
      let
        catch' :: Catch e1 (t m) (OutputT (ExceptT mark e) x)
        catch' y g =
          fmap (ExceptTOut . unExceptTTOut) $ catch
            (fmap (ExceptTTOut . unExceptTOut) y)
            (fmap (ExceptTTOut . unExceptTOut) . g)
      in
        ExceptTT $ liftCatch catch' (unExceptTT x) (unExceptTT . h)

instance
  ( MonadIdentity mark
  ) => LiftDraftT (ExceptTT mark e)
  where
    liftDraftT
      :: forall m t w
       . ( Monad m, MonadTrans t, Monoid w )
      => (forall x. Draft w (t m) (OutputTT (ExceptTT mark e) x))
      -> (forall x. Draft w (ExceptTT mark e t m) x)
    liftDraftT draft =
      let
        draft' :: Draft w (t m) (OutputT (ExceptT mark e) x)
        draft' =
          fmap (fmap (ExceptTOut . unExceptTTOut))
            . draft . fmap (ExceptTTOut . unExceptTOut)
      in
        ExceptTT . liftDraft draft' . unExceptTT

instance
  ( MonadIdentity mark
  ) => LiftLocalT (ExceptTT mark e)
  where
    liftLocalT
      :: forall m t r
       . ( Monad m, MonadTrans t )
      => (forall x. Local r (t m) (OutputTT (ExceptTT mark e) x))
      -> (forall x. Local r (ExceptTT mark e t m) x)
    liftLocalT local f =
      let
        local' :: Local r (t m) (OutputT (ExceptT mark e) x)
        local' g =
          fmap (ExceptTOut . unExceptTTOut)
            . local g . fmap (ExceptTTOut . unExceptTOut)
      in
        ExceptTT . liftLocal local' f . unExceptTT





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
