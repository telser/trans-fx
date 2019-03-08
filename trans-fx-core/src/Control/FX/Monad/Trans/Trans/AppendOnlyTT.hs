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

module Control.FX.Monad.Trans.Trans.AppendOnlyTT (
    AppendOnlyTT(..)
  , runAppendOnlyTT
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



newtype AppendOnlyTT
  (mark :: * -> *)
  (w :: *)
  (t :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = AppendOnlyTT
        { unAppendOnlyTT :: AppendOnlyT mark w (t m) a
        } deriving
          ( Show, Typeable, Functor, Applicative, Monad )

instance
  ( MonadTrans t, MonadIdentity mark, Monoid w
  ) => MonadTrans (AppendOnlyTT mark w t)
  where
    lift
      :: ( Monad m )
      => m a
      -> AppendOnlyTT mark w t m a
    lift = AppendOnlyTT . lift . lift

instance
  ( MonadFunctor t, MonadIdentity mark, Monoid w
  ) => MonadFunctor (AppendOnlyTT mark w t)
  where
    hoist
      :: ( Monad m, Monad n )
      => (forall u. m u -> n u)
      -> AppendOnlyTT mark w t m a
      -> AppendOnlyTT mark w t n a
    hoist f = AppendOnlyTT . hoist (hoist f) . unAppendOnlyTT

instance
  ( MonadIdentity mark, Monoid w
  ) => MonadTransTrans (AppendOnlyTT mark w)
  where
    liftT
      :: ( Monad m, MonadTrans t )
      => t m a
      -> AppendOnlyTT mark w t m a
    liftT = AppendOnlyTT . lift





instance
  ( Monad m, MonadTrans t, MonadIdentity mark, Eq w
  , EqIn (t m), Monoid w
  ) => EqIn (AppendOnlyTT mark w t m)
  where
    newtype Context (AppendOnlyTT mark w t m)
      = AppendOnlyTTCtx
          { unAppendOnlyTTCtx :: (mark (), Context (t m))
          } deriving (Typeable)

    eqIn
      :: (Eq a)
      => Context (AppendOnlyTT mark w t m)
      -> AppendOnlyTT mark w t m a
      -> AppendOnlyTT mark w t m a
      -> Bool
    eqIn (AppendOnlyTTCtx (v,h)) x y =
      eqIn h
        (fmap unAppendOnlyTTOut $ runTT (AppendOnlyTTIn v) x)
        (fmap unAppendOnlyTTOut $ runTT (AppendOnlyTTIn v) y)

deriving instance
  ( Eq (mark ()), Eq (Context (t m))
  ) => Eq (Context (AppendOnlyTT mark w t m))

deriving instance
  ( Show (mark ()), Show (Context (t m))
  ) => Show (Context (AppendOnlyTT mark w t m))



instance
  ( MonadIdentity mark, Monoid w
  ) => RunMonadTransTrans (AppendOnlyTT mark w)
  where
    newtype InputTT (AppendOnlyTT mark w) m
      = AppendOnlyTTIn
          { unAppendOnlyTTIn :: mark ()
          } deriving (Typeable)

    newtype OutputTT (AppendOnlyTT mark w) a
      = AppendOnlyTTOut
          { unAppendOnlyTTOut :: Pair (mark w) a
          } deriving (Typeable)

    runTT
      :: ( Monad m, MonadTrans t )
      => InputTT (AppendOnlyTT mark w) m
      -> AppendOnlyTT mark w t m a
      -> t m (OutputTT (AppendOnlyTT mark w) a)
    runTT (AppendOnlyTTIn w) =
      fmap (AppendOnlyTTOut . unAppendOnlyTOut)
        . runT (AppendOnlyTIn w) . unAppendOnlyTT

deriving instance
  ( Eq (mark ())
  ) => Eq (InputTT (AppendOnlyTT mark w) m)

deriving instance
  ( Show (mark ())
  ) => Show (InputTT (AppendOnlyTT mark w) m)

deriving instance
  ( Eq (mark w), Eq a
  ) => Eq (OutputTT (AppendOnlyTT mark w) a)

deriving instance
  ( Show (mark w), Show a
  ) => Show (OutputTT (AppendOnlyTT mark w) a)

runAppendOnlyTT
  :: ( MonadIdentity mark, Monad m, MonadTrans t, Monoid w )
  => mark ()
  -> AppendOnlyTT mark w t m a
  -> t m (Pair (mark w) a)
runAppendOnlyTT w = fmap unAppendOnlyTTOut . runTT (AppendOnlyTTIn w)











{- Specialized Lifts -}

instance
  ( MonadIdentity mark, Monoid w
  ) => LiftCatchT (AppendOnlyTT mark w)
  where
    liftCatchT
      :: forall m t e
       . ( Monad m, MonadTrans t )
      => (forall x. Catch e (t m) (OutputTT (AppendOnlyTT mark w) x))
      -> (forall x. Catch e (AppendOnlyTT mark w t m) x)
    liftCatchT catch x h =
      let
        catch' :: Catch e (t m) (OutputT (AppendOnlyT mark w) x)
        catch' y g =
          fmap (AppendOnlyTOut . unAppendOnlyTTOut) $ catch
            (fmap (AppendOnlyTTOut . unAppendOnlyTOut) y)
            (fmap (AppendOnlyTTOut . unAppendOnlyTOut) . g)
      in
        AppendOnlyTT $ liftCatch catch' (unAppendOnlyTT x) (unAppendOnlyTT . h)

instance
  ( MonadIdentity mark, Monoid w
  ) => LiftDraftT (AppendOnlyTT mark w)
  where
    liftDraftT
      :: forall m t w1
       . ( Monad m, MonadTrans t, Monoid w1 )
      => (forall x. Draft w1 (t m) (OutputTT (AppendOnlyTT mark w) x))
      -> (forall x. Draft w1 (AppendOnlyTT mark w t m) x)
    liftDraftT draft =
      let
        draft' :: Draft w1 (t m) (OutputT (AppendOnlyT mark w) x)
        draft' =
          fmap (fmap (AppendOnlyTOut . unAppendOnlyTTOut))
            . draft . fmap (AppendOnlyTTOut . unAppendOnlyTOut)
      in
        AppendOnlyTT . liftDraft draft' . unAppendOnlyTT

instance
  ( MonadIdentity mark, Monoid w
  ) => LiftLocalT (AppendOnlyTT mark w)
  where
    liftLocalT
      :: forall m t r
       . ( Monad m, MonadTrans t )
      => (forall x. Local r (t m) (OutputTT (AppendOnlyTT mark w) x))
      -> (forall x. Local r (AppendOnlyTT mark w t m) x)
    liftLocalT local f =
      let
        local' :: Local r (t m) (OutputT (AppendOnlyT mark w) x)
        local' g =
          fmap (AppendOnlyTOut . unAppendOnlyTTOut)
            . local g . fmap (AppendOnlyTTOut . unAppendOnlyTOut)
      in
        AppendOnlyTT . liftLocal local' f . unAppendOnlyTT





{- Effect Classes -}

instance {-# OVERLAPPING #-}
  ( Monad m, MonadTrans t, MonadIdentity mark, Monoid w
  ) => MonadAppendOnly mark w (AppendOnlyTT mark w t m)
  where
    look
      :: AppendOnlyTT mark w t m (mark w)
    look = AppendOnlyTT look

    jot
      :: mark w
      -> AppendOnlyTT mark w t m ()
    jot = AppendOnlyTT . jot

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadAppendOnly mark w (t x), Monoid w1, Monoid w
  ) => MonadAppendOnly mark w (AppendOnlyTT mark1 w1 t m)
  where
    look
      :: AppendOnlyTT mark1 w1 t m (mark w)
    look = AppendOnlyTT $ lift look

    jot
      :: mark w
      -> AppendOnlyTT mark1 w1 t m ()
    jot = AppendOnlyTT . lift . jot

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadState mark s (t x), Monoid w
  ) => MonadState mark s (AppendOnlyTT mark1 w t m)
  where
    get
      :: AppendOnlyTT mark1 w t m (mark s)
    get = AppendOnlyTT $ lift get

    put
      :: mark s
      -> AppendOnlyTT mark1 w t m ()
    put = AppendOnlyTT . lift . put

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadExcept mark e (t x), Monoid w
  ) => MonadExcept mark e (AppendOnlyTT mark1 w t m)
  where
    throw
      :: mark e
      -> AppendOnlyTT mark1 w t m a
    throw = AppendOnlyTT . lift . throw

    catch
      :: AppendOnlyTT mark1 w t m a
      -> (mark e -> AppendOnlyTT mark1 w t m a)
      -> AppendOnlyTT mark1 w t m a
    catch = liftCatchT catch

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1, Monoid w
  , forall x. (Monad x) => MonadWriteOnly mark w (t x), Monoid w1
  ) => MonadWriteOnly mark w (AppendOnlyTT mark1 w1 t m)
  where
    tell
      :: mark w
      -> AppendOnlyTT mark1 w1 t m ()
    tell = AppendOnlyTT . lift . tell

    draft
      :: AppendOnlyTT mark1 w1 t m a
      -> AppendOnlyTT mark1 w1 t m (Pair (mark w) a)
    draft = liftDraftT draft

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadReadOnly mark r (t x), Monoid w
  ) => MonadReadOnly mark r (AppendOnlyTT mark1 w t m)
  where
    ask
      :: AppendOnlyTT mark1 w t m (mark r)
    ask = AppendOnlyTT $ lift ask

    local
      :: (mark r -> mark r)
      -> AppendOnlyTT mark1 w t m a
      -> AppendOnlyTT mark1 w t m a
    local = liftLocalT local

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadPrompt mark p (t x), Monoid w
  ) => MonadPrompt mark p (AppendOnlyTT mark1 w t m)
  where
    prompt
      :: mark (p a)
      -> AppendOnlyTT mark1 w t m (mark a)
    prompt = AppendOnlyTT . lift . prompt

instance
  ( Monad m, MonadTrans t, MonadIdentity mark1, MonadIdentity mark, Monoid w
  , forall x. (Monad x) => MonadHalt mark (t x)
  ) => MonadHalt mark (AppendOnlyTT mark1 w t m)
  where
    halt
      :: mark ()
      -> AppendOnlyTT mark1 w t m a
    halt = AppendOnlyTT . lift . halt
