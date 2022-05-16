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

module Control.FX.Monad.Trans.Trans.Data.StackTT (
    StackTT(..)
  , runStackTT
  , Context(..)
  , InputTT(..)
  , OutputTT(..)
) where



import Data.Typeable (Typeable, Proxy)

import Control.FX
import Control.FX.Structure.Stack
import Control.FX.Monad.Data.Class
import Control.FX.Monad.Trans.Data.StackT



newtype StackTT
  (mark :: * -> *)
  (f :: * -> *)
  (d :: *)
  (t :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = StackTT
        { unStackTT :: StackT mark f d (t m) a
        } deriving
          ( Show, Typeable, Functor, Applicative, Monad )

instance
  ( MonadTrans t, MonadIdentity mark
  ) => MonadTrans (StackTT mark f d t)
  where
    lift
      :: ( Monad m )
      => m a
      -> StackTT mark f d t m a
    lift = StackTT . lift . lift

instance
  ( MonadFunctor t, MonadIdentity mark
  ) => MonadFunctor (StackTT mark f d t)
  where
    hoist
      :: ( Monad m, Monad n )
      => (forall u. m u -> n u)
      -> StackTT mark f d t m a
      -> StackTT mark f d t n a
    hoist f = StackTT . hoist (hoist f) . unStackTT

instance
  ( MonadIdentity mark
  ) => MonadTransTrans (StackTT mark f d)
  where
    liftT
      :: ( Monad m, MonadTrans t )
      => t m a
      -> StackTT mark f d t m a
    liftT = StackTT . lift





instance
  ( Monad m, MonadTrans t, MonadIdentity mark, Eq (f d)
  , EqIn (t m), IsStack f
  ) => EqIn (StackTT mark f d t m)
  where
    newtype Context (StackTT mark f d t m)
      = StackTTCtx
          { unStackTTCtx :: (mark (f d), Context (t m))
          } deriving (Typeable)

    eqIn
      :: (Eq a)
      => Context (StackTT mark f d t m)
      -> StackTT mark f d t m a
      -> StackTT mark f d t m a
      -> Bool
    eqIn (StackTTCtx (v,h)) x y =
      eqIn h
        (fmap unStackTTOut $ runTT (StackTTIn v) x)
        (fmap unStackTTOut $ runTT (StackTTIn v) y)

deriving instance
  ( Eq (mark (f d)), Eq (Context (t m))
  ) => Eq (Context (StackTT mark f d t m))

deriving instance
  ( Show (mark (f d)), Show (Context (t m))
  ) => Show (Context (StackTT mark f d t m))



instance
  ( MonadIdentity mark, IsStack f
  ) => RunMonadTransTrans (StackTT mark f d)
  where
    newtype InputTT (StackTT mark f d) m
      = StackTTIn
          { unStackTTIn :: mark (f d)
          } deriving (Typeable)

    newtype OutputTT (StackTT mark f d) a
      = StackTTOut
          { unStackTTOut :: Pair (mark (f d)) a
          } deriving (Typeable)

    runTT
      :: ( Monad m, MonadTrans t )
      => InputTT (StackTT mark f d) m
      -> StackTT mark f d t m a
      -> t m (OutputTT (StackTT mark f d) a)
    runTT (StackTTIn s) =
      fmap (StackTTOut . unStackTOut)
        . runT (StackTIn s) . unStackTT

deriving instance
  ( Eq (mark (f d))
  ) => Eq (InputTT (StackTT mark f d) m)

deriving instance
  ( Show (mark (f d))
  ) => Show (InputTT (StackTT mark f d) m)

deriving instance
  ( Eq (mark (f d)), Eq a
  ) => Eq (OutputTT (StackTT mark f d) a)

deriving instance
  ( Show (mark (f d)), Show a
  ) => Show (OutputTT (StackTT mark f d) a)

runStackTT
  :: ( MonadIdentity mark, Monad m, MonadTrans t, IsStack f )
  => mark (f d)
  -> StackTT mark f d t m a
  -> t m (Pair (mark (f d)) a)
runStackTT s = fmap unStackTTOut . runTT (StackTTIn s)





{- Specialized Lifts -}

instance
  ( MonadIdentity mark, IsStack f
  ) => LiftCatchT (StackTT mark f d)
  where
    liftCatchT
      :: forall m t e
       . ( Monad m, MonadTrans t )
      => (forall x. Catch e (t m) (OutputTT (StackTT mark f d) x))
      -> (forall x. Catch e (StackTT mark f d t m) x)
    liftCatchT catch x h =
      let
        catch' :: Catch e (t m) (OutputT (StackT mark f d) x)
        catch' y g =
          fmap (StackTOut . unStackTTOut) $ catch
            (fmap (StackTTOut . unStackTOut) y)
            (fmap (StackTTOut . unStackTOut) . g)
      in
        StackTT $
          liftCatch catch' (unStackTT x) (unStackTT . h)

instance
  ( MonadIdentity mark, IsStack f
  ) => LiftDraftT (StackTT mark f d)
  where
    liftDraftT
      :: forall m t w
       . ( Monad m, MonadTrans t, Monoid w )
      => (forall x. Draft w (t m) (OutputTT (StackTT mark f d) x))
      -> (forall x. Draft w (StackTT mark f d t m) x)
    liftDraftT draft =
      let
        draft' :: Draft w (t m) (OutputT (StackT mark f d) x)
        draft' =
          fmap (fmap (StackTOut . unStackTTOut))
            . draft . fmap (StackTTOut . unStackTOut)
      in
        StackTT . liftDraft draft' . unStackTT

instance
  ( MonadIdentity mark, IsStack f
  ) => LiftLocalT (StackTT mark f d)
  where
    liftLocalT
      :: forall m t r
       . ( Monad m, MonadTrans t )
      => (forall x. Local r (t m) (OutputTT (StackTT mark f d) x))
      -> (forall x. Local r (StackTT mark f d t m) x)
    liftLocalT local f =
      let
        local' :: Local r (t m) (OutputT (StackT mark f d) x)
        local' g =
          fmap (StackTOut . unStackTTOut)
            . local g . fmap (StackTTOut . unStackTOut)
      in
        StackTT . liftLocal local' f . unStackTT





{- Effect Classes -}

instance {-# OVERLAPPING #-}
  ( Monad m, MonadTrans t, MonadIdentity mark, IsStack f
  ) => MonadStack mark f d (StackTT mark f d t m)
  where
    push
      :: Proxy f
      -> mark d
      -> StackTT mark f d t m ()
    push proxy = StackTT . push proxy

    pop
      :: Proxy f
      -> StackTT mark f d t m (mark (Maybe d))
    pop proxy = StackTT $ pop proxy

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadStack mark f d (t x), IsStack f
  ) => MonadStack mark f d (StackTT mark1 f1 d1 t m)
  where
    push
      :: Proxy f
      -> mark d
      -> StackTT mark1 f1 d1 t m ()
    push proxy = StackTT . lift . push proxy

    pop
      :: Proxy f
      -> StackTT mark1 f1 d1 t m (mark (Maybe d))
    pop proxy = StackTT $ lift $ pop proxy

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadState mark s (t x), IsStack f
  ) => MonadState mark s (StackTT mark1 f d1 t m)
  where
    get
      :: StackTT mark1 f d1 t m (mark s)
    get = StackTT $ lift get

    put
      :: mark s
      -> StackTT mark1 f d1 t m ()
    put = StackTT . lift . put

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadExcept mark e (t x), IsStack f
  ) => MonadExcept mark e (StackTT mark1 f d t m)
  where
    throw
      :: mark e
      -> StackTT mark1 f d t m a
    throw = StackTT . lift . throw

    catch
      :: StackTT mark1 f d t m a
      -> (mark e -> StackTT mark1 f d t m a)
      -> StackTT mark1 f d t m a
    catch = liftCatchT catch

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1, Monoid w
  , forall x. (Monad x) => MonadWriteOnly mark w (t x), IsStack f
  ) => MonadWriteOnly mark w (StackTT mark1 f d t m)
  where
    tell
      :: mark w
      -> StackTT mark1 f d t m ()
    tell = StackTT . lift . tell

    draft
      :: StackTT mark1 f d t m a
      -> StackTT mark1 f d t m (Pair (mark w) a)
    draft = liftDraftT draft

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1, Monoid w
  , forall x. (Monad x) => MonadAppendOnly mark w (t x), IsStack f
  ) => MonadAppendOnly mark w (StackTT mark1 f d t m)
  where
    jot
      :: mark w
      -> StackTT mark1 f d t m ()
    jot = StackTT . lift . jot

    look
      :: StackTT mark1 f d t m (mark w)
    look = StackTT $ lift look

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadWriteOnce mark w (t x), IsStack f
  ) => MonadWriteOnce mark w (StackTT mark1 f d t m)
  where
    etch
      :: mark w
      -> StackTT mark1 f d t m Bool
    etch = StackTT . lift . etch

    press
      :: StackTT mark1 f d t m (Maybe (mark w))
    press = StackTT $ lift press

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadReadOnly mark r (t x), IsStack f
  ) => MonadReadOnly mark r (StackTT mark1 f d t m)
  where
    ask
      :: StackTT mark1 f d t m (mark r)
    ask = StackTT $ lift ask

    local
      :: (mark r -> mark r)
      -> StackTT mark1 f d t m a
      -> StackTT mark1 f d t m a
    local = liftLocalT local

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadPrompt mark p (t x), IsStack f
  ) => MonadPrompt mark p (StackTT mark1 f d t m)
  where
    prompt
      :: mark (p a)
      -> StackTT mark1 f d t m (mark a)
    prompt = StackTT . lift . prompt

instance
  ( Monad m, MonadTrans t, MonadIdentity mark1, MonadIdentity mark
  , forall x. (Monad x) => MonadHalt mark (t x), IsStack f
  ) => MonadHalt mark (StackTT mark1 f d t m)
  where
    halt
      :: mark ()
      -> StackTT mark1 f d t m a
    halt = StackTT . lift . halt
