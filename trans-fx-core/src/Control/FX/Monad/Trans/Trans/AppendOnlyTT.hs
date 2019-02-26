{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.FX.Monad.Trans.Trans.AppendOnlyTT (
    AppendOnlyTT(..)
  , runAppendOnlyTT
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
  ( MonadIdentity mark, Monoid w
  ) => RunMonadTransTrans (Val (mark ())) (AppendOnlyTT mark w) (Pair (mark w))
  where
    runTT
      :: ( Monad m, MonadTrans t )
      => Val (mark ()) m
      -> AppendOnlyTT mark w t m a
      -> t m (Pair (mark w) a)
    runTT (Val w) = runT w . unAppendOnlyTT

runAppendOnlyTT
  :: ( MonadIdentity mark, Monad m, MonadTrans t, Monoid w )
  => mark ()
  -> AppendOnlyTT mark w t m a
  -> t m (Pair (mark w) a)
runAppendOnlyTT w = runTT (Val w)

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, Eq a, Eq w
  , forall x. (Eq x) => EqIn h (t m x), Monoid w
  ) => EqIn (Val (mark ()) m, h) (AppendOnlyTT mark w t m a)
  where
    eqIn
      :: (Val (mark ()) m, h)
      -> AppendOnlyTT mark w t m a
      -> AppendOnlyTT mark w t m a
      -> Bool
    eqIn (v, h) x y =
      eqIn h (runTT v x) (runTT v y)





{- Specialized Lifts -}

instance
  ( MonadIdentity mark, Monoid w
  ) => LiftCatchT (Val (mark ())) (AppendOnlyTT mark w) (Pair (mark w))
  where
    liftCatchT
      :: ( Monad m, MonadTrans t )
      => (forall x. Catch e (t m) (Pair (mark w) x))
      -> (forall x. Catch e (AppendOnlyTT mark w t m) x)
    liftCatchT catch x h = AppendOnlyTT $
      liftCatch catch (unAppendOnlyTT x) (unAppendOnlyTT . h)

instance
  ( MonadIdentity mark, Monoid w
  ) => LiftDraftT (Val (mark ())) (AppendOnlyTT mark w) (Pair (mark w))
  where
    liftDraftT
      :: ( Monad m, MonadTrans t, Monoid w1 )
      => (forall x. Draft w1 (t m) (Pair (mark w) x))
      -> (forall x. Draft w1 (AppendOnlyTT mark w t m) x)
    liftDraftT draft = AppendOnlyTT . liftDraft draft . unAppendOnlyTT

instance
  ( MonadIdentity mark, Monoid w
  ) => LiftLocalT (Val (mark ())) (AppendOnlyTT mark w) (Pair (mark w))
  where
    liftLocalT
      :: ( Monad m, MonadTrans t )
      => (forall x. Local r (t m) (Pair (mark w) x))
      -> (forall x. Local r (AppendOnlyTT mark w t m) x)
    liftLocalT local f =
      AppendOnlyTT . liftLocal local f . unAppendOnlyTT





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
