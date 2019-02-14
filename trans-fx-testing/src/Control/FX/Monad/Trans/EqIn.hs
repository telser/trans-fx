{-#
  LANGUAGE
    FlexibleContexts,
    FlexibleInstances,
    MultiParamTypeClasses
#-}

module Control.FX.Monad.Trans.EqIn where

import Control.FX.EqIn.Class
import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans

instance
  ( EqIn h (m a), Functor m
  ) => EqIn ((),h) (IdentityT m a)
  where
    eqIn ((),h) (IdentityT x) (IdentityT y) =
      eqIn h x y

instance
  ( EqIn h (m a), Functor m
  ) => EqIn (r,h) (ReaderT mark r m a)
  where
    eqIn (r,h) (ReaderT x) (ReaderT y) =
      eqIn h (unReader x r) (unReader y r)

instance
  ( EqIn h (m (Pair s a))
  ) => EqIn (s,h) (StateT mark s m a)
  where
    eqIn (s,h) (StateT x) (StateT y) =
      eqIn h (x s) (y s)

instance
  ( EqIn h (m (Except mark e a))
  ) => EqIn ((),h) (ExceptT mark e m a)
  where
    eqIn ((),h) (ExceptT x) (ExceptT y) =
      eqIn h x y

instance
  ( EqIn h (m (Writer mark e a))
  ) => EqIn ((),h) (WriterT mark e m a)
  where
    eqIn ((),h) (WriterT x) (WriterT y) =
      eqIn h x y

instance
  ( EqIn h (m (Maybe a))
  ) => EqIn ((),h) (MaybeT m a)
  where
    eqIn ((),h) (MaybeT x) (MaybeT y) =
      eqIn h x y

instance
  ( EqIn (w1,(w2,w3)) (t1 (t2 m) a)
  ) => EqIn ((w1,w2),w3) (ComposeT t1 t2 m a)
  where
    eqIn ((w1,w2),w3) (ComposeT x) (ComposeT y) =
      eqIn (w1,(w2,w3)) x y
