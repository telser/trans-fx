{-#
  LANGUAGE
    InstanceSigs,
    FlexibleInstances,
    MultiParamTypeClasses
#-}

module Control.FX.Monad.Class (
    Central(..)
  , RunMonad(..)

  , MonadIdentity(..)
  , MonadMaybe(..)
  , MonadExcept(..)
  , MonadState(..)
  , MonadWriteOnly(..)
  , MonadReadOnly(..)
) where

import Control.FX.Functor



class
  ( Commutant c, Monad c
  ) => Central c

instance Central Maybe
instance Central (Either e)
instance Central Tag




class
  ( Monad m, Commutant f
  ) => RunMonad z m f
  where
    run :: z -> m a -> f a

instance RunMonad () Maybe Maybe where
  run () = id



{- Basic Effect Classes -}

class
  ( Monad m
  ) => MonadIdentity m
  where
    unwrap :: m a -> a

class
  ( Monad m
  ) => MonadMaybe m
  where
    bail :: m a

class
  ( Monad m, MonadIdentity mark
  ) => MonadExcept mark e m
  where
    throw :: mark e -> m a
    catch :: m a -> (mark e -> m a) -> m a

class
  ( Monad m, MonadIdentity mark
  ) => MonadState mark s m
  where
    get :: m (mark s)
    put :: (mark s) -> m ()

class
  ( Monad m, Monoid w, MonadIdentity mark
  ) => MonadWriteOnly mark w m
  where
    tell :: mark w -> m ()
    draft :: m a -> m (Pair (mark w) a)

class
  ( Monad m, MonadIdentity mark
  ) => MonadReadOnly mark r m
  where
    ask :: m (mark r)
    local :: (mark r -> mark r) -> m a -> m a
