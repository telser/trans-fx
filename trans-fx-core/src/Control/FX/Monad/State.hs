-- | Module      : Control.FX.Monad.State
--   Description : Concrete state monad
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.State (
    State(..)
  , runState
) where



import Data.Typeable (Typeable)

import Control.FX.Functor
import Control.FX.Monad.Class



-- | Concrete state monad
newtype State
  (mark :: * -> *)
  (s :: *)
  (a :: *)
    = State
        { unState :: s -> Pair s a
        } deriving (Typeable)

instance Functor (State mark s) where
  fmap
    :: (a -> b)
    -> State mark s a
    -> State mark s b
  fmap f (State x) = State $ \s1 ->
    let Pair s2 a = x s1 in
    Pair s2 (f a)

instance Applicative (State mark s) where
  pure
    :: a
    -> State mar s a
  pure a = State $ \s -> Pair s a

  (<*>)
    :: State mark s (a -> b)
    -> State mark s a
    -> State mark s b
  (State f') <*> (State x') = State $ \s1 ->
    let Pair s2 f = f' s1 in
    let Pair s3 x = x' s2 in
    Pair s3 (f x)

instance Monad (State mark s) where
  return
    :: a
    -> State mark s a
  return a = State $ \s -> Pair s a

  (>>=)
    :: State mark s a
    -> (a -> State mark s b)
    -> State mark s b
  (State x') >>= f = State $ \s1 ->
    let Pair s2 x = x' s1 in
    (unState . f) x s2

instance
  ( MonadIdentity mark
  ) => RunMonad (mark s) (State mark s) (Pair (mark s)) where
  run
    :: mark s
    -> State mark s a
    -> Pair (mark s) a
  run s (State x) =
    let Pair s1 a = x (unwrap s)
    in Pair (return s1) a



-- | Run a @State mark s a@ with an initial state @s@, producing a @Pair s a@.
runState
  :: ( MonadIdentity mark )
  => mark s
  -> State mark s a
  -> Pair (mark s) a
runState = run

instance
  ( MonadIdentity mark
  ) => MonadState mark s (State mark s)
  where
    get
      :: State mark s (mark s)
    get = State $ \s -> Pair s (pure s)

    put
      :: mark s
      -> State mark s ()
    put s = State $ \_ -> Pair (unwrap s) ()
