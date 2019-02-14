{-#
  LANGUAGE
    InstanceSigs,
    KindSignatures,
    FlexibleInstances,
    MultiParamTypeClasses
#-}

module Control.FX.Monad.State (
    State(..)
  , runState
) where

import Data.Typeable (Typeable)

import Control.FX.Functor
import Control.FX.Monad.Class

newtype State
  (k :: * -> *)
  (s :: *)
  (a :: *)
    = State
        { unState :: s -> Pair s a
        } deriving (Typeable)

instance Functor (State mark s) where
  fmap :: (a -> b) -> State mark s a -> State mark s b
  fmap f (State x) = State $ \s1 ->
    let Pair s2 a = x s1 in
    Pair s2 (f a)

instance Applicative (State mark s) where
  pure :: a -> State mar s a
  pure a = State $ \s -> Pair s a

  (<*>) :: State mark s (a -> b) -> State mark s a -> State mark s b
  (State f') <*> (State x') = State $ \s1 ->
    let Pair s2 f = f' s1 in
    let Pair s3 x = x' s2 in
    Pair s3 (f x)

instance Monad (State mark s) where
  return :: a -> State mark s a
  return a = State $ \s -> Pair s a

  (>>=) :: State mark s a -> (a -> State mark s b) -> State mark s b
  (State x') >>= f = State $ \s1 ->
    let Pair s2 x = x' s1 in
    (unState . f) x s2

instance RunMonad s (State mark s) (Pair s) where
  run s (State x) = x s

runState :: s -> State mark s a -> Pair s a
runState = run

instance
  ( MonadIdentity mark
  ) => MonadState mark s (State mark s)
  where
    get = State $ \s -> Pair s (pure s)

    put s = State $ \_ -> Pair (unwrap s) ()
