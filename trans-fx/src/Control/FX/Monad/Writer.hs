{-#
  LANGUAGE
    InstanceSigs,
    KindSignatures,
    FlexibleInstances,
    MultiParamTypeClasses
#-}

module Control.FX.Monad.Writer (
    Writer(..)
  , runWriter
) where

import Data.Typeable (Typeable)

import Control.FX.Functor
import Control.FX.Monad.Class

newtype Writer
  (k :: * -> *)
  (w :: *)
  (a :: *)
    = Writer
        { unWriter :: Pair w a
        } deriving (Show, Eq, Typeable)

instance (Monoid w) => Functor (Writer mark w) where
  fmap :: (a -> b) -> Writer mark w a -> Writer mark w b
  fmap f = Writer . fmap f . unWriter

instance (Monoid w) => Applicative (Writer mark w) where
  pure :: a -> Writer mark w a
  pure = Writer . Pair mempty

  (<*>) :: Writer mark w (a -> b) -> Writer mark w a -> Writer mark w b
  (Writer (Pair w1 f)) <*> (Writer (Pair w2 x)) =
    Writer (Pair (w1 <> w2) (f x))

instance (Monoid w) => Monad (Writer mark w) where
  return :: a -> Writer mark w a
  return = Writer . Pair mempty

  (>>=) :: Writer mark w a -> (a -> Writer mark w b) -> Writer mark w b
  (Writer (Pair w1 x)) >>= f =
    let Pair w2 y = unWriter $ f x in
    Writer $ Pair (w1 <> w2) y

instance (Monoid w) => Central (Writer mark w) where
  commute :: (Applicative f) => Writer mark w (f a) -> f (Writer mark w a)
  commute (Writer (Pair w x)) = fmap (\a -> (Writer (Pair w a))) x

instance (Monoid w) => RunMonad () (Writer mark w) (Pair w) where
  run () (Writer x) = x

runWriter :: (Monoid w) => Writer mark w a -> Pair w a
runWriter = run ()

instance
  ( Monoid w, MonadIdentity mark
  ) => MonadWriter mark w (Writer mark w)
  where
    tell w =
      Writer (Pair (unwrap w) ())

    draft (Writer (Pair w a)) =
      Writer (Pair mempty (Pair (pure w) a))
