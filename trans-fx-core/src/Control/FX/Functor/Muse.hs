{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Functor.Muse (
    Muse(..)
  , liftMuseT
) where



import Data.Typeable (Typeable)

import Control.FX.EqIn
import Control.FX.Functor.Class



data Muse
  (f :: * -> *)
  (m :: * -> *)
  (a :: *)
    = Idea a
    | Muse (f (m a))
    deriving (Typeable)

instance
  ( Functor f, Monad m
  ) => Functor (Muse f m)
  where
    fmap
      :: (a -> b)
      -> Muse f m a
      -> Muse f m b
    fmap f x = case x of
      Idea a -> Idea (f a)
      Muse z -> Muse $ fmap (fmap f) z

liftMuseT
  :: ( Functor f )
  => (t m a -> u t m a)
  -> Muse f (t m) a
  -> Muse f (u t m) a
liftMuseT lift x = case x of
  Idea a -> Idea a
  Muse z -> Muse $ fmap lift z
