{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Functor.Muse (
    Muse(..)
  , Thunk(..)
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



newtype Thunk
  (mark :: * -> *)
  (sus :: * -> *)
  (m :: * -> *)
  (a :: *)
    = Thunk
        { unThunk :: m a
        } deriving (Typeable)
