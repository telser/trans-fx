{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Functor.Await (
    Await(..)
) where



import Data.Typeable (Typeable)

import Control.FX.Functor.Class



data Await
  (exp :: *)
  (a :: *)
    = Await
      { unAwait :: exp -> a
      } deriving (Typeable)

instance
  Functor (Await exp)
  where
    fmap
      :: (a -> b)
      -> Await exp a
      -> Await exp b
    fmap f (Await p) = Await (f . p)
