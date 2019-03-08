-- | Module      : Control.FX.Monad.Apply
--   Description : Concrete application monad
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.Apply (
    Apply(..)
  , Context(..)
  , Input(..)
  , Output(..)
) where



import Data.Typeable (Typeable)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad.Class



-- | Concrete application monad
newtype Apply
  (m :: * -> *)
  (a :: *)
    = Apply
        { unApply :: m a
        } deriving (Typeable)

deriving instance
  ( Show (m a)
  ) => Show (Apply m a)

instance
  ( Functor m
  ) => Functor (Apply m)
  where
    fmap
      :: (a -> b)
      -> Apply m a
      -> Apply m b
    fmap f (Apply x) = Apply (fmap f x)

instance
  ( Applicative m
  ) => Applicative (Apply m)
  where
    pure
      :: a
      -> Apply m a
    pure = Apply . pure

    (<*>)
      :: Apply m (a -> b)
      -> Apply m a
      -> Apply m b
    (Apply f) <*> (Apply x) =
      Apply (f <*> x)

instance
  ( Monad m
  ) => Monad (Apply m)
  where
    return
      :: a
      -> Apply m a
    return = Apply . return

    (>>=)
      :: Apply m a
      -> (a -> Apply m b)
      -> Apply m b
    (Apply x) >>= f =
      Apply (x >>= (unApply . f))

instance
  ( Commutant c
  ) => Commutant (Apply c)
  where
    commute
      :: ( Applicative f )
      => Apply c (f a)
      -> f (Apply c a)
    commute = fmap Apply . commute . unApply

instance
  ( Central c
  ) => Central (Apply c)





instance
  ( EqIn m
  ) => EqIn (Apply m)
  where
    newtype Context (Apply m)
      = ApplyCtx
          { unApplyCtx :: Context m
          } deriving (Typeable)

    eqIn
      :: ( Eq a )
      => Context (Apply m)
      -> Apply m a
      -> Apply m a
      -> Bool
    eqIn (ApplyCtx h) (Apply x) (Apply y) =
      eqIn h x y

deriving instance
  ( Show (Context m)
  ) => Show (Context (Apply m))

deriving instance
  ( Eq (Context m)
  ) => Eq (Context (Apply m))



instance
  ( RunMonad m
  ) => RunMonad (Apply m)
  where
    newtype Input (Apply m)
      = ApplyIn
          { unApplyIn :: Input m
          } deriving (Typeable)

    newtype Output (Apply m) a
      = ApplyOut
          { unApplyOut :: Output m a
          } deriving (Typeable)

    run
      :: Input (Apply m)
      -> Apply m a
      -> Output (Apply m) a
    run (ApplyIn z) (Apply x) = ApplyOut $ run z x

deriving instance
  ( Show (Input m)
  ) => Show (Input (Apply m))

deriving instance
  ( Eq (Input m)
  ) => Eq (Input (Apply m))

deriving instance
  ( Show (Output m a)
  ) => Show (Output (Apply m) a)

deriving instance
  ( Eq (Output m a)
  ) => Eq (Output (Apply m) a)
