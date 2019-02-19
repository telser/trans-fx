-- | Module      : Control.FX.Monad.Compose
--   Description : Concrete composite monad
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.Compose (
    Compose(..)
  , runCompose
) where



import Data.Typeable (Typeable)
import Control.Monad (join)
import Control.Applicative (liftA2)

import Control.FX.Functor
import Control.FX.Monad.Class



-- | Concrete composite monad
newtype Compose
  (m1 :: * -> *)
  (m2 :: * -> *)
  (a :: *)
    = Compose
        { unCompose :: m1 (m2 a)
        } deriving (Typeable)

deriving instance
  ( Show (m1 (m2 a))
  ) => Show (Compose m1 m2 a)

instance
  ( Functor m1, Functor m2
  ) => Functor (Compose m1 m2)
  where
    fmap
      :: (a -> b)
      -> Compose m1 m2 a
      -> Compose m1 m2 b
    fmap f = Compose . fmap (fmap f) . unCompose

instance
  ( Applicative m1, Applicative m2
  ) => Applicative (Compose m1 m2)
  where
    pure
      :: a
      -> Compose m1 m2 a
    pure = Compose . pure . pure

    (<*>)
      :: Compose m1 m2 (a -> b)
      -> Compose m1 m2 a
      -> Compose m1 m2 b
    (Compose f) <*> (Compose x) =
      Compose (liftA2 (<*>) f x)

instance
  ( Monad m1, Monad m2, Central m2
  ) => Monad (Compose m1 m2)
  where
    return
      :: a
      -> Compose m1 m2 a
    return = Compose . return . return

    (>>=)
      :: Compose m1 m2 a
      -> (a -> Compose m1 m2 b)
      -> Compose m1 m2 b
    (Compose x) >>= f =
      Compose . fmap join . join . fmap commute . fmap (fmap (unCompose . f)) $ x

instance
  ( Commutant c1, Commutant c2
  ) => Commutant (Compose c1 c2)
  where
    commute
      :: ( Applicative f )
      => Compose c1 c2 (f a)
      -> f (Compose c1 c2 a)
    commute = fmap Compose . commute . fmap commute . unCompose

instance
  ( Central c1, Central c2
  ) => Central (Compose c1 c2)

instance
  ( RunMonad z1 m1 f1, RunMonad z2 m2 f2, Central m2
  ) => RunMonad (z1,z2) (Compose m1 m2) (Compose f1 f2)
  where
    run
      :: (z1,z2)
      -> Compose m1 m2 a
      -> Compose f1 f2 a
    run (z1,z2) = Compose . fmap (run z2) . run z1 . unCompose

-- | Run a composite monadic computation
runCompose
  :: ( RunMonad z1 m1 f1, RunMonad z2 m2 f2, Central m2 )
  => z1
  -> z2
  -> Compose m1 m2 a
  -> f1 (f2 a)
runCompose z1 z2 x = unCompose $ run (z1,z2) x
