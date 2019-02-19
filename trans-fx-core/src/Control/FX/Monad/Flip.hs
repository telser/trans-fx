-- | Module      : Control.FX.Monad.Flip
--   Description : Concrete flipped composite monad
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

module Control.FX.Monad.Flip (
    Flip(..)
  , runFlip
) where



import Data.Typeable (Typeable)
import Control.Applicative (liftA2)
import Control.Monad (join)

import Control.FX.Functor
import Control.FX.Monad.Class



-- | Concrete flipped composite monad
newtype Flip
  (c :: * -> *)
  (m :: * -> *)
  (a :: *)
    = Flip
        { unFlip :: m (c a)
        } deriving (Typeable)

deriving instance
  ( Show (m (c a))
  ) => Show (Flip c m a)

instance
  ( Functor m, Functor c
  ) => Functor (Flip c m)
  where
    fmap
      :: (a -> b)
      -> Flip c m a
      -> Flip c m b
    fmap f = Flip . fmap (fmap f) . unFlip

instance
  ( Applicative m, Applicative c
  ) => Applicative (Flip c m)
  where
    pure
      :: a
      -> Flip c m a
    pure = Flip . pure . pure

    (<*>)
      :: Flip c m (a -> b)
      -> Flip c m a
      -> Flip c m b
    (Flip f) <*> (Flip x) =
      Flip (liftA2 (<*>) f x)

instance
  ( Monad m, Monad c, Central c
  ) => Monad (Flip c m)
  where
    return
      :: a
      -> Flip c m a
    return = Flip . return . return

    (>>=)
      :: Flip c m a
      -> (a -> Flip c m b)
      -> Flip c m b
    (Flip x) >>= f =
      Flip . fmap join . join . fmap commute . fmap (fmap (unFlip . f)) $ x

instance
  ( Commutant c1, Commutant c2
  ) => Commutant (Flip c1 c2)
  where
    commute
      :: ( Applicative f )
      => Flip c1 c2 (f a)
      -> f (Flip c1 c2 a)
    commute = fmap Flip . commute . fmap commute . unFlip

instance
  ( Central c1, Central c2
  ) => Central (Flip c1 c2)

instance
  ( RunMonad z1 m1 f1, RunMonad z2 m2 f2, Central m1
  ) => RunMonad (z1,z2) (Flip m1 m2) (Flip f1 f2)
  where
    run
      :: (z1,z2)
      -> Flip m1 m2 a
      -> Flip f1 f2 a
    run (z1,z2) = Flip . fmap (run z1) . run z2 . unFlip

-- | Run a flipped composite monad computation in context
runFlip
  :: (RunMonad z1 m1 f1, RunMonad z2 m2 f2, Central m1)
  => z1
  -> z2
  -> Flip m1 m2 a
  -> f2 (f1 a)
runFlip z1 z2 x = unFlip $ run (z1,z2) x
