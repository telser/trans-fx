-- | Module      : Control.FX.Monad.Flip
--   Description : Concrete flipped composite monad
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Control.FX.Monad.Flip (
    Flip(..)
  , Input(..)
  , Output(..)
) where



import Data.Typeable (Typeable)
import Control.Applicative (liftA2)
import Control.Monad (join)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad.Class



-- | Concrete flipped composite monad
newtype Flip
  (c :: * -> *)
  (m :: * -> *)
  (a :: *)
    = Flip
        { unFlip :: m (c a)
        } deriving (Eq, Typeable)

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
  ( Monad m, Central c
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
  ( RunMonad m1, RunMonad m2, Central m1, Functor (Output m2)
  ) => RunMonad (Flip m1 m2)
  where
    data Input (Flip m1 m2)
      = FlipIn
          { unFlipIn :: (Input m1, Input m2)
          } deriving (Typeable)

    data Output (Flip m1 m2) a
      = FlipOut
          { unFlipOut :: Flip (Output m1) (Output m2) a
          } deriving (Typeable)

    run
      :: Input (Flip m1 m2)
      -> Flip m1 m2 a
      -> Output (Flip m1 m2) a
    run (FlipIn (z1,z2)) =
      FlipOut . Flip . fmap (run z1) . run z2 . unFlip

deriving instance
  ( Eq (Input m1), Eq (Input m2)
  ) => Eq (Input (Flip m1 m2))

deriving instance
  ( Show (Input m1), Show (Input m2)
  ) => Show (Input (Flip m1 m2))

deriving instance
  ( Eq (Output m2 (Output m1 a)), Eq (Output m1 a)
  ) => Eq (Output (Flip m1 m2) a)

deriving instance
  ( Show (Output m2 (Output m1 a)), Show (Output m1 a)
  ) => Show (Output (Flip m1 m2) a)
