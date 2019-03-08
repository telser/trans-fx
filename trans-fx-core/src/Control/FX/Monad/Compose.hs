-- | Module      : Control.FX.Monad.Compose
--   Description : Concrete composite monad
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
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Control.FX.Monad.Compose (
    Compose(..)
  , Input(..)
  , Output(..)
) where



import Data.Typeable (Typeable)
import Control.Monad (join)
import Control.Applicative (liftA2)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad.Class



-- | Concrete composite monad
newtype Compose
  (m1 :: * -> *)
  (m2 :: * -> *)
  (a :: *)
    = Compose
        { unCompose :: m1 (m2 a)
        } deriving (Eq, Typeable)

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
  ( RunMonad m1, RunMonad m2, Central m2, Functor (Output m1)
  ) => RunMonad (Compose m1 m2)
  where
    newtype Input (Compose m1 m2)
      = ComposeIn
          { unComposeIn :: (Input m1, Input m2)
          } deriving (Typeable)

    newtype Output (Compose m1 m2) a
      = ComposeOut
          { unComposeOut :: Compose (Output m1) (Output m2) a
          } deriving (Typeable)

    run
      :: Input (Compose m1 m2)
      -> Compose m1 m2 a
      -> Output (Compose m1 m2) a
    run (ComposeIn (z1,z2)) =
      ComposeOut . Compose . fmap (run z2) . run z1 . unCompose

deriving instance
  ( Show (Input m1), Show (Input m2)
  ) => Show (Input (Compose m1 m2))

deriving instance
  ( Eq (Input m1), Eq (Input m2)
  ) => Eq (Input (Compose m1 m2))

deriving instance
  ( Show (Output m1 (Output m2 a)), Show (Output m2 a)
  ) => Show (Output (Compose m1 m2) a)

deriving instance
  ( Eq (Output m1 (Output m2 a)), Eq (Output m2 a)
  ) => Eq (Output (Compose m1 m2) a)
