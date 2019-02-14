{-#
  LANGUAGE
    InstanceSigs,
    KindSignatures,
    FlexibleInstances,
    QuantifiedConstraints,
    MultiParamTypeClasses
#-}

module Control.FX.Monad.Flip (
    Flip(..)
  , runFlip
) where

import Data.Typeable (Typeable)
import Control.Applicative (liftA2)
import Control.Monad (join)

import Control.FX.Functor
import Control.FX.Monad.Class

newtype Flip
  (c :: * -> *)
  (m :: * -> *)
  (a :: *)
    = Flip
        { unFlip :: m (c a)
        } deriving (Typeable)

instance
  ( Show a
  , forall u. (Show u) => Show (m u)
  , forall u. (Show u) => Show (c u)
  ) => Show (Flip c m a)
  where
    show (Flip x) = "(Flip " ++ show x ++ ")"

instance
  ( Functor m, Functor c
  ) => Functor (Flip c m)
  where
    fmap :: (a -> b) -> Flip c m a -> Flip c m b
    fmap f = Flip . fmap (fmap f) . unFlip

instance
  ( Applicative m, Applicative c
  ) => Applicative (Flip c m)
  where
    pure = Flip . pure . pure

    (Flip f) <*> (Flip x) =
      Flip (liftA2 (<*>) f x)

instance
  ( Monad m, Monad c, Central c
  ) => Monad (Flip c m)
  where
    return :: a -> Flip c m a
    return = Flip . return . return

    (Flip x) >>= f =
      Flip . fmap join . join . fmap commute . fmap (fmap (unFlip . f)) $ x

instance
  ( Functor c1, Functor c2, Central c1, Central c2
  ) => Central (Flip c1 c2)
  where
    commute :: (Applicative f) => Flip c1 c2 (f a) -> f (Flip c1 c2 a)
    commute = fmap Flip . commute . fmap commute . unFlip

instance
  ( RunMonad z1 m1 f1, RunMonad z2 m2 f2, Central m1
  ) => RunMonad (z1,z2) (Flip m1 m2) (Flip f1 f2)
  where
    run :: (z1,z2) -> Flip m1 m2 a -> Flip f1 f2 a
    run (z1,z2) = Flip . fmap (run z1) . run z2 . unFlip

runFlip
  :: (RunMonad z1 m1 f1, RunMonad z2 m2 f2, Central m1)
  => z1 -> z2 -> Flip m1 m2 a -> f2 (f1 a)
runFlip z1 z2 x = unFlip $ run (z1,z2) x
