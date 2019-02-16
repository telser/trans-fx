{-#
  LANGUAGE
    InstanceSigs,
    KindSignatures,
    FlexibleInstances,
    QuantifiedConstraints,
    MultiParamTypeClasses
#-}

module Control.FX.Monad.Compose (
    Compose(..)
  , runCompose
) where

import Data.Typeable (Typeable)
import Control.Monad (join)
import Control.Applicative (liftA2)

import Control.FX.Functor
import Control.FX.Monad.Class

newtype Compose
  (m1 :: * -> *)
  (m2 :: * -> *)
  (a :: *)
    = Compose
        { unCompose :: m1 (m2 a)
        } deriving (Typeable)

instance
  ( Show a
  , forall u. (Show u) => Show (m1 u)
  , forall u. (Show u) => Show (m2 u)
  ) => Show (Compose m1 m2 a)
  where
    show (Compose x) = "(Compose " ++ show x ++ ")"

instance
  ( Functor m1, Functor m2
  ) => Functor (Compose m1 m2)
  where
    fmap f = Compose . fmap (fmap f) . unCompose

instance
  ( Applicative m1, Applicative m2
  ) => Applicative (Compose m1 m2)
  where
    pure = Compose . pure . pure

    (Compose f) <*> (Compose x) =
      Compose (liftA2 (<*>) f x)

instance
  ( Monad m1, Monad m2, Central m2
  ) => Monad (Compose m1 m2)
  where
    return = Compose . return . return

    (Compose x) >>= f =
      Compose . fmap join . join . fmap commute . fmap (fmap (unCompose . f)) $ x

instance
  ( Commutant c1, Commutant c2
  ) => Commutant (Compose c1 c2)
  where
    commute
      :: ( Applicative f )
      => Compose c1 c2 (f a) -> f (Compose c1 c2 a)
    commute = fmap Compose . commute . fmap commute . unCompose

instance
  ( Central c1, Central c2
  ) => Central (Compose c1 c2)

instance
  ( RunMonad z1 m1 f1, RunMonad z2 m2 f2, Central m2
  ) => RunMonad (z1,z2) (Compose m1 m2) (Compose f1 f2)
  where
    run :: (z1,z2) -> Compose m1 m2 a -> Compose f1 f2 a
    run (z1,z2) = Compose . fmap (run z2) . run z1 . unCompose

runCompose
  :: (RunMonad z1 m1 f1, RunMonad z2 m2 f2, Central m2)
  => z1 -> z2 -> Compose m1 m2 a -> f1 (f2 a)
runCompose z1 z2 x = unCompose $ run (z1,z2) x
