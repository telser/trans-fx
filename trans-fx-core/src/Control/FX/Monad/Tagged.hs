{-#
  LANGUAGE
    InstanceSigs,
    KindSignatures,
    FlexibleInstances,
    MultiParamTypeClasses
#-}

module Control.FX.Monad.Tagged (
    Tagged(..)
  , runTagged

  , makeTag
) where

import Data.Typeable (Typeable)

import Control.FX.Functor
import Control.FX.Monad.Class

data Tagged
  (g :: *)
  (a :: *)
    = Tagged (Tag g) a
    deriving (Eq, Show, Typeable)


makeTag :: Pair g a -> Tagged g a
makeTag (Pair g a) = Tagged (Tag g) a

instance Functor (Tagged g) where
  fmap :: (a -> b) -> Tagged g a -> Tagged g b
  fmap f (Tagged g a) = Tagged g (f a)

instance Applicative (Tagged g) where
  pure :: a -> Tagged g a
  pure = Tagged Blank

  (Tagged g1 f) <*> (Tagged g2 x) =
    Tagged (g1 <> g2) (f x)

instance Monad (Tagged g) where
  return :: a -> Tagged g a
  return = Tagged Blank

  (Tagged g1 x) >>= f =
    let Tagged g2 y = f x in
    Tagged (g1 <> g2) y

instance Commutant (Tagged g) where
  commute
    :: ( Applicative f )
    => Tagged g (f a) -> f (Tagged g a)
  commute (Tagged g x) = fmap (Tagged g) x

instance Central (Tagged g)

instance RunMonad () (Tagged g) (Tagged g) where
  run :: () -> Tagged g a -> Tagged g a
  run () = id

runTagged :: Tagged g a -> Pair (Tag g) a
runTagged (Tagged g a) = Pair g a
