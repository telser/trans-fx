-- | Module      : Control.FX.Functor.Class
--   Description : Functor subclasses
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE InstanceSigs   #-}
{-# LANGUAGE KindSignatures #-}

module Control.FX.Functor.Class (
    Commutant(..)
  , Bifunctor(..)

  , Wrap(..)
  , Renaming(..)
) where



-- | Class representing @Functor@s which "commute" with every
-- @Applicative@ in a precise sense. Instances should satisfy
-- the following law:
--
-- > (1) commute . fmap pure  ===  pure
--
-- This looks a lot like the @sequenceA@ function from @Traversable@,
-- but that class entails a bunch of extra technology that we don't
-- really need.
--
-- The motivation for @Commutant@ comes from the observation that
-- most useful monads can be /run/ to produce a "value", though in
-- general that value will depend on some other context. In every
-- case I've tried so far that context is a @Commutant@ functor,
-- which is enough to make a generic @RunMonad@ instance for
-- @Compose@.
class
  ( Functor d
  ) => Commutant d
  where
    commute
      :: (Applicative f)
      => d (f a) -> f (d a)

instance
  Commutant Maybe
  where
    commute
      :: ( Applicative f )
      => Maybe (f a) -> f (Maybe a)
    commute x = case x of
      Nothing -> pure Nothing
      Just m  -> fmap Just m

instance
  Commutant (Either e)
  where
    commute
      :: ( Applicative f )
      => Either e (f a) -> f (Either e a)
    commute x = case x of
      Left e  -> pure (Left e)
      Right m -> fmap Right m



-- | Class representing bifunctors on the category of types.
-- Instances should satisfy the following laws:
--
-- > (1) bimap1 id  ===  id
-- >
-- > (2) bimap1 (f . g)  ===  bimap1 f . bimap1 g
-- >
-- > (3) bimap2 id  ===  id
-- >
-- > (4) bimap2 (f . g)  ===  bimap2 f . bimap2 g
-- >
-- > (5) bimap1 f . bimap2 g  ===  bimap2 g . bimap1 f
class
  Bifunctor (f :: * -> * -> *)
  where
    -- | @fmap@ in the "first" component
    bimap1 :: (a -> c) -> f a b -> f c b

    -- | @fmap@ in the "second" component
    bimap2 :: (b -> c) -> f a b -> f a c

instance
  Bifunctor Either
  where
    bimap1
      :: (a -> c)
      -> Either a b
      -> Either c b
    bimap1 f x = case x of
      Left a -> Left (f a)
      Right b -> Right b

    bimap2
      :: (b -> c)
      -> Either a b
      -> Either a c
    bimap2 f x = case x of
      Left a -> Left a
      Right b -> Right (f b)

instance
  Bifunctor (,)
  where
    bimap1
      :: (a -> c)
      -> (a,b)
      -> (c,b)
    bimap1 f (a,b) = (f a, b)

    bimap2
      :: (b -> c)
      -> (a,b)
      -> (a,c)
    bimap2 f (a,b) = (a, f b)





newtype Wrap f a = Wrap
  { unWrap :: f a }

class Renaming f where
  namingMap :: a -> f a
  namingInv :: f a -> a

instance
  ( Renaming f
  ) => Functor (Wrap f)
  where
    fmap f =
      Wrap . namingMap . f . namingInv . unWrap

instance
  ( Renaming f
  ) => Applicative (Wrap f)
  where
    pure = Wrap . namingMap

    f <*> x =
      Wrap $ namingMap $
        (namingInv $ unWrap f)
        (namingInv $ unWrap x)

instance
  ( Renaming f
  ) => Monad (Wrap f)
  where
    return = Wrap . namingMap

    x >>= f =
      f (namingInv $ unWrap x)

instance
  ( Renaming f, Eq a
  ) => Eq (Wrap f a)
  where
    x == y =
      (namingInv $ unWrap x) == (namingInv $ unWrap y)

instance
  ( Renaming f, Semigroup a
  ) => Semigroup (Wrap f a)
  where
    x <> y = Wrap . namingMap $
      (namingInv $ unWrap x) <> (namingInv $ unWrap y)

instance
  ( Renaming f, Monoid a
  ) => Monoid (Wrap f a)
  where
    mempty = Wrap $ namingMap mempty
