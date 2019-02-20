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
) where



-- | Class representing @Functor@s which "commute" with every
-- @Applicative@ in a precise sense. This looks a lot like
-- the @sequenceA@ function from @Traversable@, but that class
-- entails a bunch of extra technology that we don't really need.
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
