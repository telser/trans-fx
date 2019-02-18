-- | Module      : Control.FX.Functor.Class
--   Description : Functor subclasses
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE InstanceSigs #-}

module Control.FX.Functor.Class (
    Commutant(..)
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

instance Commutant Maybe where
  commute
    :: ( Applicative f )
    => Maybe (f a) -> f (Maybe a)
  commute x = case x of
    Nothing -> pure Nothing
    Just m  -> fmap Just m

instance Commutant (Either e) where
  commute
    :: ( Applicative f )
    => Either e (f a) -> f (Either e a)
  commute x = case x of
    Left e  -> pure (Left e)
    Right m -> fmap Right m
