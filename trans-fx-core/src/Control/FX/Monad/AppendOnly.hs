-- | Module      : Control.FX.Monad.AppendOnly
--   Description : Concrete append-only state monad
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.AppendOnly (
    AppendOnly(..)
) where



import Data.Typeable (Typeable, typeOf)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad.Class



-- | Concrete write-only state monad
newtype AppendOnly
  (mark :: * -> *)
  (w :: *)
  (a :: *)
    = AppendOnly
        { unAppendOnly :: w -> Pair w a
          -- ^ @f = unAppendOnly x@ must have the property that
          -- if @f w1 = Pair w2 a@, then there exists @w@ such that
          -- @w2 == w1 <> w@. This cannot be enforced by the type,
          -- but the class instance methods for @AppendOnly@ all
          -- preserve it.
        } deriving (Typeable)

instance
  ( Eq w, Eq a, Monoid w
  ) => EqIn (mark ()) (AppendOnly mark w a)
  where
    eqIn
      :: mark ()
      -> AppendOnly mark w a
      -> AppendOnly mark w a
      -> Bool
    eqIn _ (AppendOnly x) (AppendOnly y) =
      (x mempty) == (y mempty)

instance
  ( Typeable w, Typeable a, Typeable mark
  ) => Show (AppendOnly mark w a)
  where
    show
      :: AppendOnly mark w a
      -> String
    show = show . typeOf

instance
  ( Monoid w, MonadIdentity mark
  ) => Functor (AppendOnly mark w)
  where
    fmap
      :: (a -> b)
      -> AppendOnly mark w a
      -> AppendOnly mark w b
    fmap f (AppendOnly x) = AppendOnly $ \w1 ->
      let Pair w2 a = x w1 in
      Pair (w1 <> w2) (f a)

instance
  ( Monoid w, MonadIdentity mark
  ) => Applicative (AppendOnly mark w)
  where
    pure
      :: a
      -> AppendOnly mark w a
    pure a = AppendOnly $ \w -> Pair w a

    (<*>)
      :: AppendOnly mark w (a -> b)
      -> AppendOnly mark w a
      -> AppendOnly mark w b
    (AppendOnly f') <*> (AppendOnly x') =
      AppendOnly $ \w1 ->
        let Pair w2 f = f' w1 in
        let Pair w3 x = x' w2 in
        Pair w3 (f x)

instance
  ( Monoid w, MonadIdentity mark
  ) => Monad (AppendOnly mark w)
  where
    return
      :: a
      -> AppendOnly mark w a
    return a = AppendOnly $ \w -> Pair w a

    (>>=)
      :: AppendOnly mark w a
      -> (a -> AppendOnly mark w b)
      -> AppendOnly mark w b
    (AppendOnly x') >>= f =
      AppendOnly $ \w1 ->
        let Pair w2 a = x' w1 in
        let Pair w3 b = unAppendOnly (f a) w2 in
        Pair w3 b



instance
  ( Monoid w, MonadIdentity mark
  ) => RunMonad (mark ()) (AppendOnly mark w) (Pair (mark w))
  where
    run
      :: mark ()
      -> AppendOnly mark w a
      -> Pair (mark w) a
    run _ (AppendOnly x) = bimap1 pure $ x mempty



{- Effect Class -}

instance
  ( Monoid w, MonadIdentity mark
  ) => MonadAppendOnly mark w (AppendOnly mark w)
  where
    look
      :: AppendOnly mark w (mark w)
    look = AppendOnly $ \w ->
      Pair w (pure w)

    jot
      :: mark w
      -> AppendOnly mark w ()
    jot w =
      AppendOnly $ \w1 -> Pair (w1 <> (unwrap w)) ()
