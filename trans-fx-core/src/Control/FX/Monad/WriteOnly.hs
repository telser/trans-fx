-- | Module      : Control.FX.Monad.WriteOnly
--   Description : Concrete write-only state monad
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.WriteOnly (
    WriteOnly(..)
  , runWriteOnly
) where



import Data.Typeable (Typeable)

import Control.FX.Functor
import Control.FX.Monad.Class



-- | Concrete write-only state monad
newtype WriteOnly
  (k :: * -> *)
  (w :: *)
  (a :: *)
    = WriteOnly
        { unWriteOnly :: Pair w a
        } deriving (Show, Eq, Typeable)

instance
  ( Monoid w
  ) => Functor (WriteOnly mark w)
  where
    fmap
      :: (a -> b)
      -> WriteOnly mark w a
      -> WriteOnly mark w b
    fmap f = WriteOnly . fmap f . unWriteOnly

instance
  ( Monoid w
  ) => Applicative (WriteOnly mark w)
  where
    pure
      :: a
      -> WriteOnly mark w a
    pure = WriteOnly . Pair mempty

    (<*>)
      :: WriteOnly mark w (a -> b)
      -> WriteOnly mark w a
      -> WriteOnly mark w b
    (WriteOnly (Pair w1 f)) <*> (WriteOnly (Pair w2 x)) =
      WriteOnly (Pair (w1 <> w2) (f x))

instance
  ( Monoid w
  ) => Monad (WriteOnly mark w)
  where
    return
      :: a
      -> WriteOnly mark w a
    return = WriteOnly . Pair mempty

    (>>=)
      :: WriteOnly mark w a
      -> (a -> WriteOnly mark w b)
      -> WriteOnly mark w b
    (WriteOnly (Pair w1 x)) >>= f =
      let Pair w2 y = unWriteOnly $ f x in
      WriteOnly $ Pair (w1 <> w2) y

instance
  ( Monoid w
  ) => Commutant (WriteOnly mark w)
  where
    commute
      :: ( Applicative f )
      => WriteOnly mark w (f a)
      -> f (WriteOnly mark w a)
    commute (WriteOnly (Pair w x)) =
      fmap (\a -> (WriteOnly (Pair w a))) x

instance
  ( Monoid w
  ) => Central (WriteOnly mark w)



instance
  ( Monoid w
  ) => RunMonad () (WriteOnly mark w) (Pair w)
  where
    run
      :: ()
      -> WriteOnly mark w a
      -> Pair w a
    run () (WriteOnly x) = x

-- | Run a @WriteOnly mark w a@, producing a @Pair w a@.
runWriteOnly
  :: ( Monoid w )
  => WriteOnly mark w a
  -> Pair w a
runWriteOnly = run ()

instance
  ( Monoid w, MonadIdentity mark
  ) => MonadWriteOnly mark w (WriteOnly mark w)
  where
    tell
      :: mark w
      -> WriteOnly mark w ()
    tell w =
      WriteOnly (Pair (unwrap w) ())

    draft
      :: WriteOnly mark w a
      -> WriteOnly mark w (Pair (mark w) a)
    draft (WriteOnly (Pair w a)) =
      WriteOnly (Pair mempty (Pair (pure w) a))
