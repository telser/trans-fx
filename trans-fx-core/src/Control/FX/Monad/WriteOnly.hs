-- | Module      : Control.FX.Monad.WriteOnly
--   Description : Concrete write-only state monad
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.WriteOnly (
    WriteOnly(..)
  , Context(..)
  , Input(..)
  , Output(..)
) where



import Data.Typeable (Typeable)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad.Class



-- | Concrete write-only state monad with state type @w@
newtype WriteOnly
  (mark :: * -> *)
  (w :: *)
  (a :: *)
    = WriteOnly
        { unWriteOnly :: Pair w a
        } deriving (Eq, Show, Typeable)



instance
  ( Monoid w, MonadIdentity mark
  ) => Functor (WriteOnly mark w)
  where
    fmap
      :: (a -> b)
      -> WriteOnly mark w a
      -> WriteOnly mark w b
    fmap f = WriteOnly . fmap f . unWriteOnly

instance
  ( Monoid w, MonadIdentity mark
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
  ( Monoid w, MonadIdentity mark
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
  ( Monoid w, MonadIdentity mark
  ) => Commutant (WriteOnly mark w)
  where
    commute
      :: ( Applicative f )
      => WriteOnly mark w (f a)
      -> f (WriteOnly mark w a)
    commute (WriteOnly (Pair w x)) =
      fmap (\a -> (WriteOnly (Pair w a))) x

instance
  ( MonadIdentity mark
  ) => Bifunctor (WriteOnly mark)
  where
    bimap1
      :: (w -> v)
      -> WriteOnly mark w a
      -> WriteOnly mark v a
    bimap1 f = WriteOnly . bimap1 f . unWriteOnly

    bimap2
      :: (a -> b)
      -> WriteOnly mark w a
      -> WriteOnly mark w b
    bimap2 f = WriteOnly . bimap2 f . unWriteOnly

instance
  ( Monoid w, MonadIdentity mark
  ) => Central (WriteOnly mark w)





instance
  ( Eq w
  ) => EqIn (WriteOnly mark w)
  where
    newtype Context (WriteOnly mark w)
      = WriteOnlyCtx
          { unWriteOnlyCtx :: mark ()
          } deriving (Typeable)

    eqIn
      :: (Eq a)
      => Context (WriteOnly mark w)
      -> WriteOnly mark w a
      -> WriteOnly mark w a
      -> Bool
    eqIn _ = (==)

deriving instance
  ( Eq (mark ())
  ) => Eq (Context (WriteOnly mark w))

deriving instance
  ( Show (mark ())
  ) => Show (Context (WriteOnly mark w))



instance
  ( Monoid w, MonadIdentity mark
  ) => RunMonad (WriteOnly mark w)
  where
    newtype Input (WriteOnly mark w)
      = WriteOnlyIn
          { unWriteOnlyIn :: mark ()
          } deriving (Typeable)

    newtype Output (WriteOnly mark w) a
      = WriteOnlyOut
          { unWriteOnlyOut :: Pair (mark w) a
          } deriving (Typeable)

    run
      :: Input (WriteOnly mark w)
      -> WriteOnly mark w a
      -> Output (WriteOnly mark w) a
    run _ (WriteOnly (Pair w a)) =
      WriteOnlyOut $ Pair (pure w) a

deriving instance
  ( Eq (mark ())
  ) => Eq (Input (WriteOnly mark w))

deriving instance
  ( Show (mark ())
  ) => Show (Input (WriteOnly mark w))

deriving instance
  ( Eq (mark w), Eq a
  ) => Eq (Output (WriteOnly mark w) a)

deriving instance
  ( Show (mark w), Show a
  ) => Show (Output (WriteOnly mark w) a)



{- Effect Class -}

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
