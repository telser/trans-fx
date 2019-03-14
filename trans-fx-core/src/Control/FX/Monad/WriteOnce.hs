-- | Module      : Control.FX.Monad.WriteOnce
--   Description : Concrete write-once state monad
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

module Control.FX.Monad.WriteOnce (
    WriteOnce(..)
  , Context(..)
  , Input(..)
  , Output(..)
) where



import Data.Typeable (Typeable, typeOf)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad.Class



-- | Concrete write-once state monad
newtype WriteOnce
  (mark :: * -> *)
  (w :: *)
  (a :: *)
    = WriteOnce
        { unWriteOnce :: LeftZero w -> Pair (LeftZero w) a
        } deriving (Typeable)

instance
  ( Typeable w, Typeable a, Typeable mark
  ) => Show (WriteOnce mark w a)
  where
    show
      :: WriteOnce mark w a
      -> String
    show = show . typeOf

instance
  ( MonadIdentity mark
  ) => Functor (WriteOnce mark w)
  where
    fmap
      :: (a -> b)
      -> WriteOnce mark w a
      -> WriteOnce mark w b
    fmap f x = x >>= (return . f)

instance
  ( MonadIdentity mark
  ) => Applicative (WriteOnce mark w)
  where
    pure
      :: a
      -> WriteOnce mark w a
    pure a = WriteOnce $ \w -> Pair w a

    (<*>)
      :: WriteOnce mark w (a -> b)
      -> WriteOnce mark w a
      -> WriteOnce mark w b
    (WriteOnce f') <*> (WriteOnce x') =
      WriteOnce $ \w1 ->
        let Pair w2 f = f' w1 in
        let Pair w3 x = x' (w1 <> w2) in
        Pair (w2 <> w3) (f x)

instance
  ( MonadIdentity mark
  ) => Monad (WriteOnce mark w)
  where
    return
      :: a
      -> WriteOnce mark w a
    return a = WriteOnce $ \w -> Pair w a

    (>>=)
      :: WriteOnce mark w a
      -> (a -> WriteOnce mark w b)
      -> WriteOnce mark w b
    (WriteOnce x') >>= f =
      WriteOnce $ \w1 ->
        let Pair w2 a = x' w1 in
        let Pair w3 b = unWriteOnce (f a) (w1 <> w2) in
        Pair (w2 <> w3) b



instance
  ( Eq w, Monoid w
  ) => EqIn (WriteOnce mark w)
  where
    newtype Context (WriteOnce mark w)
      = WriteOnceCtx
          { unWriteOnceCtx :: mark ()
          } deriving (Typeable)

    eqIn
      :: (Eq a)
      => Context (WriteOnce mark w)
      -> WriteOnce mark w a
      -> WriteOnce mark w a
      -> Bool
    eqIn _ (WriteOnce x) (WriteOnce y) =
      (x mempty) == (y mempty)

deriving instance
  ( Eq (mark ())
  ) => Eq (Context (WriteOnce mark w))

deriving instance
  ( Show (mark ())
  ) => Show (Context (WriteOnce mark w))



instance
  ( MonadIdentity mark
  ) => RunMonad (WriteOnce mark w)
  where
    newtype Input (WriteOnce mark w)
      = WriteOnceIn
          { unWriteOnceIn :: mark ()
          } deriving (Typeable)

    newtype Output (WriteOnce mark w) a
      = WriteOnceOut
          { unWriteOnceOut :: Pair (mark (Maybe w)) a
          } deriving (Typeable)

    run
      :: Input (WriteOnce mark w)
      -> WriteOnce mark w a
      -> Output (WriteOnce mark w) a
    run _ (WriteOnce x) = WriteOnceOut $ bimap1 (pure . toMaybe) $ x mempty

deriving instance
  ( Eq (mark ())
  ) => Eq (Input (WriteOnce mark w))

deriving instance
  ( Show (mark ())
  ) => Show (Input (WriteOnce mark w))

deriving instance
  ( Eq (mark (Maybe w)), Eq a
  ) => Eq (Output (WriteOnce mark w) a)

deriving instance
  ( Show (mark (Maybe w)), Show a
  ) => Show (Output (WriteOnce mark w) a)





{- Effect Class -}

instance
  ( MonadIdentity mark
  ) => MonadWriteOnce mark w (WriteOnce mark w)
  where
    press
      :: WriteOnce mark w (Maybe (mark w))
    press = WriteOnce $ \w ->
      Pair mempty (fmap pure $ toMaybe w)

    etch
      :: mark w
      -> WriteOnce mark w Bool
    etch w =
      WriteOnce $ \w1 ->
        case w1 of
          LeftUnit   -> Pair (LeftZero $ unwrap w) True
          LeftZero _ -> Pair mempty False
