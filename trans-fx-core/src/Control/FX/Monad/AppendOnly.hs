-- | Module      : Control.FX.Monad.AppendOnly
--   Description : Concrete append-only state monad
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

module Control.FX.Monad.AppendOnly (
    AppendOnly(..)
  , Context(..)
  , Input(..)
  , Output(..)
) where



import Data.Typeable (Typeable, typeOf)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad.Class



-- | Concrete append-only state monad
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
    fmap f x =
      x >>= (return . f)

instance
  ( Monoid w, MonadIdentity mark
  ) => Applicative (AppendOnly mark w)
  where
    pure
      :: a
      -> AppendOnly mark w a
    pure a = AppendOnly $ \_ ->
      Pair mempty a

    (<*>)
      :: AppendOnly mark w (a -> b)
      -> AppendOnly mark w a
      -> AppendOnly mark w b
    (AppendOnly f') <*> (AppendOnly x') =
      AppendOnly $ \w1 ->
        let Pair w2 f = f' w1 in
        let Pair w3 x = x' (w1 <> w2) in
        Pair (w2 <> w3) (f x)

instance
  ( Monoid w, MonadIdentity mark
  ) => Monad (AppendOnly mark w)
  where
    return
      :: a
      -> AppendOnly mark w a
    return a = AppendOnly $ \_ ->
      Pair mempty a

    (>>=)
      :: AppendOnly mark w a
      -> (a -> AppendOnly mark w b)
      -> AppendOnly mark w b
    (AppendOnly x') >>= f =
      AppendOnly $ \w1 ->
        let Pair w2 a = x' w1 in
        let Pair w3 b = unAppendOnly (f a) (w1 <> w2) in
        Pair (w2 <> w3) b



instance
  ( Monoid w, MonadIdentity mark
  ) => RunMonad (AppendOnly mark w)
  where
    newtype Input (AppendOnly mark w)
      = AppendOnlyIn
          { unAppendOnlyIn :: mark ()
          } deriving (Typeable)

    newtype Output (AppendOnly mark w) a
      = AppendOnlyOut
          { unAppendOnlyOut :: Pair (mark w) a
          } deriving (Typeable)

    run
      :: Input (AppendOnly mark w)
      -> AppendOnly mark w a
      -> Output (AppendOnly mark w) a
    run _ (AppendOnly x) = AppendOnlyOut $ bimap1 pure $ x mempty

deriving instance
  ( Eq (mark ())
  ) => Eq (Input (AppendOnly mark w))

deriving instance
  ( Show (mark ())
  ) => Show (Input (AppendOnly mark w))

deriving instance
  ( Eq (mark w), Eq a
  ) => Eq (Output (AppendOnly mark w) a)

deriving instance
  ( Show (mark w), Show a
  ) => Show (Output (AppendOnly mark w) a)



instance
  ( Eq w, Monoid w
  ) => EqIn (AppendOnly mark w)
  where
    newtype Context (AppendOnly mark w)
      = AppendOnlyCtx
          { unAppendOnlyCtx :: mark ()
          } deriving (Typeable)

    eqIn
      :: (Eq a)
      => Context (AppendOnly mark w)
      -> AppendOnly mark w a
      -> AppendOnly mark w a
      -> Bool
    eqIn _ (AppendOnly x) (AppendOnly y) =
      (x mempty) == (y mempty)

deriving instance
  ( Eq (mark ())
  ) => Eq (Context (AppendOnly mark w))

deriving instance
  ( Show (mark ())
  ) => Show (Context (AppendOnly mark w))





{- Effect Class -}

instance
  ( Monoid w, MonadIdentity mark
  ) => MonadAppendOnly mark w (AppendOnly mark w)
  where
    look
      :: AppendOnly mark w (mark w)
    look = AppendOnly $ \w ->
      Pair mempty (pure w)

    jot
      :: mark w
      -> AppendOnly mark w ()
    jot w = AppendOnly $ \_ ->
      Pair (unwrap w) ()
