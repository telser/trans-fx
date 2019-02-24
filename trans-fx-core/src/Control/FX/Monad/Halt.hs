-- | Module      : Control.FX.Monad.Except
--   Description : Concrete halt monad
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.Halt (
    Halt(..)
) where



import Data.Typeable (Typeable)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad.Class



-- | Concrete exception monad, throwing marked exceptions of type
-- @mark e@ and producing values of type @a@
data Halt
  (mark :: * -> *)
  (a :: *)
    = Step a -- ^ Proceed
    | Halt   -- ^ Bail out
    deriving (Eq, Show, Typeable)

instance
  ( Eq a
  ) => EqIn (mark ()) (Halt mark a)
  where
    eqIn
      :: mark ()
      -> Halt mark a
      -> Halt mark a
      -> Bool
    eqIn _ = (==)

instance
  ( MonadIdentity mark
  ) => Functor (Halt mark)
  where
    fmap
      :: (a -> b)
      -> Halt mark a
      -> Halt mark b
    fmap f x = case x of
      Step a -> Step (f a)
      Halt   -> Halt

instance
  ( MonadIdentity mark
  ) => Applicative (Halt mark)
  where
    pure
      :: a
      -> Halt mark a
    pure = Step

    (<*>)
      :: Halt mark (a -> b)
      -> Halt mark a
      -> Halt mark b
    f' <*> x' =
      case f' of
        Halt -> Halt
        Step f -> case x' of
          Halt -> Halt
          Step x -> Step (f x)

instance
  ( MonadIdentity mark
  ) => Monad (Halt mark)
  where
    return
      :: a
      -> Halt mark a
    return = Step

    (>>=)
      :: Halt mark a
      -> (a -> Halt mark b)
      -> Halt mark b
    x' >>= f =
      case x' of
        Halt -> Halt
        Step x -> f x

instance
  ( MonadIdentity mark
  ) => Commutant (Halt mark)
  where
    commute
      :: ( Applicative f )
      => Halt mark (f a)
      -> f (Halt mark a)
    commute x = case x of
      Halt -> pure Halt
      Step m -> Step <$> m

instance
  ( MonadIdentity mark
  ) => Central (Halt mark)

instance
  ( MonadIdentity mark
  ) => RunMonad (mark ()) (Halt mark) (Halt mark)
  where
    run
      :: mark ()
      -> Halt mark a
      -> Halt mark a
    run _ = id



{- Effect Classes -}

instance
  ( MonadIdentity mark
  ) => MonadHalt mark (Halt mark)
  where
    halt
      :: mark ()
      -> Halt mark a
    halt _ = Halt
