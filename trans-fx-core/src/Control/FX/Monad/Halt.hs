-- | Module      : Control.FX.Monad.Except
--   Description : Concrete halt monad
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
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.Halt (
    Halt(..)
  , Context(..)
  , Input(..)
  , Output(..)
) where



import Data.Typeable (Typeable)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad.Class



-- | Concrete monad representing catastrophic failure
-- @mark e@ and producing values of type @a@
data Halt
  (mark :: * -> *)
  (a :: *)
    = Step a -- ^ Proceed
    | Halt   -- ^ Bail out
    deriving (Eq, Show, Typeable)



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
  EqIn (Halt mark)
  where
    data Context (Halt mark)
      = HaltCtx
          { unHaltCtx :: mark ()
          } deriving (Typeable)

    eqIn
      :: (Eq a)
      => Context (Halt mark)
      -> Halt mark a
      -> Halt mark a
      -> Bool
    eqIn _ = (==)

deriving instance
  ( Eq (mark ())
  ) => Eq (Context (Halt mark))

deriving instance
  ( Show (mark ())
  ) => Show (Context (Halt mark))



instance
  ( MonadIdentity mark
  ) => RunMonad (Halt mark)
  where
    data Input (Halt mark)
      = HaltIn
          { unHaltIn :: mark ()
          } deriving (Typeable)

    data Output (Halt mark) a
      = HaltOut
          { unHaltOut :: Halt mark a
          } deriving (Typeable)

    run
      :: Input (Halt mark)
      -> Halt mark a
      -> Output (Halt mark) a
    run _ = HaltOut

deriving instance
  ( Eq (mark ())
  ) => Eq (Input (Halt mark))

deriving instance
  ( Show (mark ())
  ) => Show (Input (Halt mark))

deriving instance
  ( Eq a
  ) => Eq (Output (Halt mark) a)

deriving instance
  ( Show a
  ) => Show (Output (Halt mark) a)





{- Effect Classes -}

instance
  ( MonadIdentity mark
  ) => MonadHalt mark (Halt mark)
  where
    halt
      :: mark ()
      -> Halt mark a
    halt _ = Halt
