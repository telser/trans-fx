-- | Module      : Control.FX.Monad.Except
--   Description : Concrete exception monad
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

module Control.FX.Monad.Except (
    Except(..)
) where



import Data.Typeable (Typeable)

import Control.FX.Functor
import Control.FX.Monad.Class



-- | Concrete exception monad, throwing marked exceptions of type
-- @mark e@ and producing values of type @a@
data Except
  (mark :: * -> *)
  (e :: *)
  (a :: *)
    = Except e -- ^ Exceptional result
    | Accept a -- ^ Normal result
    deriving (Eq, Show, Typeable)

instance
  ( MonadIdentity mark
  ) => Functor (Except mark e)
  where
    fmap
      :: (a -> b)
      -> Except mark e a
      -> Except mark e b
    fmap f x = case x of
      Except e -> Except e
      Accept a -> Accept (f a)

instance
  ( MonadIdentity mark
  ) => Applicative (Except mark e)
  where
    pure
      :: a
      -> Except mark e a
    pure = Accept

    (<*>)
      :: Except mark e (a -> b)
      -> Except mark e a
      -> Except mark e b
    f' <*> x' =
      case f' of
        Except e -> Except e
        Accept f -> case x' of
          Except e -> Except e
          Accept x -> Accept (f x)

instance
  ( MonadIdentity mark
  ) => Monad (Except mark e)
  where
    return
      :: a
      -> Except mark e a
    return = Accept

    (>>=)
      :: Except mark e a
      -> (a -> Except mark e b)
      -> Except mark e b
    x' >>= f =
      case x' of
        Except e -> Except e
        Accept x -> f x

instance
  ( MonadIdentity mark
  ) => Commutant (Except mark e)
  where
    commute
      :: ( Applicative f )
      => Except mark e (f a)
      -> f (Except mark e a)
    commute x = case x of
      Except e -> pure (Except e)
      Accept m -> Accept <$> m

instance
  ( MonadIdentity mark
  ) => Central (Except mark e)

instance
  ( MonadIdentity mark
  ) => RunMonad (mark ()) (Except mark e) (Except mark e)
  where
    run
      :: mark ()
      -> Except mark e a
      -> Except mark e a
    run _ = id

instance
  ( MonadIdentity mark
  ) => MonadExcept mark e (Except mark e)
  where
    throw
      :: mark e
      -> Except mark e a
    throw = Except . unwrap

    catch
      :: Except mark e a
      -> (mark e -> Except mark e a)
      -> Except mark e a
    catch x h = case x of
      Accept a -> Accept a
      Except e -> h (pure e)
