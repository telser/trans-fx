-- | Module      : Control.FX.Monad.Except
--   Description : Concrete exception monad
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-#
  LANGUAGE
    InstanceSigs,
    KindSignatures,
    FlexibleInstances,
    MultiParamTypeClasses
#-}

module Control.FX.Monad.Except (
    Except(..)
  , runExcept
) where



import Data.Typeable (Typeable)

import Control.FX.Functor
import Control.FX.Monad.Class



-- | Concrete exception monad, throwing marked exceptions of type
-- @mark e@ and producing values of type @a@
data Except
  (k :: * -> *)
  (e :: *)
  (a :: *)
    = Except e -- ^ Exceptional result
    | Accept a -- ^ Normal result
    deriving (Eq, Show, Typeable)

instance Functor (Except mark e) where
  fmap :: (a -> b) -> Except mark e a -> Except mark e b
  fmap f x = case x of
    Except e -> Except e
    Accept a -> Accept (f a)

instance Applicative (Except mark e) where
  pure :: a -> Except mark e a
  pure = Accept

  (<*>) :: Except mark e (a -> b) -> Except mark e a -> Except mark e b
  f' <*> x' =
    case f' of
      Except e -> Except e
      Accept f -> case x' of
        Except e -> Except e
        Accept x -> Accept (f x)

instance Monad (Except mark e) where
  return :: a -> Except mark e a
  return = Accept

  (>>=) :: Except mark e a -> (a -> Except mark e b) -> Except mark e b
  x' >>= f =
    case x' of
      Except e -> Except e
      Accept x -> f x

instance Commutant (Except mark e) where
  commute
    :: ( Applicative f )
    => Except mark e (f a) -> f (Except mark e a)
  commute x = case x of
    Except e -> pure (Except e)
    Accept m -> Accept <$> m

instance Central (Except mark e)

instance RunMonad () (Except mark e) (Except mark e) where
  run () = id

-- | Run an @Except e a@, producing either an @Except e@ or an @Accept a@.
runExcept :: Except mark e a -> Except mark e a
runExcept = run ()

instance
  ( MonadIdentity mark
  ) => MonadExcept mark e (Except mark e)
  where
    throw = Except . unwrap

    catch x h = case x of
      Accept a -> Accept a
      Except e -> h (pure e)
