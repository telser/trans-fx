-- | Module      : Control.FX.Monad.Except
--   Description : Concrete exception monad
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

module Control.FX.Monad.Except (
    Except(..)
  , Context(..)
  , Input(..)
  , Output(..)
) where



import Data.Typeable (Typeable)

import Control.FX.EqIn
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
  ) => Bifunctor (Except mark)
  where
    bimap1
      :: (e -> c)
      -> Except mark e a
      -> Except mark c a
    bimap1 f x = case x of
      Except e -> Except (f e)
      Accept a -> Accept a

    bimap2
      :: (a -> c)
      -> Except mark e a
      -> Except mark e c
    bimap2 f x = case x of
      Except e -> Except e
      Accept a -> Accept (f a)

instance
  ( MonadIdentity mark
  ) => Central (Except mark e)





instance
  ( Eq e
  ) => EqIn (Except mark e)
  where
    newtype Context (Except mark e)
      = ExceptCtx
          { unExceptCtx :: mark ()
          } deriving (Typeable)

    eqIn
      :: (Eq a)
      => Context (Except mark e)
      -> Except mark e a
      -> Except mark e a
      -> Bool
    eqIn _ = (==)

deriving instance
  ( Eq (mark ())
  ) => Eq (Context (Except mark e))

deriving instance
  ( Show (mark ())
  ) => Show (Context (Except mark e))



instance
  ( MonadIdentity mark
  ) => RunMonad (Except mark e)
  where
    newtype Input (Except mark e)
      = ExceptIn
          { unExceptIn :: mark ()
          } deriving (Typeable)

    newtype Output (Except mark e) a
      = ExceptOut
          { unExceptOut :: Except mark e a
          } deriving (Typeable)

    run
      :: Input (Except mark e)
      -> Except mark e a
      -> Output (Except mark e) a
    run _ = ExceptOut

deriving instance
  ( Eq (mark ())
  ) => Eq (Input (Except mark e))

deriving instance
  ( Show (mark ())
  ) => Show (Input (Except mark e))

deriving instance
  ( Eq e, Eq a
  ) => Eq (Output (Except mark e) a)

deriving instance
  ( Show e, Show a
  ) => Show (Output (Except mark e) a)





{- Effect Classes -}

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
