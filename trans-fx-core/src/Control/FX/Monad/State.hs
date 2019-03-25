-- | Module      : Control.FX.Monad.State
--   Description : Concrete state monad
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

module Control.FX.Monad.State (
    State(..)
  , Context(..)
  , Input(..)
  , Output(..)
) where



import Data.Typeable (Typeable, typeOf)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad.Class



-- | Concrete state monad
newtype State
  (mark :: * -> *)
  (s :: *)
  (a :: *)
    = State
        { unState :: s -> Pair s a
        } deriving (Typeable)



instance
  ( Typeable s, Typeable a, Typeable mark
  ) => Show (State mark s a)
  where
    show
      :: State mark s a
      -> String
    show = show . typeOf

instance
  ( MonadIdentity mark
  ) => Functor (State mark s)
  where
    fmap
      :: (a -> b)
      -> State mark s a
      -> State mark s b
    fmap f (State x) = State $ \s1 ->
      let Pair s2 a = x s1 in
      Pair s2 (f a)

instance
  ( MonadIdentity mark
  ) => Applicative (State mark s)
  where
    pure
      :: a
      -> State mark s a
    pure a = State $ \s -> Pair s a

    (<*>)
      :: State mark s (a -> b)
      -> State mark s a
      -> State mark s b
    (State f') <*> (State x') = State $ \s1 ->
      let Pair s2 f = f' s1 in
      let Pair s3 x = x' s2 in
      Pair s3 (f x)

instance
  ( MonadIdentity mark
  ) => Monad (State mark s)
  where
    return
      :: a
      -> State mark s a
    return a = State $ \s -> Pair s a

    (>>=)
      :: State mark s a
      -> (a -> State mark s b)
      -> State mark s b
    (State x') >>= f = State $ \s1 ->
      let Pair s2 x = x' s1 in
      (unState . f) x s2





instance
  ( Eq s, MonadIdentity mark
  ) => EqIn (State mark s)
  where
    data Context (State mark s)
      = StateCtx
          { unStateCtx :: mark s
          } deriving (Typeable)

    eqIn
      :: (Eq a)
      => Context (State mark s)
      -> State mark s a
      -> State mark s a
      -> Bool
    eqIn (StateCtx s) (State x) (State y) =
      (x $ unwrap s) == (y $ unwrap s)

deriving instance
  ( Eq (mark s)
  ) => Eq (Context (State mark s))

deriving instance
  ( Show (mark s)
  ) => Show (Context (State mark s))



instance
  ( MonadIdentity mark
  ) => RunMonad (State mark s)
  where
    data Input (State mark s)
      = StateIn
          { unStateIn :: mark s
          } deriving (Typeable)

    data Output (State mark s) a
      = StateOut
          { unStateOut :: Pair (mark s) a
          } deriving (Typeable)

    run
      :: Input (State mark s)
      -> State mark s a
      -> Output (State mark s) a
    run (StateIn s) (State x) =
      let Pair s1 a = x (unwrap s)
      in StateOut $ Pair (return s1) a

deriving instance
  ( Eq (mark s)
  ) => Eq (Input (State mark s))

deriving instance
  ( Show (mark s)
  ) => Show (Input (State mark s))

deriving instance
  ( Eq (mark s), Eq a
  ) => Eq (Output (State mark s) a)

deriving instance
  ( Show (mark s), Show a
  ) => Show (Output (State mark s) a)





{- Effect Class -}

instance
  ( MonadIdentity mark
  ) => MonadState mark s (State mark s)
  where
    get
      :: State mark s (mark s)
    get = State $ \s ->
      Pair s (pure s)

    put
      :: mark s
      -> State mark s ()
    put s = State $ \_ ->
      Pair (unwrap s) ()
