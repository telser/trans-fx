-- | Module      : Control.FX.IO.Monad.Trans.Trans.TeletypeTT
--   Description : Teletype monad transformer transformer
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.FX.IO.Monad.Trans.Trans.TeletypeTT (
    TeletypeTT(..)
  , runTeletypeTT

  , TeletypeAction(..)
  , evalTeletypeIO
) where



import Data.Typeable
  ( Typeable, Proxy )
import Control.Exception
  ( IOException, try )

import Control.FX
import Control.FX.IO.Monad.Trans.Trans.Class



data TeletypeAction a where
  ReadLine
    :: TeletypeAction
        (Except TeletypeError IOException String)

  PrintLine
    :: String
    -> TeletypeAction
        (Except TeletypeError IOException ())

evalTeletypeIO
  :: TeletypeAction a -> IO a
evalTeletypeIO x = case x of
  ReadLine -> do
    x <- try getLine
    return $ case x of
      Left e -> Except e
      Right a -> Accept a

  PrintLine msg -> do
    x <- try $ putStrLn msg
    return $ case x of
      Left e -> Except e
      Right () -> Accept ()

newtype TeletypeTT
  (mark :: * -> *)
  (t :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = TeletypeTT
        { unTeletypeTT
            :: OverTT
                ( PromptTT mark TeletypeAction )
                ( ExceptT TeletypeError IOException )
                t m a
        } deriving
          ( Typeable, Functor, Applicative, Monad
          , MonadTrans, MonadTransTrans
          , MonadPrompt mark TeletypeAction )

data TeletypeError
  (a :: *)
    = TeletypeError
      { unTeletypeError :: a
      } deriving (Eq, Show, Typeable)

instance Functor TeletypeError where
  fmap f (TeletypeError a) = TeletypeError (f a)

instance Applicative TeletypeError where
  pure = TeletypeError
  (TeletypeError f) <*> (TeletypeError x) =
    TeletypeError (f x)

instance Monad TeletypeError where
  return = TeletypeError
  (TeletypeError x) >>= f = f x

instance MonadIdentity TeletypeError where
  unwrap = unTeletypeError



instance
  RunMonadTransTrans
    (Eval TeletypeAction)
    (TeletypeTT mark)
    (Except TeletypeError IOException)
  where
    runTT
      :: ( Monad m, MonadTrans t )
      => Eval TeletypeAction m
      -> TeletypeTT mark t m a
      -> t m (Except TeletypeError IOException a)
    runTT eval (TeletypeTT x) =
      fmap (unIdentity . unCompose) $ runTT (Sing eval (pure ())) x

runTeletypeTT
  :: ( Monad m, MonadTrans t )
  => (forall a. TeletypeAction a -> m a)
  -> TeletypeTT mark t m a
  -> t m (Except TeletypeError IOException a)
runTeletypeTT f = runTT (Eval f)





{- Effect Instances -}

instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  ) => MonadTeletype mark (TeletypeTT mark t m)
  where
    readLine
      :: TeletypeTT mark t m (mark String)
    readLine = TeletypeTT $ OverTT $ do
      x :: mark (Except TeletypeError IOException String)
        <- lift $ prompt $ return ReadLine
      case unwrap x of
        Except e -> throw $ TeletypeError e
        Accept a -> return $ return a

    printLine
      :: mark String
      -> TeletypeTT mark t m ()
    printLine msg = TeletypeTT $ OverTT $ do
      x :: mark (Except TeletypeError IOException ())
        <- lift $ prompt $ return $ PrintLine $ unwrap msg
      case unwrap x of
        Except e -> throw $ TeletypeError e
        Accept a -> return a

instance
  ( Monad m, MonadTrans t, MonadFunctor w
  , MonadTransTrans u, MonadTeletype mark (u t m)
  ) => MonadTeletype mark (OverTT u w t m)
  where
    readLine
      :: OverTT u w t m (mark String)
    readLine = OverTT $ lift readLine

    printLine
      :: mark String
      -> OverTT u w t m ()
    printLine = OverTT . lift . printLine

instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t, MonadTransTrans u2, MonadIdentity mark
  ) => MonadTeletype mark (ComposeTT (TeletypeTT mark) u2 t m)
  where
    readLine
      :: ComposeTT (TeletypeTT mark) u2 t m (mark String)
    readLine = ComposeTT readLine

    printLine
      :: mark String
      -> ComposeTT (TeletypeTT mark) u2 t m ()
    printLine = ComposeTT . printLine

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadTransTrans u1
  , MonadTransTrans u2, MonadIdentity mark
  , forall m1 t1. (Monad m1, MonadTrans t1) => MonadTeletype mark (u2 t1 m1)
  ) => MonadTeletype mark (ComposeTT u1 u2 t m)
  where
    readLine
      :: ComposeTT u1 u2 t m (mark String)
    readLine = ComposeTT $ liftT readLine

    printLine
      :: mark String
      -> ComposeTT u1 u2 t m ()
    printLine = ComposeTT . liftT . printLine
