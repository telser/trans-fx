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
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.FX.IO.Monad.Trans.Trans.TeletypeTT (
    TeletypeTT(..)
  , TeletypeAction(..)
  , evalTeletypeIO
  , MonadTeletype(..)
  , TeletypeError(..)
  , runTeletypeTT
) where



import Data.Typeable
  ( Typeable, Proxy )
import Control.Exception
  ( IOException, try )

import Control.FX
import Control.FX.IO.Monad.Trans.Trans.Class



-- | Teletype monad transformer transformer
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
          ( Typeable, Functor, Applicative
          , Monad, MonadTrans, MonadTransTrans
          , MonadPrompt mark TeletypeAction )

deriving instance {-# OVERLAPPING #-}
  ( Monad m, MonadTrans t, MonadIdentity mark
  ) => MonadExcept TeletypeError IOException (TeletypeTT mark t m)



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

instance
  ( Semigroup a
  ) => Semigroup (TeletypeError a)
  where
    (<>)
      :: TeletypeError a
      -> TeletypeError a
      -> TeletypeError a
    (TeletypeError a) <> (TeletypeError b) =
      TeletypeError (a <> b)

instance
  ( Monoid a
  ) => Monoid (TeletypeError a)
  where
    mempty
      :: TeletypeError a
    mempty = TeletypeError mempty

    mappend
      :: TeletypeError a
      -> TeletypeError a
      -> TeletypeError a
    mappend = (<>)

instance MonadIdentity TeletypeError where
  unwrap = unTeletypeError



instance
  ( MonadIdentity mark, Commutant mark
  ) => RunMonadTransTrans
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
      fmap (unwrap . unCompose) $ runTT (Sing eval (pure ())) x

runTeletypeTT
  :: ( Monad m, MonadTrans t, MonadIdentity mark, Commutant mark )
  => Eval TeletypeAction m
  -> TeletypeTT mark t m a
  -> t m (Except TeletypeError IOException a)
runTeletypeTT = runTT





{- Actions -}

-- | Type representing atomic teletype actions
data TeletypeAction a where
  ReadLine
    :: TeletypeAction
        (Except TeletypeError IOException String)

  PrintLine
    :: String
    -> TeletypeAction
        (Except TeletypeError IOException ())

-- | Default @IO@ evaluator
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





{- Effect Instances -}

instance {-# OVERLAPS #-}
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

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadTeletype mark (t x)
  ) => MonadTeletype mark (TeletypeTT mark1 t m)
  where
    readLine
      :: TeletypeTT mark1 t m (mark String)
    readLine = liftT readLine

    printLine
      :: mark String
      -> TeletypeTT mark1 t m ()
    printLine = liftT . printLine



instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadState mark s (t x)
  ) => MonadState mark s (TeletypeTT mark1 t m)
  where
    get
      :: TeletypeTT mark1 t m (mark s)
    get = TeletypeTT $ OverTT $ lift $ liftT get

    put
      :: mark s
      -> TeletypeTT mark1 t m ()
    put = TeletypeTT . OverTT . lift . liftT . put



instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadIdentity mark1, Commutant mark1
  , forall x. (Monad x) => MonadExcept mark e (t x)
  ) => MonadExcept mark e (TeletypeTT mark1 t m)
  where
    throw
      :: mark e
      -> TeletypeTT mark1 t m a
    throw = TeletypeTT . OverTT . lift . liftT . throw

    catch
      :: TeletypeTT mark1 t m a
      -> (mark e -> TeletypeTT mark1 t m a)
      -> TeletypeTT mark1 t m a
    catch x h = TeletypeTT $ OverTT $
      liftCatch (liftCatchT catch)
        (unOverTT $ unTeletypeTT x)
        (unOverTT . unTeletypeTT . h)



instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadIdentity mark1, Commutant mark1
  , forall x. (Monad x) => MonadReadOnly mark r (t x)
  ) => MonadReadOnly mark r (TeletypeTT mark1 t m)
  where
    ask
      :: TeletypeTT mark1 t m (mark r)
    ask = TeletypeTT $ OverTT $ lift $ liftT ask

    local
      :: (mark r -> mark r)
      -> TeletypeTT mark1 t m a
      -> TeletypeTT mark1 t m a
    local f x = TeletypeTT $ OverTT $
      liftLocal (liftLocalT local) f
        (unOverTT $ unTeletypeTT x)



instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadIdentity mark1, Commutant mark1, Monoid w
  , forall x. (Monad x) => MonadWriteOnly mark w (t x)
  ) => MonadWriteOnly mark w (TeletypeTT mark1 t m)
  where
    tell
      :: mark w
      -> TeletypeTT mark1 t m ()
    tell = TeletypeTT . OverTT . lift . liftT . tell

    draft
      :: TeletypeTT mark1 t m a
      -> TeletypeTT mark1 t m (Pair (mark w) a)
    draft = TeletypeTT . OverTT . draft . unOverTT . unTeletypeTT



instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadIdentity mark1, Commutant mark1
  , forall x. (Monad x) => MonadPrompt mark p (t x)
  ) => MonadPrompt mark p (TeletypeTT mark1 t m)
  where
    prompt
      :: mark (p a)
      -> TeletypeTT mark1 t m (mark a)
    prompt = TeletypeTT . OverTT . lift . liftT . prompt



instance
  ( Monad m, MonadTrans t, MonadIdentity mark1, MonadIdentity mark
  , forall x. (Monad x) => MonadHalt mark (t x)
  ) => MonadHalt mark (TeletypeTT mark1 t m)
  where
    halt
      :: mark ()
      -> TeletypeTT mark1 t m a
    halt = TeletypeTT . OverTT . lift . liftT . halt
