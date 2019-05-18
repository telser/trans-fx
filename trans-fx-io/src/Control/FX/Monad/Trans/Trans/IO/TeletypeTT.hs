-- | Module      : Control.FX.IO.Monad.Trans.Trans.TeletypeTT
--   Description : Teletype monad transformer transformer
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.FX.Monad.Trans.Trans.IO.TeletypeTT (
    TeletypeTT(..)
  , TeletypeAction(..)
  , evalTeletypeStdIO
  , evalTeletypeHandleIO
  , MonadTeletype(..)
  , TeletypeError(..)
  , IOException
  , runTeletypeTT
  , Context(..)
  , InputTT(..)
  , OutputTT(..)
) where



import Data.Typeable
  ( Typeable, Proxy, typeOf )
import Control.Exception
  ( IOException, try )
import Data.Time.Clock.System
  ( SystemTime )
import System.IO
  ( Handle, hPutStrLn, hGetLine )

import Control.FX
import Control.FX.Data
import Control.FX.Monad.Trans.Trans.IO.Class



-- | Teletype monad transformer transformer
newtype TeletypeTT
  (mark :: * -> *)
  (t :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = TeletypeTT
        { unTeletypeTT
            :: OverTT
                ( ExceptT TeletypeError (mark IOException) )
                ( PromptTT mark (TeletypeAction mark) )
                t m a
        } deriving
          ( Typeable, Functor, Applicative
          , Monad, MonadTrans, MonadTransTrans
          , MonadPrompt mark (TeletypeAction mark) )

deriving instance {-# OVERLAPPING #-}
  ( Monad m, MonadTrans t, MonadIdentity mark
  ) => MonadExcept TeletypeError (mark IOException) (TeletypeTT mark t m)

instance
  ( Typeable mark, Typeable t, Typeable m, Typeable a
  ) => Show (TeletypeTT mark t m a)
  where
    show
      :: TeletypeTT mark t m a
      -> String
    show = show . typeOf



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

instance MonadIdentity TeletypeError where
  unwrap = unTeletypeError



instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , Commutant mark, EqIn (t m)
  ) => EqIn (TeletypeTT mark t m)
  where
    newtype Context (TeletypeTT mark t m)
      = TeletypeTTCtx
          { unTeletypeTTCtx :: (Eval (TeletypeAction mark) m, Context (t m))
          } deriving (Typeable)

    eqIn
      :: (Eq a)
      => Context (TeletypeTT mark t m)
      -> TeletypeTT mark t m a
      -> TeletypeTT mark t m a
      -> Bool
    eqIn (TeletypeTTCtx (eval,h)) x y =
      eqIn h
        (fmap unTeletypeTTOut $ runTT (TeletypeTTIn eval) x)
        (fmap unTeletypeTTOut $ runTT (TeletypeTTIn eval) y)

instance
  ( Typeable mark, Typeable t, Typeable m
  ) => Show (Context (TeletypeTT mark t m))
  where
    show = show . typeOf



instance
  ( MonadIdentity mark, Commutant mark
  ) => RunMonadTransTrans (TeletypeTT mark)
  where
    newtype InputTT (TeletypeTT mark) m
      = TeletypeTTIn
          { unTeletypeTTIn :: Eval (TeletypeAction mark) m
          } deriving (Typeable)

    newtype OutputTT (TeletypeTT mark) a
      = TeletypeTTOut
          { unTeletypeTTOut :: Except TeletypeError (mark IOException) a
          } deriving (Typeable)

    runTT
      :: ( Monad m, MonadTrans t )
      => InputTT (TeletypeTT mark) m
      -> TeletypeTT mark t m a
      -> t m (OutputTT (TeletypeTT mark) a)
    runTT (TeletypeTTIn eval) (TeletypeTT x) =
      fmap (TeletypeTTOut . unExceptTOut . unwrap . unCompose . unOverTTOut) $
        runTT (OverTTIn (PromptTTIn eval, ExceptTIn (pure ()))) x

instance
  ( Typeable mark, Typeable m
  ) => Show (InputTT (TeletypeTT mark) m)
  where
    show = show . typeOf

deriving instance
  ( Show a, Show (mark IOException)
  ) => Show (OutputTT (TeletypeTT mark) a)

runTeletypeTT
  :: ( Monad m, MonadTrans t, MonadIdentity mark, Commutant mark )
  => Eval (TeletypeAction mark) m
  -> TeletypeTT mark t m a
  -> t m (Except TeletypeError (mark IOException) a)
runTeletypeTT p = fmap unTeletypeTTOut . runTT (TeletypeTTIn p)





{- Actions -}

-- | Type representing atomic teletype actions
data TeletypeAction mark a where
  ReadLine
    :: TeletypeAction mark
        (Except TeletypeError (mark IOException) String)

  PrintLine
    :: String
    -> TeletypeAction mark
        (Except TeletypeError (mark IOException) ())

-- | Default @IO@ evaluator
evalTeletypeStdIO
  :: ( MonadIdentity mark )
  => TeletypeAction mark a -> IO a
evalTeletypeStdIO x = case x of
  ReadLine -> do
    x <- try getLine
    return $ case x of
      Left e -> Except (pure e)
      Right a -> Accept a

  PrintLine msg -> do
    x <- try $ putStrLn msg
    return $ case x of
      Left e -> Except (pure e)
      Right () -> Accept ()

evalTeletypeHandleIO
  :: ( MonadIdentity mark )
  => Handle -- ^ Input
  -> Handle -- ^ Output
  -> TeletypeAction mark a -> IO a
evalTeletypeHandleIO hIn hOut x = case x of
  ReadLine -> do
    x <- try $ hGetLine hIn
    return $ case x of
      Left e -> Except (pure e)
      Right a -> Accept a

  PrintLine msg -> do
    x <- try $ hPutStrLn hOut msg
    return $ case x of
      Left e -> Except (pure e)
      Right () -> Accept ()





{- Effect Instances -}

instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t, MonadIdentity mark
  ) => MonadTeletype mark (TeletypeTT mark t m)
  where
    readLine
      :: TeletypeTT mark t m (mark String)
    readLine = TeletypeTT $ toOverTT $ do
      x :: mark (Except TeletypeError (mark IOException) String)
        <- lift $ prompt $ return ReadLine
      case unwrap x of
        Except e -> throw $ TeletypeError e
        Accept a -> return $ return a

    printLine
      :: mark String
      -> TeletypeTT mark t m ()
    printLine msg = TeletypeTT $ toOverTT $ do
      x :: mark (Except TeletypeError (mark IOException) ())
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
  , forall x. (Monad x) => MonadSystemClock mark (t x)
  ) => MonadSystemClock mark (TeletypeTT mark1 t m)
  where
    getSystemTime
      :: TeletypeTT mark1 t m (mark SystemTime)
    getSystemTime = liftT getSystemTime



instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadState mark s (t x)
  ) => MonadState mark s (TeletypeTT mark1 t m)
  where
    get
      :: TeletypeTT mark1 t m (mark s)
    get = TeletypeTT $ toOverTT $ lift $ liftT get

    put
      :: mark s
      -> TeletypeTT mark1 t m ()
    put = TeletypeTT . toOverTT . lift . liftT . put



-- instance {-# OVERLAPPABLE #-}
--   ( Monad m, MonadTrans t, MonadIdentity mark
--   , MonadIdentity mark1, Commutant mark1
--   , forall x. (Monad x) => MonadExcept mark e (t x)
--   ) => MonadExcept mark e (TeletypeTT mark1 t m)
--   where
--     throw
--       :: mark e
--       -> TeletypeTT mark1 t m a
--     throw = TeletypeTT . OverTT . lift . liftT . throw
-- 
--     catch
--       :: TeletypeTT mark1 t m a
--       -> (mark e -> TeletypeTT mark1 t m a)
--       -> TeletypeTT mark1 t m a
--     catch x h = TeletypeTT $ OverTT $
--       liftCatch (liftCatchT catch)
--         (unOverTT $ unTeletypeTT x)
--         (unOverTT . unTeletypeTT . h)



instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadIdentity mark1, Commutant mark1
  , forall x. (Monad x) => MonadReadOnly mark r (t x)
  ) => MonadReadOnly mark r (TeletypeTT mark1 t m)
  where
    ask
      :: TeletypeTT mark1 t m (mark r)
    ask = TeletypeTT $ toOverTT $ lift $ liftT ask

    local
      :: (mark r -> mark r)
      -> TeletypeTT mark1 t m a
      -> TeletypeTT mark1 t m a
    local f (TeletypeTT x) =
      TeletypeTT $ toOverTT $ local f $ unOverTT x



instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadIdentity mark1, Commutant mark1, Monoid w
  , forall x. (Monad x) => MonadAppendOnly mark w (t x)
  ) => MonadAppendOnly mark w (TeletypeTT mark1 t m)
  where
    jot
      :: mark w
      -> TeletypeTT mark1 t m ()
    jot = TeletypeTT . toOverTT . lift . liftT . jot

    look
      :: TeletypeTT mark1 t m (mark w)
    look = TeletypeTT $ toOverTT $ lift $ liftT look



instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadIdentity mark1, Commutant mark1
  , forall x. (Monad x) => MonadWriteOnce mark w (t x)
  ) => MonadWriteOnce mark w (TeletypeTT mark1 t m)
  where
    etch
      :: mark w
      -> TeletypeTT mark1 t m Bool
    etch = TeletypeTT . toOverTT . lift . liftT . etch

    press
      :: TeletypeTT mark1 t m (Maybe (mark w))
    press = TeletypeTT $ toOverTT $ lift $ liftT press



-- instance
--   ( Monad m, MonadTrans t, MonadIdentity mark
--   , MonadIdentity mark1, Commutant mark1, Monoid w
--   , forall x. (Monad x) => MonadWriteOnly mark w (t x)
--   ) => MonadWriteOnly mark w (TeletypeTT mark1 t m)
--   where
--     tell
--       :: mark w
--       -> TeletypeTT mark1 t m ()
--     tell = TeletypeTT . OverTT . lift . liftT . tell
-- 
--     draft
--       :: TeletypeTT mark1 t m a
--       -> TeletypeTT mark1 t m (Pair (mark w) a)
--     draft = TeletypeTT . OverTT . draft . unOverTT . unTeletypeTT



instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadIdentity mark1, Commutant mark1
  , forall x. (Monad x) => MonadPrompt mark p (t x)
  ) => MonadPrompt mark p (TeletypeTT mark1 t m)
  where
    prompt
      :: mark (p a)
      -> TeletypeTT mark1 t m (mark a)
    prompt = TeletypeTT . toOverTT . lift . liftT . prompt



instance
  ( Monad m, MonadTrans t, MonadIdentity mark1, MonadIdentity mark
  , forall x. (Monad x) => MonadHalt mark (t x)
  ) => MonadHalt mark (TeletypeTT mark1 t m)
  where
    halt
      :: mark ()
      -> TeletypeTT mark1 t m a
    halt = TeletypeTT . toOverTT . lift . liftT . halt



instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadStack mark f d (t x), IsStack f
  ) => MonadStack mark f d (TeletypeTT mark1 t m)
  where
    push
      :: Proxy f
      -> mark d
      -> TeletypeTT mark1 t m ()
    push proxy = TeletypeTT . toOverTT . lift . liftT . push proxy

    pop
      :: Proxy f
      -> TeletypeTT mark1 t m (mark (Maybe d))
    pop proxy = TeletypeTT $ toOverTT $ lift $ liftT $ pop proxy
