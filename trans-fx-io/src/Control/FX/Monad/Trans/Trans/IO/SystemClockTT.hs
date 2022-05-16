-- | Module      : Control.FX.IO.Monad.Trans.Trans.SystemClockTT
--   Description : System clock monad transformer transformer
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE RankNTypes                 #-}
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

module Control.FX.Monad.Trans.Trans.IO.SystemClockTT (
    SystemClockTT(..)
  , SystemClockAction(..)
  , evalSystemTimeIO
  , MonadSystemClock(..)
  , runSystemClockTT
  , Context(..)
  , InputTT(..)
  , OutputTT(..)
) where



import Data.Int
  ( Int64 )
import Data.Typeable
  ( Typeable, Proxy, typeOf )
import Control.Exception
  ( IOException, try )
import Data.Time.Clock.System
  ( SystemTime )
import qualified Data.Time.Clock.System as IO
  ( getSystemTime )

import qualified Network.HTTP.Req as Req
import qualified Database.SQLite.Simple as SQLite

import Control.FX
import Control.FX.Data
import Control.FX.Monad.Trans.Trans.IO.Class



-- | System clock monad transformer transformer
newtype SystemClockTT
  (mark :: * -> *)
  (t :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = SystemClockTT
        { unSystemClockTT
            :: PromptTT mark (SystemClockAction mark) t m a
        } deriving
          ( Typeable, Functor, Applicative
          , Monad, MonadTrans, MonadTransTrans
          , MonadPrompt mark (SystemClockAction mark) )

instance
  ( Typeable mark, Typeable t, Typeable m, Typeable a
  ) => Show (SystemClockTT mark t m a)
  where
    show
      :: SystemClockTT mark t m a
      -> String
    show = show . typeOf



instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , Commutant mark, EqIn (t m)
  ) => EqIn (SystemClockTT mark t m)
  where
    newtype Context (SystemClockTT mark t m)
      = SystemClockTTCtx
          { unSystemClockTTCtx :: (Eval (SystemClockAction mark) m, Context (t m))
          } deriving (Typeable)

    eqIn
      :: (Eq a)
      => Context (SystemClockTT mark t m)
      -> SystemClockTT mark t m a
      -> SystemClockTT mark t m a
      -> Bool
    eqIn (SystemClockTTCtx (eval,h)) x y =
      eqIn h
        (fmap unSystemClockTTOut $ runTT (SystemClockTTIn eval) x)
        (fmap unSystemClockTTOut $ runTT (SystemClockTTIn eval) y)

instance
  ( Typeable mark, Typeable t, Typeable m
  ) => Show (Context (SystemClockTT mark t m))
  where
    show = show . typeOf



instance
  ( MonadIdentity mark, Commutant mark
  ) => RunMonadTransTrans (SystemClockTT mark)
  where
    newtype InputTT (SystemClockTT mark) m
      = SystemClockTTIn
          { unSystemClockTTIn :: Eval (SystemClockAction mark) m
          } deriving (Typeable)

    newtype OutputTT (SystemClockTT mark) a
      = SystemClockTTOut
          { unSystemClockTTOut :: a
          } deriving (Typeable)

    runTT
      :: ( Monad m, MonadTrans t )
      => InputTT (SystemClockTT mark) m
      -> SystemClockTT mark t m a
      -> t m (OutputTT (SystemClockTT mark) a)
    runTT (SystemClockTTIn eval) (SystemClockTT x) =
      fmap (SystemClockTTOut . unwrap) $ runTT (PromptTTIn eval) x

instance
  ( Typeable mark, Typeable m
  ) => Show (InputTT (SystemClockTT mark) m)
  where
    show = show . typeOf

deriving instance
  ( Show a, Show (mark IOException)
  ) => Show (OutputTT (SystemClockTT mark) a)

runSystemClockTT
  :: ( Monad m, MonadTrans t, MonadIdentity mark, Commutant mark )
  => Eval (SystemClockAction mark) m
  -> SystemClockTT mark t m a
  -> t m a
runSystemClockTT p = fmap unSystemClockTTOut . runTT (SystemClockTTIn p)





{- Actions -}

-- | Type representing atomic system clock actions
data SystemClockAction (mark :: * -> *) a where
  GetSystemTime
    :: SystemClockAction mark SystemTime

-- | Default @IO@ evaluator
evalSystemTimeIO
  :: ( MonadIdentity mark )
  => SystemClockAction mark a -> IO a
evalSystemTimeIO x = case x of
  GetSystemTime -> IO.getSystemTime





{- Effect Instances -}

instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t, MonadIdentity mark
  ) => MonadSystemClock mark (SystemClockTT mark t m)
  where
    getSystemTime
      :: SystemClockTT mark t m (mark SystemTime)
    getSystemTime = SystemClockTT $ do
      let
        act :: mark (SystemClockAction mark SystemTime)
        act = return GetSystemTime
      prompt act

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadSystemClock mark (t x)
  ) => MonadSystemClock mark (SystemClockTT mark1 t m)
  where
    getSystemTime
      :: SystemClockTT mark1 t m (mark SystemTime)
    getSystemTime = liftT getSystemTime



instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadTeletype mark (t x)
  ) => MonadTeletype mark (SystemClockTT mark1 t m)
  where
    readLine
      :: SystemClockTT mark1 t m (mark String)
    readLine = liftT readLine

    printLine
      :: mark String
      -> SystemClockTT mark1 t m ()
    printLine = liftT . printLine



instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadSimpleHttp mark (t x)
  ) => MonadSimpleHttp mark (SystemClockTT mark1 t m)
  where
    simpleHttpReq
      :: ( Req.HttpMethod method, Req.HttpBody body, Req.HttpResponse response
         , Req.HttpBodyAllowed (Req.AllowsBody method) (Req.ProvidesBody body) )
      => method
      -> Req.Url scheme
      -> body
      -> Proxy response
      -> Req.Option scheme
      -> SystemClockTT mark1 t m (mark response)
    simpleHttpReq method scheme body response opt =
      liftT $ simpleHttpReq method scheme body response opt



instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadSimpleSQLite mark (t x)
  ) => MonadSimpleSQLite mark (SystemClockTT mark1 t m)
  where
    simpleSQLiteOpen
      :: String
      -> SystemClockTT mark1 t m (mark SQLite.Connection)
    simpleSQLiteOpen path =
      liftT $ simpleSQLiteOpen path

    simpleSQLiteClose
      :: mark SQLite.Connection
      -> SystemClockTT mark1 t m ()
    simpleSQLiteClose conn =
      liftT $ simpleSQLiteClose conn

    simpleSQLiteQuery
      :: ( SQLite.ToRow q, SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> SystemClockTT mark1 t m (mark [r])
    simpleSQLiteQuery conn query q =
      liftT $ simpleSQLiteQuery conn query q

    simpleSQLiteQuery_
      :: ( SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> SystemClockTT mark1 t m (mark [r])
    simpleSQLiteQuery_ conn query =
      liftT $ simpleSQLiteQuery_ conn query

    simpleSQLiteQueryNamed
      :: ( SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> SystemClockTT mark1 t m (mark [r])
    simpleSQLiteQueryNamed conn query params =
      liftT $ simpleSQLiteQueryNamed conn query params

    simpleSQLiteLastInsertRowId
      :: SQLite.Connection
      -> SystemClockTT mark1 t m (mark Int64)
    simpleSQLiteLastInsertRowId conn =
      liftT $ simpleSQLiteLastInsertRowId conn

    simpleSQLiteChanges
      :: SQLite.Connection
      -> SystemClockTT mark1 t m (mark Int)
    simpleSQLiteChanges conn =
      liftT $ simpleSQLiteChanges conn

    simpleSQLiteExecute
      :: ( SQLite.ToRow q )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> SystemClockTT mark1 t m (mark ())
    simpleSQLiteExecute conn query q =
      liftT $ simpleSQLiteExecute conn query q

    simpleSQLiteExecute_
      :: SQLite.Connection
      -> SQLite.Query
      -> SystemClockTT mark1 t m (mark ())
    simpleSQLiteExecute_ conn query =
      liftT $ simpleSQLiteExecute_ conn query

    simpleSQLiteExecuteNamed
      :: SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> SystemClockTT mark1 t m (mark ())
    simpleSQLiteExecuteNamed conn query params =
      liftT $ simpleSQLiteExecuteNamed conn query params



instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadState mark s (t x)
  ) => MonadState mark s (SystemClockTT mark1 t m)
  where
    get
      :: SystemClockTT mark1 t m (mark s)
    get = SystemClockTT $ liftT get

    put
      :: mark s
      -> SystemClockTT mark1 t m ()
    put = SystemClockTT . liftT . put



-- instance {-# OVERLAPPABLE #-}
--   ( Monad m, MonadTrans t, MonadIdentity mark
--   , MonadIdentity mark1, Commutant mark1
--   , forall x. (Monad x) => MonadExcept mark e (t x)
--   ) => MonadExcept mark e (SystemClockTT mark1 t m)
--   where
--     throw
--       :: mark e
--       -> SystemClockTT mark1 t m a
--     throw = SystemClockTT . OverTT . lift . liftT . throw
-- 
--     catch
--       :: SystemClockTT mark1 t m a
--       -> (mark e -> SystemClockTT mark1 t m a)
--       -> SystemClockTT mark1 t m a
--     catch x h = SystemClockTT $ OverTT $
--       liftCatch (liftCatchT catch)
--         (unOverTT $ unSystemClockTT x)
--         (unOverTT . unSystemClockTT . h)



instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadIdentity mark1, Commutant mark1
  , forall x. (Monad x) => MonadReadOnly mark r (t x)
  ) => MonadReadOnly mark r (SystemClockTT mark1 t m)
  where
    ask
      :: SystemClockTT mark1 t m (mark r)
    ask = SystemClockTT $ liftT ask

    local
      :: (mark r -> mark r)
      -> SystemClockTT mark1 t m a
      -> SystemClockTT mark1 t m a
    local f (SystemClockTT x) =
      SystemClockTT $ local f x



instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadIdentity mark1, Commutant mark1, Monoid w
  , forall x. (Monad x) => MonadAppendOnly mark w (t x)
  ) => MonadAppendOnly mark w (SystemClockTT mark1 t m)
  where
    jot
      :: mark w
      -> SystemClockTT mark1 t m ()
    jot = SystemClockTT . liftT . jot

    look
      :: SystemClockTT mark1 t m (mark w)
    look = SystemClockTT $ liftT look



instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadIdentity mark1, Commutant mark1
  , forall x. (Monad x) => MonadWriteOnce mark w (t x)
  ) => MonadWriteOnce mark w (SystemClockTT mark1 t m)
  where
    etch
      :: mark w
      -> SystemClockTT mark1 t m Bool
    etch = SystemClockTT . liftT . etch

    press
      :: SystemClockTT mark1 t m (Maybe (mark w))
    press = SystemClockTT $ liftT press



-- instance
--   ( Monad m, MonadTrans t, MonadIdentity mark
--   , MonadIdentity mark1, Commutant mark1, Monoid w
--   , forall x. (Monad x) => MonadWriteOnly mark w (t x)
--   ) => MonadWriteOnly mark w (SystemClockTT mark1 t m)
--   where
--     tell
--       :: mark w
--       -> SystemClockTT mark1 t m ()
--     tell = SystemClockTT . OverTT . lift . liftT . tell
-- 
--     draft
--       :: SystemClockTT mark1 t m a
--       -> SystemClockTT mark1 t m (Pair (mark w) a)
--     draft = SystemClockTT . OverTT . draft . unOverTT . unSystemClockTT



instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadIdentity mark1, Commutant mark1
  , forall x. (Monad x) => MonadPrompt mark p (t x)
  ) => MonadPrompt mark p (SystemClockTT mark1 t m)
  where
    prompt
      :: mark (p a)
      -> SystemClockTT mark1 t m (mark a)
    prompt = SystemClockTT . liftT . prompt



instance
  ( Monad m, MonadTrans t, MonadIdentity mark1, MonadIdentity mark
  , forall x. (Monad x) => MonadHalt mark (t x)
  ) => MonadHalt mark (SystemClockTT mark1 t m)
  where
    halt
      :: mark ()
      -> SystemClockTT mark1 t m a
    halt = SystemClockTT . liftT . halt



instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadStack mark f d (t x), IsStack f
  ) => MonadStack mark f d (SystemClockTT mark1 t m)
  where
    push
      :: Proxy f
      -> mark d
      -> SystemClockTT mark1 t m ()
    push proxy = SystemClockTT . liftT . push proxy

    pop
      :: Proxy f
      -> SystemClockTT mark1 t m (mark (Maybe d))
    pop proxy = SystemClockTT $ liftT $ pop proxy
