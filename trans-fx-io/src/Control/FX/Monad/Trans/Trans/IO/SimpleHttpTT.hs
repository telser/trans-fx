-- | Module      : Control.FX.IO.Monad.Trans.Trans.SimpleHttpTT
--   Description : Simple HTTP client monad transformer transformer
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

module Control.FX.Monad.Trans.Trans.IO.SimpleHttpTT (
    SimpleHttpTT(..)
  , SimpleHttpAction(..)
  , evalSimpleHttpReqIO
  , evalSimpleHttpPure
  , SimpleHttpError(..)
  , HttpException
  , runSimpleHttpTT
  , Context(..)
  , InputTT(..)
  , OutputTT(..)
) where



import Data.Int
  ( Int64 )
import Data.Typeable
  ( Typeable, Proxy, typeOf )
import Control.Exception
  ( try )

import qualified Network.HTTP.Req as Req
import           Network.HTTP.Req ( HttpException(..) )
import qualified Database.SQLite.Simple as SQLite

import Control.FX
import Control.FX.Data
import Control.FX.Monad.Trans.Trans.IO.Class



-- | Teletype monad transformer transformer
newtype SimpleHttpTT
  (mark :: * -> *)
  (t :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = SimpleHttpTT
        { unSimpleHttpTT
            :: OverTT
                ( ExceptT SimpleHttpError (mark HttpException) )
                ( PromptTT mark (SimpleHttpAction mark) )
                t m a
        } deriving
          ( Typeable, Functor, Applicative
          , Monad, MonadTrans, MonadTransTrans
          , MonadPrompt mark (SimpleHttpAction mark) )

deriving instance {-# OVERLAPPING #-}
  ( Monad m, MonadTrans t, MonadIdentity mark
  ) => MonadExcept SimpleHttpError (mark HttpException) (SimpleHttpTT mark t m)

instance
  ( Typeable mark, Typeable t, Typeable m, Typeable a
  ) => Show (SimpleHttpTT mark t m a)
  where
    show
      :: SimpleHttpTT mark t m a
      -> String
    show = show . typeOf

instance Eq HttpException where
  _ == _ = False



data SimpleHttpError
  (a :: *)
    = SimpleHttpError
      { unSimpleHttpError :: a
      } deriving (Eq, Show, Typeable)

instance Functor SimpleHttpError where
  fmap f (SimpleHttpError a) = SimpleHttpError (f a)

instance Applicative SimpleHttpError where
  pure = SimpleHttpError
  (SimpleHttpError f) <*> (SimpleHttpError x) =
    SimpleHttpError (f x)

instance Monad SimpleHttpError where
  return = SimpleHttpError
  (SimpleHttpError x) >>= f = f x

instance
  ( Semigroup a
  ) => Semigroup (SimpleHttpError a)
  where
    (<>)
      :: SimpleHttpError a
      -> SimpleHttpError a
      -> SimpleHttpError a
    (SimpleHttpError a) <> (SimpleHttpError b) =
      SimpleHttpError (a <> b)

instance
  ( Monoid a
  ) => Monoid (SimpleHttpError a)
  where
    mempty
      :: SimpleHttpError a
    mempty = SimpleHttpError mempty

instance MonadIdentity SimpleHttpError where
  unwrap = unSimpleHttpError



instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , Commutant mark, EqIn (t m)
  ) => EqIn (SimpleHttpTT mark t m)
  where
    newtype Context (SimpleHttpTT mark t m)
      = SimpleHttpTTCtx
          { unSimpleHttpTTCtx :: (Eval (SimpleHttpAction mark) m, Context (t m))
          } deriving (Typeable)

    eqIn
      :: (Eq a)
      => Context (SimpleHttpTT mark t m)
      -> SimpleHttpTT mark t m a
      -> SimpleHttpTT mark t m a
      -> Bool
    eqIn (SimpleHttpTTCtx (eval,h)) x y =
      eqIn h
        (fmap unSimpleHttpTTOut $ runTT (SimpleHttpTTIn eval) x)
        (fmap unSimpleHttpTTOut $ runTT (SimpleHttpTTIn eval) y)

instance
  ( Typeable mark, Typeable t, Typeable m
  ) => Show (Context (SimpleHttpTT mark t m))
  where
    show = show . typeOf



instance
  ( MonadIdentity mark, Commutant mark
  ) => RunMonadTransTrans (SimpleHttpTT mark)
  where
    newtype InputTT (SimpleHttpTT mark) m
      = SimpleHttpTTIn
          { unSimpleHttpTTIn :: Eval (SimpleHttpAction mark) m
          } deriving (Typeable)

    newtype OutputTT (SimpleHttpTT mark) a
      = SimpleHttpTTOut
          { unSimpleHttpTTOut :: Except SimpleHttpError (mark HttpException) a
          } deriving (Typeable)

    runTT
      :: ( Monad m, MonadTrans t )
      => InputTT (SimpleHttpTT mark) m
      -> SimpleHttpTT mark t m a
      -> t m (OutputTT (SimpleHttpTT mark) a)
    runTT (SimpleHttpTTIn eval) (SimpleHttpTT x) =
      fmap (SimpleHttpTTOut . unExceptTOut . unwrap . unCompose . unOverTTOut) $
        runTT (OverTTIn (PromptTTIn eval, ExceptTIn (pure ()))) x

instance
  ( Typeable mark, Typeable m
  ) => Show (InputTT (SimpleHttpTT mark) m)
  where
    show = show . typeOf

deriving instance
  ( Show a, Show (mark HttpException)
  ) => Show (OutputTT (SimpleHttpTT mark) a)

runSimpleHttpTT
  :: ( Monad m, MonadTrans t, MonadIdentity mark, Commutant mark )
  => Eval (SimpleHttpAction mark) m
  -> SimpleHttpTT mark t m a
  -> t m (Except SimpleHttpError (mark HttpException) a)
runSimpleHttpTT p = fmap unSimpleHttpTTOut . runTT (SimpleHttpTTIn p)





{- Actions -}

-- | Type representing atomic teletype actions
data SimpleHttpAction mark a where
  SimpleHttpReq
    :: ( Req.HttpMethod method, Req.HttpBody body, Req.HttpResponse response
       , Req.HttpBodyAllowed (Req.AllowsBody method) (Req.ProvidesBody body) )
    => method
    -> Req.Url scheme
    -> body
    -> Proxy response
    -> Req.Option scheme
    -> SimpleHttpAction mark
        (Except SimpleHttpError (mark HttpException) response)

-- | Default @IO@ evaluator
evalSimpleHttpReqIO
  :: ( MonadIdentity mark )
  => Req.HttpConfig
  -> SimpleHttpAction mark a -> IO a
evalSimpleHttpReqIO config x = case x of
  SimpleHttpReq method scheme body response opt -> do
    x <- try $ Req.runReq config $ Req.req method scheme body response opt
    return $ case x of
      Left e -> Except (pure e)
      Right a -> Accept a

evalSimpleHttpPure
  :: ( MonadIdentity mark, Monad eff )
  => ( forall mark eff method scheme body response a.
         ( Req.HttpMethod method, Req.HttpBody body, Req.HttpResponse response
         , Req.HttpBodyAllowed (Req.AllowsBody method) (Req.ProvidesBody body) )
       => method -> Req.Url scheme -> body -> Proxy response -> Req.Option scheme
       -> Except SimpleHttpError (mark HttpException) response
     )
  -> SimpleHttpAction mark a -> eff a
evalSimpleHttpPure f x = case x of
  SimpleHttpReq method scheme body response opt ->
    return $ f method scheme body response opt





{- Effect Instances -}

instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t, MonadIdentity mark
  ) => MonadSimpleHttp mark (SimpleHttpTT mark t m)
  where
    simpleHttpReq
      :: forall method scheme body response
       . ( Req.HttpMethod method, Req.HttpBody body, Req.HttpResponse response
         , Req.HttpBodyAllowed (Req.AllowsBody method) (Req.ProvidesBody body) )
      => method
      -> Req.Url scheme
      -> body
      -> Proxy response
      -> Req.Option scheme
      -> SimpleHttpTT mark t m (mark response)
    simpleHttpReq method scheme body response opt = SimpleHttpTT $ toOverTT $ do
      let
        act :: mark (SimpleHttpAction mark
                (Except SimpleHttpError (mark HttpException) response))
        act = pure $ SimpleHttpReq method scheme body response opt
      x :: mark (Except SimpleHttpError (mark HttpException) response)
        <- lift $ prompt act
      case unwrap x of
        Except e -> throw $ SimpleHttpError e
        Accept a -> pure $ return a

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadSimpleHttp mark (t x)
  ) => MonadSimpleHttp mark (SimpleHttpTT mark1 t m)
  where
    simpleHttpReq
      :: ( Req.HttpMethod method, Req.HttpBody body, Req.HttpResponse response
         , Req.HttpBodyAllowed (Req.AllowsBody method) (Req.ProvidesBody body) )
      => method
      -> Req.Url scheme
      -> body
      -> Proxy response
      -> Req.Option scheme
      -> SimpleHttpTT mark1 t m (mark response)
    simpleHttpReq method scheme body response opt =
      liftT $ simpleHttpReq method scheme body response opt



instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadTeletype mark (t x)
  ) => MonadTeletype mark (SimpleHttpTT mark1 t m)
  where
    readLine
      :: SimpleHttpTT mark1 t m (mark String)
    readLine = liftT readLine

    printLine
      :: mark String
      -> SimpleHttpTT mark1 t m ()
    printLine = liftT . printLine



instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadSystemClock mark (t x)
  ) => MonadSystemClock mark (SimpleHttpTT mark1 t m)
  where
    getSystemTime
      :: SimpleHttpTT mark1 t m (mark SystemTime)
    getSystemTime = liftT getSystemTime



instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadSimpleSQLite mark (t x)
  ) => MonadSimpleSQLite mark (SimpleHttpTT mark1 t m)
  where
    simpleSQLiteOpen
      :: String
      -> SimpleHttpTT mark1 t m (mark SQLite.Connection)
    simpleSQLiteOpen path =
      liftT $ simpleSQLiteOpen path

    simpleSQLiteClose
      :: mark SQLite.Connection
      -> SimpleHttpTT mark1 t m ()
    simpleSQLiteClose conn =
      liftT $ simpleSQLiteClose conn

    simpleSQLiteQuery
      :: ( SQLite.ToRow q, SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> SimpleHttpTT mark1 t m (mark [r])
    simpleSQLiteQuery conn query q =
      liftT $ simpleSQLiteQuery conn query q

    simpleSQLiteQuery_
      :: ( SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> SimpleHttpTT mark1 t m (mark [r])
    simpleSQLiteQuery_ conn query =
      liftT $ simpleSQLiteQuery_ conn query

    simpleSQLiteQueryNamed
      :: ( SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> SimpleHttpTT mark1 t m (mark [r])
    simpleSQLiteQueryNamed conn query params =
      liftT $ simpleSQLiteQueryNamed conn query params

    simpleSQLiteLastInsertRowId
      :: SQLite.Connection
      -> SimpleHttpTT mark1 t m (mark Int64)
    simpleSQLiteLastInsertRowId conn =
      liftT $ simpleSQLiteLastInsertRowId conn

    simpleSQLiteChanges
      :: SQLite.Connection
      -> SimpleHttpTT mark1 t m (mark Int)
    simpleSQLiteChanges conn =
      liftT $ simpleSQLiteChanges conn

    simpleSQLiteExecute
      :: ( SQLite.ToRow q )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> SimpleHttpTT mark1 t m (mark ())
    simpleSQLiteExecute conn query q =
      liftT $ simpleSQLiteExecute conn query q

    simpleSQLiteExecute_
      :: SQLite.Connection
      -> SQLite.Query
      -> SimpleHttpTT mark1 t m (mark ())
    simpleSQLiteExecute_ conn query =
      liftT $ simpleSQLiteExecute_ conn query

    simpleSQLiteExecuteNamed
      :: SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> SimpleHttpTT mark1 t m (mark ())
    simpleSQLiteExecuteNamed conn query params =
      liftT $ simpleSQLiteExecuteNamed conn query params



instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadState mark s (t x)
  ) => MonadState mark s (SimpleHttpTT mark1 t m)
  where
    get
      :: SimpleHttpTT mark1 t m (mark s)
    get = SimpleHttpTT $ toOverTT $ lift $ liftT get

    put
      :: mark s
      -> SimpleHttpTT mark1 t m ()
    put = SimpleHttpTT . toOverTT . lift . liftT . put



-- instance {-# OVERLAPPABLE #-}
--   ( Monad m, MonadTrans t, MonadIdentity mark
--   , MonadIdentity mark1, Commutant mark1
--   , forall x. (Monad x) => MonadExcept mark e (t x)
--   ) => MonadExcept mark e (SimpleHttpTT mark1 t m)
--   where
--     throw
--       :: mark e
--       -> SimpleHttpTT mark1 t m a
--     throw = SimpleHttpTT . OverTT . lift . liftT . throw
-- 
--     catch
--       :: SimpleHttpTT mark1 t m a
--       -> (mark e -> SimpleHttpTT mark1 t m a)
--       -> SimpleHttpTT mark1 t m a
--     catch x h = SimpleHttpTT $ OverTT $
--       liftCatch (liftCatchT catch)
--         (unOverTT $ unSimpleHttpTT x)
--         (unOverTT . unSimpleHttpTT . h)



instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadIdentity mark1, Commutant mark1
  , forall x. (Monad x) => MonadReadOnly mark r (t x)
  ) => MonadReadOnly mark r (SimpleHttpTT mark1 t m)
  where
    ask
      :: SimpleHttpTT mark1 t m (mark r)
    ask = SimpleHttpTT $ toOverTT $ lift $ liftT ask

    local
      :: (mark r -> mark r)
      -> SimpleHttpTT mark1 t m a
      -> SimpleHttpTT mark1 t m a
    local f (SimpleHttpTT x) =
      SimpleHttpTT $ toOverTT $ local f $ unOverTT x



instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadIdentity mark1, Commutant mark1, Monoid w
  , forall x. (Monad x) => MonadAppendOnly mark w (t x)
  ) => MonadAppendOnly mark w (SimpleHttpTT mark1 t m)
  where
    jot
      :: mark w
      -> SimpleHttpTT mark1 t m ()
    jot = SimpleHttpTT . toOverTT . lift . liftT . jot

    look
      :: SimpleHttpTT mark1 t m (mark w)
    look = SimpleHttpTT $ toOverTT $ lift $ liftT look



instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadIdentity mark1, Commutant mark1
  , forall x. (Monad x) => MonadWriteOnce mark w (t x)
  ) => MonadWriteOnce mark w (SimpleHttpTT mark1 t m)
  where
    etch
      :: mark w
      -> SimpleHttpTT mark1 t m Bool
    etch = SimpleHttpTT . toOverTT . lift . liftT . etch

    press
      :: SimpleHttpTT mark1 t m (Maybe (mark w))
    press = SimpleHttpTT $ toOverTT $ lift $ liftT press



-- instance
--   ( Monad m, MonadTrans t, MonadIdentity mark
--   , MonadIdentity mark1, Commutant mark1, Monoid w
--   , forall x. (Monad x) => MonadWriteOnly mark w (t x)
--   ) => MonadWriteOnly mark w (SimpleHttpTT mark1 t m)
--   where
--     tell
--       :: mark w
--       -> SimpleHttpTT mark1 t m ()
--     tell = SimpleHttpTT . OverTT . lift . liftT . tell
-- 
--     draft
--       :: SimpleHttpTT mark1 t m a
--       -> SimpleHttpTT mark1 t m (Pair (mark w) a)
--     draft = SimpleHttpTT . OverTT . draft . unOverTT . unSimpleHttpTT



instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadIdentity mark1, Commutant mark1
  , forall x. (Monad x) => MonadPrompt mark p (t x)
  ) => MonadPrompt mark p (SimpleHttpTT mark1 t m)
  where
    prompt
      :: mark (p a)
      -> SimpleHttpTT mark1 t m (mark a)
    prompt = SimpleHttpTT . toOverTT . lift . liftT . prompt



instance
  ( Monad m, MonadTrans t, MonadIdentity mark1, MonadIdentity mark
  , forall x. (Monad x) => MonadHalt mark (t x)
  ) => MonadHalt mark (SimpleHttpTT mark1 t m)
  where
    halt
      :: mark ()
      -> SimpleHttpTT mark1 t m a
    halt = SimpleHttpTT . toOverTT . lift . liftT . halt



instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadStack mark f d (t x), IsStack f
  ) => MonadStack mark f d (SimpleHttpTT mark1 t m)
  where
    push
      :: Proxy f
      -> mark d
      -> SimpleHttpTT mark1 t m ()
    push proxy = SimpleHttpTT . toOverTT . lift . liftT . push proxy

    pop
      :: Proxy f
      -> SimpleHttpTT mark1 t m (mark (Maybe d))
    pop proxy = SimpleHttpTT $ toOverTT $ lift $ liftT $ pop proxy
