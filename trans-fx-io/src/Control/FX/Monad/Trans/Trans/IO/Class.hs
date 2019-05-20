{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.Trans.Trans.IO.Class (
    MonadTeletype(..)
  , MonadSystemClock(..)
  , MonadSimpleHttp(..)
  , MonadSimpleSQLite(..)
  , SystemTime(..)
) where



import Control.FX
import Control.FX.Data

import Data.Int (Int64)
import Data.Proxy (Proxy)
import Data.Time.Clock.System ( SystemTime )
import qualified Network.HTTP.Req as Req
import qualified Database.SQLite.Simple as SQLite



-- | Class representing monads which can interact with a teletype-style
-- interface. This is an effects-only typeclass with no laws, so lifting
-- through any transformer is safe.
class
  ( Monad m, MonadIdentity mark
  ) => MonadTeletype mark m
  where
    -- | Read a line of input
    readLine :: m (mark String)

    default readLine
      :: ( Monad m1, MonadTrans t1, m ~ t1 m1
         , MonadTeletype mark m1 )
      => m (mark String)
    readLine = lift readLine

    -- | Print a line of output
    printLine :: mark String -> m ()

    default printLine
      :: ( Monad m1, MonadTrans t1, m ~ t1 m1
         , MonadTeletype mark m1 )
      => mark String
      -> m ()
    printLine = lift . printLine



instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadTeletype mark m
  ) => MonadTeletype mark (ExceptT mark1 e m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadTeletype mark m
  ) => MonadTeletype mark (ReadOnlyT mark1 r m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadTeletype mark m, Monoid w
  ) => MonadTeletype mark (WriteOnlyT mark1 w m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadTeletype mark m, Monoid w
  ) => MonadTeletype mark (AppendOnlyT mark1 w m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadTeletype mark m
  ) => MonadTeletype mark (WriteOnceT mark1 w m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadTeletype mark m
  ) => MonadTeletype mark (StateT mark1 s m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadTeletype mark m
  ) => MonadTeletype mark (HaltT mark1 m)

instance
  ( Monad m, MonadIdentity mark
  , MonadTeletype mark m
  ) => MonadTeletype mark (IdentityT m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadTeletype mark m, IsStack f
  ) => MonadTeletype mark (StackT mark1 f d m)





instance
  ( Monad m, MonadTrans t
  , MonadTeletype mark (t m)
  ) => MonadTeletype mark (IdentityTT t m)
  where
    readLine
      :: IdentityTT t m (mark String)
    readLine = IdentityTT $ readLine

    printLine
      :: mark String
      -> IdentityTT t m ()
    printLine = IdentityTT . printLine

instance
  ( Monad m, MonadTrans t, MonadIdentity mark1
  , MonadTeletype mark (t m)
  ) => MonadTeletype mark (PromptTT mark1 p t m)
  where
    readLine
      :: PromptTT mark1 p t m (mark String)
    readLine = liftT readLine

    printLine
      :: mark String
      -> PromptTT mark1 p t m ()
    printLine = liftT . printLine

instance
  ( Monad m, MonadTrans t, MonadTransTrans u, MonadFunctor w
  , MonadTeletype mark (u t m), OverableT w
  ) => MonadTeletype mark (OverTT w u t m)
  where
    readLine
      :: OverTT w u t m (mark String)
    readLine = toOverTT $ lift readLine

    printLine
      :: mark String
      -> OverTT w u t m ()
    printLine = toOverTT . lift . printLine

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadTeletype mark (t m)
  ) => MonadTeletype mark (StateTT mark1 s t m)
  where
    readLine
      :: StateTT mark1 s t m (mark String)
    readLine = StateTT $ lift readLine

    printLine
      :: mark String
      -> StateTT mark1 s t m ()
    printLine = StateTT . lift . printLine

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadTeletype mark (t m)
  ) => MonadTeletype mark (ReadOnlyTT mark1 r t m)
  where
    readLine
      :: ReadOnlyTT mark1 r t m (mark String)
    readLine = ReadOnlyTT $ lift readLine

    printLine
      :: mark String
      -> ReadOnlyTT mark1 r t m ()
    printLine = ReadOnlyTT . lift . printLine

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadTeletype mark (t m), Monoid w
  ) => MonadTeletype mark (WriteOnlyTT mark1 w t m)
  where
    readLine
      :: WriteOnlyTT mark1 w t m (mark String)
    readLine = WriteOnlyTT $ lift readLine

    printLine
      :: mark String
      -> WriteOnlyTT mark1 w t m ()
    printLine = WriteOnlyTT . lift . printLine

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadTeletype mark (t m), Monoid w
  ) => MonadTeletype mark (AppendOnlyTT mark1 w t m)
  where
    readLine
      :: AppendOnlyTT mark1 w t m (mark String)
    readLine = AppendOnlyTT $ lift readLine

    printLine
      :: mark String
      -> AppendOnlyTT mark1 w t m ()
    printLine = AppendOnlyTT . lift . printLine

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadTeletype mark (t m)
  ) => MonadTeletype mark (WriteOnceTT mark1 w t m)
  where
    readLine
      :: WriteOnceTT mark1 w t m (mark String)
    readLine = WriteOnceTT $ lift readLine

    printLine
      :: mark String
      -> WriteOnceTT mark1 w t m ()
    printLine = WriteOnceTT . lift . printLine

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadTeletype mark (t m)
  ) => MonadTeletype mark (ExceptTT mark1 e t m)
  where
    readLine
      :: ExceptTT mark1 e t m (mark String)
    readLine = ExceptTT $ lift readLine

    printLine
      :: mark String
      -> ExceptTT mark1 e t m ()
    printLine = ExceptTT . lift . printLine

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadTeletype mark (t m)
  ) => MonadTeletype mark (HaltTT mark1 t m)
  where
    readLine
      :: HaltTT mark1 t m (mark String)
    readLine = HaltTT $ lift readLine

    printLine
      :: mark String
      -> HaltTT mark1 t m ()
    printLine = HaltTT . lift . printLine

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadTeletype mark (t m)
  ) => MonadTeletype mark (StackTT mark1 f d t m)
  where
    readLine
      :: StackTT mark1 f d t m (mark String)
    readLine = StackTT $ lift readLine

    printLine
      :: mark String
      -> StackTT mark1 f d t m ()
    printLine = StackTT . lift . printLine





-- | Class representing monads which have access to the current time in UTC format.
class
  ( Monad m, MonadIdentity mark
  ) => MonadSystemClock mark m
  where
    -- | Get the current @SystemTime@
    getSystemTime :: m (mark SystemTime)

    default getSystemTime
      :: ( Monad m1, MonadTrans t1, m ~ t1 m1
         , MonadSystemClock mark m1 )
      => m (mark SystemTime)
    getSystemTime = lift getSystemTime



instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadSystemClock mark m
  ) => MonadSystemClock mark (ExceptT mark1 e m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadSystemClock mark m
  ) => MonadSystemClock mark (ReadOnlyT mark1 r m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadSystemClock mark m, Monoid w
  ) => MonadSystemClock mark (WriteOnlyT mark1 w m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadSystemClock mark m, Monoid w
  ) => MonadSystemClock mark (AppendOnlyT mark1 w m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadSystemClock mark m
  ) => MonadSystemClock mark (WriteOnceT mark1 w m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadSystemClock mark m
  ) => MonadSystemClock mark (StateT mark1 s m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadSystemClock mark m
  ) => MonadSystemClock mark (HaltT mark1 m)

instance
  ( Monad m, MonadIdentity mark
  , MonadSystemClock mark m
  ) => MonadSystemClock mark (IdentityT m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadSystemClock mark m, IsStack f
  ) => MonadSystemClock mark (StackT mark1 f d m)



instance
  ( Monad m, MonadTrans t
  , MonadSystemClock mark (t m)
  ) => MonadSystemClock mark (IdentityTT t m)
  where
    getSystemTime
      :: IdentityTT t m (mark SystemTime)
    getSystemTime = IdentityTT $ getSystemTime

instance
  ( Monad m, MonadTrans t, MonadIdentity mark1
  , MonadSystemClock mark (t m)
  ) => MonadSystemClock mark (PromptTT mark1 p t m)
  where
    getSystemTime
      :: PromptTT mark1 p t m (mark SystemTime)
    getSystemTime = liftT getSystemTime

instance
  ( Monad m, MonadTrans t, MonadTransTrans u, MonadFunctor w
  , MonadSystemClock mark (u t m), OverableT w
  ) => MonadSystemClock mark (OverTT w u t m)
  where
    getSystemTime
      :: OverTT w u t m (mark SystemTime)
    getSystemTime = toOverTT $ lift getSystemTime

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadSystemClock mark (t m)
  ) => MonadSystemClock mark (StateTT mark1 s t m)
  where
    getSystemTime
      :: StateTT mark1 s t m (mark SystemTime)
    getSystemTime = StateTT $ lift getSystemTime

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadSystemClock mark (t m)
  ) => MonadSystemClock mark (ReadOnlyTT mark1 r t m)
  where
    getSystemTime
      :: ReadOnlyTT mark1 r t m (mark SystemTime)
    getSystemTime = ReadOnlyTT $ lift getSystemTime

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadSystemClock mark (t m), Monoid w
  ) => MonadSystemClock mark (WriteOnlyTT mark1 w t m)
  where
    getSystemTime
      :: WriteOnlyTT mark1 w t m (mark SystemTime)
    getSystemTime = WriteOnlyTT $ lift getSystemTime

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadSystemClock mark (t m), Monoid w
  ) => MonadSystemClock mark (AppendOnlyTT mark1 w t m)
  where
    getSystemTime
      :: AppendOnlyTT mark1 w t m (mark SystemTime)
    getSystemTime = AppendOnlyTT $ lift getSystemTime

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadSystemClock mark (t m)
  ) => MonadSystemClock mark (WriteOnceTT mark1 w t m)
  where
    getSystemTime
      :: WriteOnceTT mark1 w t m (mark SystemTime)
    getSystemTime = WriteOnceTT $ lift getSystemTime

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadSystemClock mark (t m)
  ) => MonadSystemClock mark (ExceptTT mark1 e t m)
  where
    getSystemTime
      :: ExceptTT mark1 e t m (mark SystemTime)
    getSystemTime = ExceptTT $ lift getSystemTime

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadSystemClock mark (t m)
  ) => MonadSystemClock mark (HaltTT mark1 t m)
  where
    getSystemTime
      :: HaltTT mark1 t m (mark SystemTime)
    getSystemTime = HaltTT $ lift getSystemTime

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadSystemClock mark (t m)
  ) => MonadSystemClock mark (StackTT mark1 f d t m)
  where
    getSystemTime
      :: StackTT mark1 f d t m (mark SystemTime)
    getSystemTime = StackTT $ lift getSystemTime





-- | Class representing monads which can perform basic HTTP requests.
-- API is based on that of the @req@ library, which also provides the
-- default @IO@ evaluator.
class
  ( Monad m, MonadIdentity mark
  ) => MonadSimpleHttp mark m
  where
    -- | Perform a simple HTTP request
    simpleHttpReq
      :: ( Req.HttpMethod method, Req.HttpBody body, Req.HttpResponse response
         , Req.HttpBodyAllowed (Req.AllowsBody method) (Req.ProvidesBody body) )
      => method
      -> Req.Url scheme
      -> body
      -> Proxy response
      -> Req.Option scheme
      -> m (mark response)

    default simpleHttpReq
      :: ( Monad m1, MonadTrans t1, m ~ t1 m1, MonadSimpleHttp mark m1
         , Req.HttpMethod method, Req.HttpBody body, Req.HttpResponse response
         , Req.HttpBodyAllowed (Req.AllowsBody method) (Req.ProvidesBody body) )
      => method
      -> Req.Url scheme
      -> body
      -> Proxy response
      -> Req.Option scheme
      -> m (mark response)
    simpleHttpReq method url body resp scheme =
      lift $ simpleHttpReq method url body resp scheme



instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleHttp mark m
  ) => MonadSimpleHttp mark (ExceptT mark1 e m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleHttp mark m
  ) => MonadSimpleHttp mark (ReadOnlyT mark1 r m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleHttp mark m, Monoid w
  ) => MonadSimpleHttp mark (WriteOnlyT mark1 w m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleHttp mark m, Monoid w
  ) => MonadSimpleHttp mark (AppendOnlyT mark1 w m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleHttp mark m
  ) => MonadSimpleHttp mark (WriteOnceT mark1 w m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleHttp mark m
  ) => MonadSimpleHttp mark (StateT mark1 s m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleHttp mark m
  ) => MonadSimpleHttp mark (HaltT mark1 m)

instance
  ( Monad m, MonadIdentity mark
  , MonadSimpleHttp mark m
  ) => MonadSimpleHttp mark (IdentityT m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleHttp mark m, IsStack f
  ) => MonadSimpleHttp mark (StackT mark1 f d m)



instance
  ( Monad m, MonadTrans t
  , MonadSimpleHttp mark (t m)
  ) => MonadSimpleHttp mark (IdentityTT t m)
  where
    simpleHttpReq
      :: ( Req.HttpMethod method, Req.HttpBody body, Req.HttpResponse response
         , Req.HttpBodyAllowed (Req.AllowsBody method) (Req.ProvidesBody body) )
      => method
      -> Req.Url scheme
      -> body
      -> Proxy response
      -> Req.Option scheme
      -> IdentityTT t m (mark response)
    simpleHttpReq method scheme body response opt = IdentityTT $
      simpleHttpReq method scheme body response opt

instance
  ( Monad m, MonadTrans t, MonadIdentity mark1
  , MonadSimpleHttp mark (t m)
  ) => MonadSimpleHttp mark (PromptTT mark1 p t m)
  where
    simpleHttpReq
      :: ( Req.HttpMethod method, Req.HttpBody body, Req.HttpResponse response
         , Req.HttpBodyAllowed (Req.AllowsBody method) (Req.ProvidesBody body) )
      => method
      -> Req.Url scheme
      -> body
      -> Proxy response
      -> Req.Option scheme
      -> PromptTT mark1 p t m (mark response)
    simpleHttpReq method scheme body response opt = liftT $
      simpleHttpReq method scheme body response opt

instance
  ( Monad m, MonadTrans t, MonadTransTrans u, MonadFunctor w
  , MonadSimpleHttp mark (u t m), OverableT w
  ) => MonadSimpleHttp mark (OverTT w u t m)
  where
    simpleHttpReq
      :: ( Req.HttpMethod method, Req.HttpBody body, Req.HttpResponse response
         , Req.HttpBodyAllowed (Req.AllowsBody method) (Req.ProvidesBody body) )
      => method
      -> Req.Url scheme
      -> body
      -> Proxy response
      -> Req.Option scheme
      -> OverTT w u t m (mark response)
    simpleHttpReq method scheme body response opt =
      toOverTT $ lift $
        simpleHttpReq method scheme body response opt

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleHttp mark (t m)
  ) => MonadSimpleHttp mark (StateTT mark1 s t m)
  where
    simpleHttpReq
      :: ( Req.HttpMethod method, Req.HttpBody body, Req.HttpResponse response
         , Req.HttpBodyAllowed (Req.AllowsBody method) (Req.ProvidesBody body) )
      => method
      -> Req.Url scheme
      -> body
      -> Proxy response
      -> Req.Option scheme
      -> StateTT mark1 s t m (mark response)
    simpleHttpReq method scheme body response opt =
      StateTT $ lift $
        simpleHttpReq method scheme body response opt

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleHttp mark (t m)
  ) => MonadSimpleHttp mark (ReadOnlyTT mark1 r t m)
  where
    simpleHttpReq
      :: ( Req.HttpMethod method, Req.HttpBody body, Req.HttpResponse response
         , Req.HttpBodyAllowed (Req.AllowsBody method) (Req.ProvidesBody body) )
      => method
      -> Req.Url scheme
      -> body
      -> Proxy response
      -> Req.Option scheme
      -> ReadOnlyTT mark1 r t m (mark response)
    simpleHttpReq method scheme body response opt =
      ReadOnlyTT $ lift $
        simpleHttpReq method scheme body response opt

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleHttp mark (t m), Monoid w
  ) => MonadSimpleHttp mark (WriteOnlyTT mark1 w t m)
  where
    simpleHttpReq
      :: ( Req.HttpMethod method, Req.HttpBody body, Req.HttpResponse response
         , Req.HttpBodyAllowed (Req.AllowsBody method) (Req.ProvidesBody body) )
      => method
      -> Req.Url scheme
      -> body
      -> Proxy response
      -> Req.Option scheme
      -> WriteOnlyTT mark1 w t m (mark response)
    simpleHttpReq method scheme body response opt =
      WriteOnlyTT $ lift $
        simpleHttpReq method scheme body response opt

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleHttp mark (t m), Monoid w
  ) => MonadSimpleHttp mark (AppendOnlyTT mark1 w t m)
  where
    simpleHttpReq
      :: ( Req.HttpMethod method, Req.HttpBody body, Req.HttpResponse response
         , Req.HttpBodyAllowed (Req.AllowsBody method) (Req.ProvidesBody body) )
      => method
      -> Req.Url scheme
      -> body
      -> Proxy response
      -> Req.Option scheme
      -> AppendOnlyTT mark1 w t m (mark response)
    simpleHttpReq method scheme body response opt =
      AppendOnlyTT $ lift $
        simpleHttpReq method scheme body response opt

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleHttp mark (t m)
  ) => MonadSimpleHttp mark (WriteOnceTT mark1 w t m)
  where
    simpleHttpReq
      :: ( Req.HttpMethod method, Req.HttpBody body, Req.HttpResponse response
         , Req.HttpBodyAllowed (Req.AllowsBody method) (Req.ProvidesBody body) )
      => method
      -> Req.Url scheme
      -> body
      -> Proxy response
      -> Req.Option scheme
      -> WriteOnceTT mark1 w t m (mark response)
    simpleHttpReq method scheme body response opt =
      WriteOnceTT $ lift $
        simpleHttpReq method scheme body response opt

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleHttp mark (t m)
  ) => MonadSimpleHttp mark (ExceptTT mark1 e t m)
  where
    simpleHttpReq
      :: ( Req.HttpMethod method, Req.HttpBody body, Req.HttpResponse response
         , Req.HttpBodyAllowed (Req.AllowsBody method) (Req.ProvidesBody body) )
      => method
      -> Req.Url scheme
      -> body
      -> Proxy response
      -> Req.Option scheme
      -> ExceptTT mark1 e t m (mark response)
    simpleHttpReq method scheme body response opt =
      ExceptTT $ lift $
        simpleHttpReq method scheme body response opt

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleHttp mark (t m)
  ) => MonadSimpleHttp mark (HaltTT mark1 t m)
  where
    simpleHttpReq
      :: ( Req.HttpMethod method, Req.HttpBody body, Req.HttpResponse response
         , Req.HttpBodyAllowed (Req.AllowsBody method) (Req.ProvidesBody body) )
      => method
      -> Req.Url scheme
      -> body
      -> Proxy response
      -> Req.Option scheme
      -> HaltTT mark1 t m (mark response)
    simpleHttpReq method scheme body response opt =
      HaltTT $ lift $
        simpleHttpReq method scheme body response opt

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleHttp mark (t m)
  ) => MonadSimpleHttp mark (StackTT mark1 f d t m)
  where
    simpleHttpReq
      :: ( Req.HttpMethod method, Req.HttpBody body, Req.HttpResponse response
         , Req.HttpBodyAllowed (Req.AllowsBody method) (Req.ProvidesBody body) )
      => method
      -> Req.Url scheme
      -> body
      -> Proxy response
      -> Req.Option scheme
      -> StackTT mark1 f d t m (mark response)
    simpleHttpReq method scheme body response opt =
      StackTT $ lift $
        simpleHttpReq method scheme body response opt





-- | Class representing monads which can interact with a SQLite database.
-- API is based on that of the @sqlite-simple@ library, which also provides the
-- default @IO@ evaluator.
class
  ( Monad m, MonadIdentity mark
  ) => MonadSimpleSQLite mark m
  where
    -- Open a database connection
    simpleSQLiteOpen
      :: String
      -> m (mark SQLite.Connection)

    default simpleSQLiteOpen
      :: ( Monad m1, MonadTrans t1, m ~ t1 m1
         , MonadSimpleSQLite mark m1 )
      => String
      -> m (mark SQLite.Connection)
    simpleSQLiteOpen file =
      lift $ simpleSQLiteOpen file

    -- Close a database connection
    simpleSQLiteClose
      :: mark SQLite.Connection
      -> m ()

    default simpleSQLiteClose
      :: ( Monad m1, MonadTrans t1, m ~ t1 m1
         , MonadSimpleSQLite mark m1 )
      => mark SQLite.Connection
      -> m ()
    simpleSQLiteClose conn =
      lift $ simpleSQLiteClose conn

    -- Perform a query expected to return results
    simpleSQLiteQuery
      :: ( SQLite.ToRow q, SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> m (mark [r])

    default simpleSQLiteQuery
      :: ( Monad m1, MonadTrans t1, m ~ t1 m1
         , MonadSimpleSQLite mark m1
         , SQLite.ToRow q, SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> m (mark [r])
    simpleSQLiteQuery conn query q =
      lift $ simpleSQLiteQuery conn query q

    simpleSQLiteQuery_
      :: ( SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> m (mark [r])

    default simpleSQLiteQuery_
      :: ( Monad m1, MonadTrans t1, m ~ t1 m1
         , MonadSimpleSQLite mark m1
         , SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> m (mark [r])
    simpleSQLiteQuery_ conn query =
      lift $ simpleSQLiteQuery_ conn query

    -- Perform a query expected to return results with named parameters
    simpleSQLiteQueryNamed
      :: ( SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> m (mark [r])

    default simpleSQLiteQueryNamed
      :: ( Monad m1, MonadTrans t1, m ~ t1 m1
         , MonadSimpleSQLite mark m1
         , SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> m (mark [r])
    simpleSQLiteQueryNamed conn query params =
      lift $ simpleSQLiteQueryNamed conn query params

    simpleSQLiteLastInsertRowId
      :: SQLite.Connection
      -> m (mark Int64)

    default simpleSQLiteLastInsertRowId
      :: ( Monad m1, MonadTrans t1, m ~ t1 m1
         , MonadSimpleSQLite mark m1 )
      => SQLite.Connection
      -> m (mark Int64)
    simpleSQLiteLastInsertRowId conn =
      lift $ simpleSQLiteLastInsertRowId conn

    simpleSQLiteChanges
      :: SQLite.Connection
      -> m (mark Int)

    default simpleSQLiteChanges
      :: ( Monad m1, MonadTrans t1, m ~ t1 m1
         , MonadSimpleSQLite mark m1 )
      => SQLite.Connection
      -> m (mark Int)
    simpleSQLiteChanges conn =
      lift $ simpleSQLiteChanges conn

    simpleSQLiteExecute
      :: ( SQLite.ToRow q )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> m (mark ())

    default simpleSQLiteExecute
      :: ( Monad m1, MonadTrans t1, m ~ t1 m1
         , MonadSimpleSQLite mark m1
         , SQLite.ToRow q )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> m (mark ())
    simpleSQLiteExecute conn query q =
      lift $ simpleSQLiteExecute conn query q

    simpleSQLiteExecute_
      :: SQLite.Connection
      -> SQLite.Query
      -> m (mark ())

    default simpleSQLiteExecute_
      :: ( Monad m1, MonadTrans t1, m ~ t1 m1
         , MonadSimpleSQLite mark m1 )
      => SQLite.Connection
      -> SQLite.Query
      -> m (mark ())
    simpleSQLiteExecute_ conn query =
      lift $ simpleSQLiteExecute_ conn query

    simpleSQLiteExecuteNamed
      :: SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> m (mark ())

    default simpleSQLiteExecuteNamed
      :: ( Monad m1, MonadTrans t1, m ~ t1 m1
         , MonadSimpleSQLite mark m1 )
      => SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> m (mark ())
    simpleSQLiteExecuteNamed conn query params =
      lift $ simpleSQLiteExecuteNamed conn query params



instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleSQLite mark m
  ) => MonadSimpleSQLite mark (ExceptT mark1 e m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleSQLite mark m
  ) => MonadSimpleSQLite mark (ReadOnlyT mark1 r m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleSQLite mark m, Monoid w
  ) => MonadSimpleSQLite mark (WriteOnlyT mark1 w m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleSQLite mark m, Monoid w
  ) => MonadSimpleSQLite mark (AppendOnlyT mark1 w m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleSQLite mark m
  ) => MonadSimpleSQLite mark (WriteOnceT mark1 w m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleSQLite mark m
  ) => MonadSimpleSQLite mark (StateT mark1 s m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleSQLite mark m
  ) => MonadSimpleSQLite mark (HaltT mark1 m)

instance
  ( Monad m, MonadIdentity mark
  , MonadSimpleSQLite mark m
  ) => MonadSimpleSQLite mark (IdentityT m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleSQLite mark m, IsStack f
  ) => MonadSimpleSQLite mark (StackT mark1 f d m)



instance
  ( Monad m, MonadTrans t
  , MonadSimpleSQLite mark (t m)
  ) => MonadSimpleSQLite mark (IdentityTT t m)
  where
    simpleSQLiteOpen
      :: String
      -> IdentityTT t m (mark SQLite.Connection)
    simpleSQLiteOpen conn =
      IdentityTT $ simpleSQLiteOpen conn

    simpleSQLiteClose
      :: mark SQLite.Connection
      -> IdentityTT t m ()
    simpleSQLiteClose conn =
      IdentityTT $ simpleSQLiteClose conn

    simpleSQLiteQuery
      :: ( SQLite.ToRow q, SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> IdentityTT t m (mark [r])
    simpleSQLiteQuery conn query q =
      IdentityTT $ simpleSQLiteQuery conn query q

    simpleSQLiteQuery_
      :: ( SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> IdentityTT t m (mark [r])
    simpleSQLiteQuery_ conn query =
      IdentityTT $ simpleSQLiteQuery_ conn query

    simpleSQLiteQueryNamed
      :: ( SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> IdentityTT t m (mark [r])
    simpleSQLiteQueryNamed conn query params =
      IdentityTT $ simpleSQLiteQueryNamed conn query params

    simpleSQLiteLastInsertRowId
      :: SQLite.Connection
      -> IdentityTT t m (mark Int64)
    simpleSQLiteLastInsertRowId conn =
      IdentityTT $ simpleSQLiteLastInsertRowId conn

    simpleSQLiteChanges
      :: SQLite.Connection
      -> IdentityTT t m (mark Int)
    simpleSQLiteChanges conn =
      IdentityTT $ simpleSQLiteChanges conn

    simpleSQLiteExecute
      :: ( SQLite.ToRow q )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> IdentityTT t m (mark ())
    simpleSQLiteExecute conn query params =
      IdentityTT $ simpleSQLiteExecute conn query params

    simpleSQLiteExecute_
      :: SQLite.Connection
      -> SQLite.Query
      -> IdentityTT t m (mark ())
    simpleSQLiteExecute_ conn query =
      IdentityTT $ simpleSQLiteExecute_ conn query

    simpleSQLiteExecuteNamed
      :: SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> IdentityTT t m (mark ())
    simpleSQLiteExecuteNamed conn query params =
      IdentityTT $ simpleSQLiteExecuteNamed conn query params

instance
  ( Monad m, MonadTrans t, MonadIdentity mark1
  , MonadSimpleSQLite mark (t m)
  ) => MonadSimpleSQLite mark (PromptTT mark1 p t m)
  where
    simpleSQLiteOpen
      :: String
      -> PromptTT mark1 p t m (mark SQLite.Connection)
    simpleSQLiteOpen conn =
      liftT $ simpleSQLiteOpen conn

    simpleSQLiteClose
      :: mark SQLite.Connection
      -> PromptTT mark1 p t m ()
    simpleSQLiteClose conn =
      liftT $ simpleSQLiteClose conn

    simpleSQLiteQuery
      :: ( SQLite.ToRow q, SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> PromptTT mark1 p t m (mark [r])
    simpleSQLiteQuery conn query q =
      liftT $ simpleSQLiteQuery conn query q

    simpleSQLiteQuery_
      :: ( SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> PromptTT mark1 p t m (mark [r])
    simpleSQLiteQuery_ conn query =
      liftT $ simpleSQLiteQuery_ conn query

    simpleSQLiteQueryNamed
      :: ( SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> PromptTT mark1 p t m (mark [r])
    simpleSQLiteQueryNamed conn query params =
      liftT $ simpleSQLiteQueryNamed conn query params

    simpleSQLiteLastInsertRowId
      :: SQLite.Connection
      -> PromptTT mark1 p t m (mark Int64)
    simpleSQLiteLastInsertRowId conn =
      liftT $ simpleSQLiteLastInsertRowId conn

    simpleSQLiteChanges
      :: SQLite.Connection
      -> PromptTT mark1 p t m (mark Int)
    simpleSQLiteChanges conn =
      liftT $ simpleSQLiteChanges conn

    simpleSQLiteExecute
      :: ( SQLite.ToRow q )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> PromptTT mark1 p t m (mark ())
    simpleSQLiteExecute conn query params =
      liftT $ simpleSQLiteExecute conn query params

    simpleSQLiteExecute_
      :: SQLite.Connection
      -> SQLite.Query
      -> PromptTT mark1 p t m (mark ())
    simpleSQLiteExecute_ conn query =
      liftT $ simpleSQLiteExecute_ conn query

    simpleSQLiteExecuteNamed
      :: SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> PromptTT mark1 p t m (mark ())
    simpleSQLiteExecuteNamed conn query params =
      liftT $ simpleSQLiteExecuteNamed conn query params

instance
  ( Monad m, MonadTrans t, MonadTransTrans u, MonadFunctor w
  , MonadSimpleSQLite mark (u t m), OverableT w
  ) => MonadSimpleSQLite mark (OverTT w u t m)
  where
    simpleSQLiteOpen
      :: String
      -> OverTT w u t m (mark SQLite.Connection)
    simpleSQLiteOpen conn =
      toOverTT $ lift $ simpleSQLiteOpen conn

    simpleSQLiteClose
      :: mark SQLite.Connection
      -> OverTT w u t m ()
    simpleSQLiteClose conn =
      toOverTT $ lift $ simpleSQLiteClose conn

    simpleSQLiteQuery
      :: ( SQLite.ToRow q, SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> OverTT w u t m (mark [r])
    simpleSQLiteQuery conn query q =
      toOverTT $ lift $ simpleSQLiteQuery conn query q

    simpleSQLiteQuery_
      :: ( SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> OverTT w u t m (mark [r])
    simpleSQLiteQuery_ conn query =
      toOverTT $ lift $ simpleSQLiteQuery_ conn query

    simpleSQLiteQueryNamed
      :: ( SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> OverTT w u t m (mark [r])
    simpleSQLiteQueryNamed conn query params =
      toOverTT $ lift $ simpleSQLiteQueryNamed conn query params

    simpleSQLiteLastInsertRowId
      :: SQLite.Connection
      -> OverTT w u t m (mark Int64)
    simpleSQLiteLastInsertRowId conn =
      toOverTT $ lift $ simpleSQLiteLastInsertRowId conn

    simpleSQLiteChanges
      :: SQLite.Connection
      -> OverTT w u t m (mark Int)
    simpleSQLiteChanges conn =
      toOverTT $ lift $ simpleSQLiteChanges conn

    simpleSQLiteExecute
      :: ( SQLite.ToRow q )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> OverTT w u t m (mark ())
    simpleSQLiteExecute conn query params =
      toOverTT $ lift $ simpleSQLiteExecute conn query params

    simpleSQLiteExecute_
      :: SQLite.Connection
      -> SQLite.Query
      -> OverTT w u t m (mark ())
    simpleSQLiteExecute_ conn query =
      toOverTT $ lift $ simpleSQLiteExecute_ conn query

    simpleSQLiteExecuteNamed
      :: SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> OverTT w u t m (mark ())
    simpleSQLiteExecuteNamed conn query params =
      toOverTT $ lift $ simpleSQLiteExecuteNamed conn query params

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleSQLite mark (t m)
  ) => MonadSimpleSQLite mark (StateTT mark1 s t m)
  where
    simpleSQLiteOpen
      :: String
      -> StateTT mark1 s t m (mark SQLite.Connection)
    simpleSQLiteOpen conn =
      StateTT $ lift $ simpleSQLiteOpen conn

    simpleSQLiteClose
      :: mark SQLite.Connection
      -> StateTT mark1 s t m ()
    simpleSQLiteClose conn =
      StateTT $ lift $ simpleSQLiteClose conn

    simpleSQLiteQuery
      :: ( SQLite.ToRow q, SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> StateTT mark1 s t m (mark [r])
    simpleSQLiteQuery conn query q =
      StateTT $ lift $ simpleSQLiteQuery conn query q

    simpleSQLiteQuery_
      :: ( SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> StateTT mark1 s t m (mark [r])
    simpleSQLiteQuery_ conn query =
      StateTT $ lift $ simpleSQLiteQuery_ conn query

    simpleSQLiteQueryNamed
      :: ( SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> StateTT mark1 s t m (mark [r])
    simpleSQLiteQueryNamed conn query params =
      StateTT $ lift $ simpleSQLiteQueryNamed conn query params

    simpleSQLiteLastInsertRowId
      :: SQLite.Connection
      -> StateTT mark1 s t m (mark Int64)
    simpleSQLiteLastInsertRowId conn =
      StateTT $ lift $ simpleSQLiteLastInsertRowId conn

    simpleSQLiteChanges
      :: SQLite.Connection
      -> StateTT mark1 s t m (mark Int)
    simpleSQLiteChanges conn =
      StateTT $ lift $ simpleSQLiteChanges conn

    simpleSQLiteExecute
      :: ( SQLite.ToRow q )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> StateTT mark1 s t m (mark ())
    simpleSQLiteExecute conn query params =
      StateTT $ lift $ simpleSQLiteExecute conn query params

    simpleSQLiteExecute_
      :: SQLite.Connection
      -> SQLite.Query
      -> StateTT mark1 s t m (mark ())
    simpleSQLiteExecute_ conn query =
      StateTT $ lift $ simpleSQLiteExecute_ conn query

    simpleSQLiteExecuteNamed
      :: SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> StateTT mark1 s t m (mark ())
    simpleSQLiteExecuteNamed conn query params =
      StateTT $ lift $ simpleSQLiteExecuteNamed conn query params

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleSQLite mark (t m)
  ) => MonadSimpleSQLite mark (ReadOnlyTT mark1 r t m)
  where
    simpleSQLiteOpen
      :: String
      -> ReadOnlyTT mark1 r t m (mark SQLite.Connection)
    simpleSQLiteOpen conn =
      ReadOnlyTT $ lift $ simpleSQLiteOpen conn

    simpleSQLiteClose
      :: mark SQLite.Connection
      -> ReadOnlyTT mark1 r t m ()
    simpleSQLiteClose conn =
      ReadOnlyTT $ lift $ simpleSQLiteClose conn

    simpleSQLiteQuery
      :: ( SQLite.ToRow q, SQLite.FromRow row )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> ReadOnlyTT mark1 r t m (mark [row])
    simpleSQLiteQuery conn query q =
      ReadOnlyTT $ lift $ simpleSQLiteQuery conn query q

    simpleSQLiteQuery_
      :: ( SQLite.FromRow row )
      => SQLite.Connection
      -> SQLite.Query
      -> ReadOnlyTT mark1 r t m (mark [row])
    simpleSQLiteQuery_ conn query =
      ReadOnlyTT $ lift $ simpleSQLiteQuery_ conn query

    simpleSQLiteQueryNamed
      :: ( SQLite.FromRow row )
      => SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> ReadOnlyTT mark1 r t m (mark [row])
    simpleSQLiteQueryNamed conn query params =
      ReadOnlyTT $ lift $ simpleSQLiteQueryNamed conn query params

    simpleSQLiteLastInsertRowId
      :: SQLite.Connection
      -> ReadOnlyTT mark1 r t m (mark Int64)
    simpleSQLiteLastInsertRowId conn =
      ReadOnlyTT $ lift $ simpleSQLiteLastInsertRowId conn

    simpleSQLiteChanges
      :: SQLite.Connection
      -> ReadOnlyTT mark1 r t m (mark Int)
    simpleSQLiteChanges conn =
      ReadOnlyTT $ lift $ simpleSQLiteChanges conn

    simpleSQLiteExecute
      :: ( SQLite.ToRow q )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> ReadOnlyTT mark1 r t m (mark ())
    simpleSQLiteExecute conn query params =
      ReadOnlyTT $ lift $ simpleSQLiteExecute conn query params

    simpleSQLiteExecute_
      :: SQLite.Connection
      -> SQLite.Query
      -> ReadOnlyTT mark1 r t m (mark ())
    simpleSQLiteExecute_ conn query =
      ReadOnlyTT $ lift $ simpleSQLiteExecute_ conn query

    simpleSQLiteExecuteNamed
      :: SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> ReadOnlyTT mark1 r t m (mark ())
    simpleSQLiteExecuteNamed conn query params =
      ReadOnlyTT $ lift $ simpleSQLiteExecuteNamed conn query params

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleSQLite mark (t m), Monoid w
  ) => MonadSimpleSQLite mark (WriteOnlyTT mark1 w t m)
  where
    simpleSQLiteOpen
      :: String
      -> WriteOnlyTT mark1 w t m (mark SQLite.Connection)
    simpleSQLiteOpen conn =
      WriteOnlyTT $ lift $ simpleSQLiteOpen conn

    simpleSQLiteClose
      :: mark SQLite.Connection
      -> WriteOnlyTT mark1 w t m ()
    simpleSQLiteClose conn =
      WriteOnlyTT $ lift $ simpleSQLiteClose conn

    simpleSQLiteQuery
      :: ( SQLite.ToRow q, SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> WriteOnlyTT mark1 w t m (mark [r])
    simpleSQLiteQuery conn query q =
      WriteOnlyTT $ lift $ simpleSQLiteQuery conn query q

    simpleSQLiteQuery_
      :: ( SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> WriteOnlyTT mark1 w t m (mark [r])
    simpleSQLiteQuery_ conn query =
      WriteOnlyTT $ lift $ simpleSQLiteQuery_ conn query

    simpleSQLiteQueryNamed
      :: ( SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> WriteOnlyTT mark1 w t m (mark [r])
    simpleSQLiteQueryNamed conn query params =
      WriteOnlyTT $ lift $ simpleSQLiteQueryNamed conn query params

    simpleSQLiteLastInsertRowId
      :: SQLite.Connection
      -> WriteOnlyTT mark1 w t m (mark Int64)
    simpleSQLiteLastInsertRowId conn =
      WriteOnlyTT $ lift $ simpleSQLiteLastInsertRowId conn

    simpleSQLiteChanges
      :: SQLite.Connection
      -> WriteOnlyTT mark1 w t m (mark Int)
    simpleSQLiteChanges conn =
      WriteOnlyTT $ lift $ simpleSQLiteChanges conn

    simpleSQLiteExecute
      :: ( SQLite.ToRow q )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> WriteOnlyTT mark1 w t m (mark ())
    simpleSQLiteExecute conn query params =
      WriteOnlyTT $ lift $ simpleSQLiteExecute conn query params

    simpleSQLiteExecute_
      :: SQLite.Connection
      -> SQLite.Query
      -> WriteOnlyTT mark1 w t m (mark ())
    simpleSQLiteExecute_ conn query =
      WriteOnlyTT $ lift $ simpleSQLiteExecute_ conn query

    simpleSQLiteExecuteNamed
      :: SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> WriteOnlyTT mark1 w t m (mark ())
    simpleSQLiteExecuteNamed conn query params =
      WriteOnlyTT $ lift $ simpleSQLiteExecuteNamed conn query params

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleSQLite mark (t m), Monoid w
  ) => MonadSimpleSQLite mark (AppendOnlyTT mark1 w t m)
  where
    simpleSQLiteOpen
      :: String
      -> AppendOnlyTT mark1 w t m (mark SQLite.Connection)
    simpleSQLiteOpen conn =
      AppendOnlyTT $ lift $ simpleSQLiteOpen conn

    simpleSQLiteClose
      :: mark SQLite.Connection
      -> AppendOnlyTT mark1 w t m ()
    simpleSQLiteClose conn =
      AppendOnlyTT $ lift $ simpleSQLiteClose conn

    simpleSQLiteQuery
      :: ( SQLite.ToRow q, SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> AppendOnlyTT mark1 w t m (mark [r])
    simpleSQLiteQuery conn query q =
      AppendOnlyTT $ lift $ simpleSQLiteQuery conn query q

    simpleSQLiteQuery_
      :: ( SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> AppendOnlyTT mark1 w t m (mark [r])
    simpleSQLiteQuery_ conn query =
      AppendOnlyTT $ lift $ simpleSQLiteQuery_ conn query

    simpleSQLiteQueryNamed
      :: ( SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> AppendOnlyTT mark1 w t m (mark [r])
    simpleSQLiteQueryNamed conn query params =
      AppendOnlyTT $ lift $ simpleSQLiteQueryNamed conn query params

    simpleSQLiteLastInsertRowId
      :: SQLite.Connection
      -> AppendOnlyTT mark1 w t m (mark Int64)
    simpleSQLiteLastInsertRowId conn =
      AppendOnlyTT $ lift $ simpleSQLiteLastInsertRowId conn

    simpleSQLiteChanges
      :: SQLite.Connection
      -> AppendOnlyTT mark1 w t m (mark Int)
    simpleSQLiteChanges conn =
      AppendOnlyTT $ lift $ simpleSQLiteChanges conn

    simpleSQLiteExecute
      :: ( SQLite.ToRow q )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> AppendOnlyTT mark1 w t m (mark ())
    simpleSQLiteExecute conn query params =
      AppendOnlyTT $ lift $ simpleSQLiteExecute conn query params

    simpleSQLiteExecute_
      :: SQLite.Connection
      -> SQLite.Query
      -> AppendOnlyTT mark1 w t m (mark ())
    simpleSQLiteExecute_ conn query =
      AppendOnlyTT $ lift $ simpleSQLiteExecute_ conn query

    simpleSQLiteExecuteNamed
      :: SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> AppendOnlyTT mark1 w t m (mark ())
    simpleSQLiteExecuteNamed conn query params =
      AppendOnlyTT $ lift $ simpleSQLiteExecuteNamed conn query params

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleSQLite mark (t m)
  ) => MonadSimpleSQLite mark (WriteOnceTT mark1 w t m)
  where
    simpleSQLiteOpen
      :: String
      -> WriteOnceTT mark1 w t m (mark SQLite.Connection)
    simpleSQLiteOpen conn =
      WriteOnceTT $ lift $ simpleSQLiteOpen conn

    simpleSQLiteClose
      :: mark SQLite.Connection
      -> WriteOnceTT mark1 w t m ()
    simpleSQLiteClose conn =
      WriteOnceTT $ lift $ simpleSQLiteClose conn

    simpleSQLiteQuery
      :: ( SQLite.ToRow q, SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> WriteOnceTT mark1 w t m (mark [r])
    simpleSQLiteQuery conn query q =
      WriteOnceTT $ lift $ simpleSQLiteQuery conn query q

    simpleSQLiteQuery_
      :: ( SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> WriteOnceTT mark1 w t m (mark [r])
    simpleSQLiteQuery_ conn query =
      WriteOnceTT $ lift $ simpleSQLiteQuery_ conn query

    simpleSQLiteQueryNamed
      :: ( SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> WriteOnceTT mark1 w t m (mark [r])
    simpleSQLiteQueryNamed conn query params =
      WriteOnceTT $ lift $ simpleSQLiteQueryNamed conn query params

    simpleSQLiteLastInsertRowId
      :: SQLite.Connection
      -> WriteOnceTT mark1 w t m (mark Int64)
    simpleSQLiteLastInsertRowId conn =
      WriteOnceTT $ lift $ simpleSQLiteLastInsertRowId conn

    simpleSQLiteChanges
      :: SQLite.Connection
      -> WriteOnceTT mark1 w t m (mark Int)
    simpleSQLiteChanges conn =
      WriteOnceTT $ lift $ simpleSQLiteChanges conn

    simpleSQLiteExecute
      :: ( SQLite.ToRow q )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> WriteOnceTT mark1 w t m (mark ())
    simpleSQLiteExecute conn query params =
      WriteOnceTT $ lift $ simpleSQLiteExecute conn query params

    simpleSQLiteExecute_
      :: SQLite.Connection
      -> SQLite.Query
      -> WriteOnceTT mark1 w t m (mark ())
    simpleSQLiteExecute_ conn query =
      WriteOnceTT $ lift $ simpleSQLiteExecute_ conn query

    simpleSQLiteExecuteNamed
      :: SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> WriteOnceTT mark1 w t m (mark ())
    simpleSQLiteExecuteNamed conn query params =
      WriteOnceTT $ lift $ simpleSQLiteExecuteNamed conn query params

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleSQLite mark (t m)
  ) => MonadSimpleSQLite mark (ExceptTT mark1 e t m)
  where
    simpleSQLiteOpen
      :: String
      -> ExceptTT mark1 e t m (mark SQLite.Connection)
    simpleSQLiteOpen conn =
      ExceptTT $ lift $ simpleSQLiteOpen conn

    simpleSQLiteClose
      :: mark SQLite.Connection
      -> ExceptTT mark1 e t m ()
    simpleSQLiteClose conn =
      ExceptTT $ lift $ simpleSQLiteClose conn

    simpleSQLiteQuery
      :: ( SQLite.ToRow q, SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> ExceptTT mark1 e t m (mark [r])
    simpleSQLiteQuery conn query q =
      ExceptTT $ lift $ simpleSQLiteQuery conn query q

    simpleSQLiteQuery_
      :: ( SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> ExceptTT mark1 e t m (mark [r])
    simpleSQLiteQuery_ conn query =
      ExceptTT $ lift $ simpleSQLiteQuery_ conn query

    simpleSQLiteQueryNamed
      :: ( SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> ExceptTT mark1 e t m (mark [r])
    simpleSQLiteQueryNamed conn query params =
      ExceptTT $ lift $ simpleSQLiteQueryNamed conn query params

    simpleSQLiteLastInsertRowId
      :: SQLite.Connection
      -> ExceptTT mark1 e t m (mark Int64)
    simpleSQLiteLastInsertRowId conn =
      ExceptTT $ lift $ simpleSQLiteLastInsertRowId conn

    simpleSQLiteChanges
      :: SQLite.Connection
      -> ExceptTT mark1 e t m (mark Int)
    simpleSQLiteChanges conn =
      ExceptTT $ lift $ simpleSQLiteChanges conn

    simpleSQLiteExecute
      :: ( SQLite.ToRow q )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> ExceptTT mark1 e t m (mark ())
    simpleSQLiteExecute conn query params =
      ExceptTT $ lift $ simpleSQLiteExecute conn query params

    simpleSQLiteExecute_
      :: SQLite.Connection
      -> SQLite.Query
      -> ExceptTT mark1 e t m (mark ())
    simpleSQLiteExecute_ conn query =
      ExceptTT $ lift $ simpleSQLiteExecute_ conn query

    simpleSQLiteExecuteNamed
      :: SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> ExceptTT mark1 e t m (mark ())
    simpleSQLiteExecuteNamed conn query params =
      ExceptTT $ lift $ simpleSQLiteExecuteNamed conn query params

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleSQLite mark (t m)
  ) => MonadSimpleSQLite mark (HaltTT mark1 t m)
  where
    simpleSQLiteOpen
      :: String
      -> HaltTT mark1 t m (mark SQLite.Connection)
    simpleSQLiteOpen conn =
      HaltTT $ lift $ simpleSQLiteOpen conn

    simpleSQLiteClose
      :: mark SQLite.Connection
      -> HaltTT mark1 t m ()
    simpleSQLiteClose conn =
      HaltTT $ lift $ simpleSQLiteClose conn

    simpleSQLiteQuery
      :: ( SQLite.ToRow q, SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> HaltTT mark1 t m (mark [r])
    simpleSQLiteQuery conn query q =
      HaltTT $ lift $ simpleSQLiteQuery conn query q

    simpleSQLiteQuery_
      :: ( SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> HaltTT mark1 t m (mark [r])
    simpleSQLiteQuery_ conn query =
      HaltTT $ lift $ simpleSQLiteQuery_ conn query

    simpleSQLiteQueryNamed
      :: ( SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> HaltTT mark1 t m (mark [r])
    simpleSQLiteQueryNamed conn query params =
      HaltTT $ lift $ simpleSQLiteQueryNamed conn query params

    simpleSQLiteLastInsertRowId
      :: SQLite.Connection
      -> HaltTT mark1 t m (mark Int64)
    simpleSQLiteLastInsertRowId conn =
      HaltTT $ lift $ simpleSQLiteLastInsertRowId conn

    simpleSQLiteChanges
      :: SQLite.Connection
      -> HaltTT mark1 t m (mark Int)
    simpleSQLiteChanges conn =
      HaltTT $ lift $ simpleSQLiteChanges conn

    simpleSQLiteExecute
      :: ( SQLite.ToRow q )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> HaltTT mark1 t m (mark ())
    simpleSQLiteExecute conn query params =
      HaltTT $ lift $ simpleSQLiteExecute conn query params

    simpleSQLiteExecute_
      :: SQLite.Connection
      -> SQLite.Query
      -> HaltTT mark1 t m (mark ())
    simpleSQLiteExecute_ conn query =
      HaltTT $ lift $ simpleSQLiteExecute_ conn query

    simpleSQLiteExecuteNamed
      :: SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> HaltTT mark1 t m (mark ())
    simpleSQLiteExecuteNamed conn query params =
      HaltTT $ lift $ simpleSQLiteExecuteNamed conn query params

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , MonadSimpleSQLite mark (t m)
  ) => MonadSimpleSQLite mark (StackTT mark1 f d t m)
  where
    simpleSQLiteOpen
      :: String
      -> StackTT mark1 f d t m (mark SQLite.Connection)
    simpleSQLiteOpen conn =
      StackTT $ lift $ simpleSQLiteOpen conn

    simpleSQLiteClose
      :: mark SQLite.Connection
      -> StackTT mark1 f d t m ()
    simpleSQLiteClose conn =
      StackTT $ lift $ simpleSQLiteClose conn

    simpleSQLiteQuery
      :: ( SQLite.ToRow q, SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> StackTT mark1 f d t m (mark [r])
    simpleSQLiteQuery conn query q =
      StackTT $ lift $ simpleSQLiteQuery conn query q

    simpleSQLiteQuery_
      :: ( SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> StackTT mark1 f d t m (mark [r])
    simpleSQLiteQuery_ conn query =
      StackTT $ lift $ simpleSQLiteQuery_ conn query

    simpleSQLiteQueryNamed
      :: ( SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> StackTT mark1 f d t m (mark [r])
    simpleSQLiteQueryNamed conn query params =
      StackTT $ lift $ simpleSQLiteQueryNamed conn query params

    simpleSQLiteLastInsertRowId
      :: SQLite.Connection
      -> StackTT mark1 f d t m (mark Int64)
    simpleSQLiteLastInsertRowId conn =
      StackTT $ lift $ simpleSQLiteLastInsertRowId conn

    simpleSQLiteChanges
      :: SQLite.Connection
      -> StackTT mark1 f d t m (mark Int)
    simpleSQLiteChanges conn =
      StackTT $ lift $ simpleSQLiteChanges conn

    simpleSQLiteExecute
      :: ( SQLite.ToRow q )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> StackTT mark1 f d t m (mark ())
    simpleSQLiteExecute conn query params =
      StackTT $ lift $ simpleSQLiteExecute conn query params

    simpleSQLiteExecute_
      :: SQLite.Connection
      -> SQLite.Query
      -> StackTT mark1 f d t m (mark ())
    simpleSQLiteExecute_ conn query =
      StackTT $ lift $ simpleSQLiteExecute_ conn query

    simpleSQLiteExecuteNamed
      :: SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> StackTT mark1 f d t m (mark ())
    simpleSQLiteExecuteNamed conn query params =
      StackTT $ lift $ simpleSQLiteExecuteNamed conn query params
