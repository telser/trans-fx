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
  , SystemTime(..)
) where



import Control.FX
import Control.FX.Data

import Data.Proxy (Proxy)
import Data.Time.Clock.System ( SystemTime )
import qualified Network.HTTP.Req as Req



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
