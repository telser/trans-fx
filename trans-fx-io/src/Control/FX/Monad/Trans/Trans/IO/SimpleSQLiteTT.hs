-- | Module      : Control.FX.IO.Monad.Trans.Trans.SimpleSQLiteTT
--   Description : Simple SQLite monad transformer transformer
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

module Control.FX.Monad.Trans.Trans.IO.SimpleSQLiteTT (
    SimpleSQLiteTT(..)
  , SimpleSQLiteAction(..)
  , evalSimpleSQLiteIO
  , SimpleSQLiteError(..)
  , SimpleSQLiteException(..)
  , runSimpleSQLiteTT
  , Context(..)
  , InputTT(..)
  , OutputTT(..)
) where



import Data.Int
  ( Int64 )
import Data.Typeable
  ( Typeable, Proxy, typeOf )
import Control.Exception
  ( catches, Handler(..) )
import Data.Time.Clock.System
  ( SystemTime )
import System.IO
  ( Handle, hPutStrLn, hGetLine )
import qualified Network.HTTP.Req as Req
import qualified Database.SQLite.Simple as SQLite

import Control.FX
import Control.FX.Data
import Control.FX.Monad.Trans.Trans.IO.Class



-- | SQLite monad transformer transformer
newtype SimpleSQLiteTT
  (mark :: * -> *)
  (t :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = SimpleSQLiteTT
        { unSimpleSQLiteTT
            :: OverTT
                ( ExceptT SimpleSQLiteError (mark SimpleSQLiteException) )
                ( PromptTT mark (SimpleSQLiteAction mark) )
                t m a
        } deriving
          ( Typeable, Functor, Applicative
          , Monad, MonadTrans, MonadTransTrans
          , MonadPrompt mark (SimpleSQLiteAction mark) )

deriving instance {-# OVERLAPPING #-}
  ( Monad m, MonadTrans t, MonadIdentity mark
  ) => MonadExcept SimpleSQLiteError (mark SimpleSQLiteException) (SimpleSQLiteTT mark t m)

instance
  ( Typeable mark, Typeable t, Typeable m, Typeable a
  ) => Show (SimpleSQLiteTT mark t m a)
  where
    show
      :: SimpleSQLiteTT mark t m a
      -> String
    show = show . typeOf



data SimpleSQLiteError
  (a :: *)
    = SimpleSQLiteError
      { unSimpleSQLiteError :: a
      } deriving (Eq, Show, Typeable)

instance Functor SimpleSQLiteError where
  fmap f (SimpleSQLiteError a) = SimpleSQLiteError (f a)

instance Applicative SimpleSQLiteError where
  pure = SimpleSQLiteError
  (SimpleSQLiteError f) <*> (SimpleSQLiteError x) =
    SimpleSQLiteError (f x)

instance Monad SimpleSQLiteError where
  return = SimpleSQLiteError
  (SimpleSQLiteError x) >>= f = f x

instance
  ( Semigroup a
  ) => Semigroup (SimpleSQLiteError a)
  where
    (<>)
      :: SimpleSQLiteError a
      -> SimpleSQLiteError a
      -> SimpleSQLiteError a
    (SimpleSQLiteError a) <> (SimpleSQLiteError b) =
      SimpleSQLiteError (a <> b)

instance
  ( Monoid a
  ) => Monoid (SimpleSQLiteError a)
  where
    mempty
      :: SimpleSQLiteError a
    mempty = SimpleSQLiteError mempty

instance MonadIdentity SimpleSQLiteError where
  unwrap = unSimpleSQLiteError



data SimpleSQLiteException
  = SQLiteFormatError SQLite.FormatError
  | SQLiteResultError SQLite.ResultError
  | SQLiteSQLError    SQLite.SQLError
  deriving (Eq, Show)



instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , Commutant mark, EqIn (t m)
  ) => EqIn (SimpleSQLiteTT mark t m)
  where
    newtype Context (SimpleSQLiteTT mark t m)
      = SimpleSQLiteTTCtx
          { unSimpleSQLiteTTCtx :: (Eval (SimpleSQLiteAction mark) m, Context (t m))
          } deriving (Typeable)

    eqIn
      :: (Eq a)
      => Context (SimpleSQLiteTT mark t m)
      -> SimpleSQLiteTT mark t m a
      -> SimpleSQLiteTT mark t m a
      -> Bool
    eqIn (SimpleSQLiteTTCtx (eval,h)) x y =
      eqIn h
        (fmap unSimpleSQLiteTTOut $ runTT (SimpleSQLiteTTIn eval) x)
        (fmap unSimpleSQLiteTTOut $ runTT (SimpleSQLiteTTIn eval) y)

instance
  ( Typeable mark, Typeable t, Typeable m
  ) => Show (Context (SimpleSQLiteTT mark t m))
  where
    show = show . typeOf



instance
  ( MonadIdentity mark, Commutant mark
  ) => RunMonadTransTrans (SimpleSQLiteTT mark)
  where
    newtype InputTT (SimpleSQLiteTT mark) m
      = SimpleSQLiteTTIn
          { unSimpleSQLiteTTIn :: Eval (SimpleSQLiteAction mark) m
          } deriving (Typeable)

    newtype OutputTT (SimpleSQLiteTT mark) a
      = SimpleSQLiteTTOut
          { unSimpleSQLiteTTOut :: Except SimpleSQLiteError (mark SimpleSQLiteException) a
          } deriving (Typeable)

    runTT
      :: ( Monad m, MonadTrans t )
      => InputTT (SimpleSQLiteTT mark) m
      -> SimpleSQLiteTT mark t m a
      -> t m (OutputTT (SimpleSQLiteTT mark) a)
    runTT (SimpleSQLiteTTIn eval) (SimpleSQLiteTT x) =
      fmap (SimpleSQLiteTTOut . unExceptTOut . unwrap . unCompose . unOverTTOut) $
        runTT (OverTTIn (PromptTTIn eval, ExceptTIn (pure ()))) x

instance
  ( Typeable mark, Typeable m
  ) => Show (InputTT (SimpleSQLiteTT mark) m)
  where
    show = show . typeOf

deriving instance
  ( Show a, Show (mark SimpleSQLiteException)
  ) => Show (OutputTT (SimpleSQLiteTT mark) a)

runSimpleSQLiteTT
  :: ( Monad m, MonadTrans t, MonadIdentity mark, Commutant mark )
  => Eval (SimpleSQLiteAction mark) m
  -> SimpleSQLiteTT mark t m a
  -> t m (Except SimpleSQLiteError (mark SimpleSQLiteException) a)
runSimpleSQLiteTT p = fmap unSimpleSQLiteTTOut . runTT (SimpleSQLiteTTIn p)





{- Actions -}

-- | Type representing atomic teletype actions
data SimpleSQLiteAction mark a where
  SimpleSQLiteOpen
    :: String
    -> SimpleSQLiteAction mark
        (Except SimpleSQLiteError (mark SimpleSQLiteException) SQLite.Connection)

  SimpleSQLiteClose
    :: mark SQLite.Connection
    -> SimpleSQLiteAction mark
        (Except SimpleSQLiteError (mark SimpleSQLiteException) ())

  SimpleSQLiteQuery
    :: ( SQLite.ToRow q, SQLite.FromRow r )
    => SQLite.Connection
    -> SQLite.Query
    -> q
    -> SimpleSQLiteAction mark
        (Except SimpleSQLiteError (mark SimpleSQLiteException) [r])

  SimpleSQLiteQuery_
    :: ( SQLite.FromRow r )
    => SQLite.Connection
    -> SQLite.Query
    -> SimpleSQLiteAction mark
        (Except SimpleSQLiteError (mark SimpleSQLiteException) [r])

  SimpleSQLiteQueryNamed
    :: ( SQLite.FromRow r )
    => SQLite.Connection
    -> SQLite.Query
    -> [SQLite.NamedParam]
    -> SimpleSQLiteAction mark
        (Except SimpleSQLiteError (mark SimpleSQLiteException) [r])

  SimpleSQLiteLastInsertRowId
    :: SQLite.Connection
    -> SimpleSQLiteAction mark
        (Except SimpleSQLiteError (mark SimpleSQLiteException) Int64)

  SimpleSQLiteChanges
    :: SQLite.Connection
    -> SimpleSQLiteAction mark
        (Except SimpleSQLiteError (mark SimpleSQLiteException) Int)

  SimpleSQLiteExecute
    :: ( SQLite.ToRow q )
    => SQLite.Connection
    -> SQLite.Query
    -> q
    -> SimpleSQLiteAction mark
        (Except SimpleSQLiteError (mark SimpleSQLiteException) ())

  SimpleSQLiteExecute_
    :: SQLite.Connection
    -> SQLite.Query
    -> SimpleSQLiteAction mark
        (Except SimpleSQLiteError (mark SimpleSQLiteException) ())

  SimpleSQLiteExecuteNamed
    :: SQLite.Connection
    -> SQLite.Query
    -> [SQLite.NamedParam]
    -> SimpleSQLiteAction mark
        (Except SimpleSQLiteError (mark SimpleSQLiteException) ())

-- | Default @IO@ evaluator
evalSimpleSQLiteIO
  :: ( MonadIdentity mark )
  => SimpleSQLiteAction mark a -> IO a
evalSimpleSQLiteIO x = case x of
  SimpleSQLiteOpen path -> catches
    (Accept <$> SQLite.open path)
    [ Handler $ \(e :: SQLite.FormatError) ->
        return $ Except (pure $ SQLiteFormatError e)
    , Handler $ \(e :: SQLite.ResultError) ->
        return $ Except (pure $ SQLiteResultError e)
    , Handler $ \(e :: SQLite.SQLError) ->
        return $ Except (pure $ SQLiteSQLError e)
    ]

  SimpleSQLiteClose conn -> catches
    (Accept <$> SQLite.close (unwrap conn))
    [ Handler $ \(e :: SQLite.FormatError) ->
        return $ Except (pure $ SQLiteFormatError e)
    , Handler $ \(e :: SQLite.ResultError) ->
        return $ Except (pure $ SQLiteResultError e)
    , Handler $ \(e :: SQLite.SQLError) ->
        return $ Except (pure $ SQLiteSQLError e)
    ]

  SimpleSQLiteQuery conn query q -> catches
    (Accept <$> SQLite.query conn query q)
    [ Handler $ \(e :: SQLite.FormatError) ->
        return $ Except (pure $ SQLiteFormatError e)
    , Handler $ \(e :: SQLite.ResultError) ->
        return $ Except (pure $ SQLiteResultError e)
    , Handler $ \(e :: SQLite.SQLError) ->
        return $ Except (pure $ SQLiteSQLError e)
    ]

  SimpleSQLiteQuery_ conn query -> catches
    (Accept <$> SQLite.query_ conn query)
    [ Handler $ \(e :: SQLite.FormatError) ->
        return $ Except (pure $ SQLiteFormatError e)
    , Handler $ \(e :: SQLite.ResultError) ->
        return $ Except (pure $ SQLiteResultError e)
    , Handler $ \(e :: SQLite.SQLError) ->
        return $ Except (pure $ SQLiteSQLError e)
    ]

  SimpleSQLiteQueryNamed conn query params -> catches
    (Accept <$> SQLite.queryNamed conn query params)
    [ Handler $ \(e :: SQLite.FormatError) ->
        return $ Except (pure $ SQLiteFormatError e)
    , Handler $ \(e :: SQLite.ResultError) ->
        return $ Except (pure $ SQLiteResultError e)
    , Handler $ \(e :: SQLite.SQLError) ->
        return $ Except (pure $ SQLiteSQLError e)
    ]

  SimpleSQLiteLastInsertRowId conn -> catches
    (Accept <$> SQLite.lastInsertRowId conn)
    [ Handler $ \(e :: SQLite.FormatError) ->
        return $ Except (pure $ SQLiteFormatError e)
    , Handler $ \(e :: SQLite.ResultError) ->
        return $ Except (pure $ SQLiteResultError e)
    , Handler $ \(e :: SQLite.SQLError) ->
        return $ Except (pure $ SQLiteSQLError e)
    ]

  SimpleSQLiteChanges conn -> catches
    (Accept <$> SQLite.changes conn)
    [ Handler $ \(e :: SQLite.FormatError) ->
        return $ Except (pure $ SQLiteFormatError e)
    , Handler $ \(e :: SQLite.ResultError) ->
        return $ Except (pure $ SQLiteResultError e)
    , Handler $ \(e :: SQLite.SQLError) ->
        return $ Except (pure $ SQLiteSQLError e)
    ]

  SimpleSQLiteExecute conn query q -> catches
    (Accept <$> SQLite.execute conn query q)
    [ Handler $ \(e :: SQLite.FormatError) ->
        return $ Except (pure $ SQLiteFormatError e)
    , Handler $ \(e :: SQLite.ResultError) ->
        return $ Except (pure $ SQLiteResultError e)
    , Handler $ \(e :: SQLite.SQLError) ->
        return $ Except (pure $ SQLiteSQLError e)
    ]

  SimpleSQLiteExecute_ conn query -> catches
    (Accept <$> SQLite.execute_ conn query)
    [ Handler $ \(e :: SQLite.FormatError) ->
        return $ Except (pure $ SQLiteFormatError e)
    , Handler $ \(e :: SQLite.ResultError) ->
        return $ Except (pure $ SQLiteResultError e)
    , Handler $ \(e :: SQLite.SQLError) ->
        return $ Except (pure $ SQLiteSQLError e)
    ]

  SimpleSQLiteExecuteNamed conn query params -> catches
    (Accept <$> SQLite.executeNamed conn query params)
    [ Handler $ \(e :: SQLite.FormatError) ->
        return $ Except (pure $ SQLiteFormatError e)
    , Handler $ \(e :: SQLite.ResultError) ->
        return $ Except (pure $ SQLiteResultError e)
    , Handler $ \(e :: SQLite.SQLError) ->
        return $ Except (pure $ SQLiteSQLError e)
    ]





{- Effect Instances -}

instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t, MonadIdentity mark
  ) => MonadSimpleSQLite mark (SimpleSQLiteTT mark t m)
  where
    simpleSQLiteOpen
      :: String
      -> SimpleSQLiteTT mark t m (mark SQLite.Connection)
    simpleSQLiteOpen path = SimpleSQLiteTT $ toOverTT $ do
      x :: mark (Except SimpleSQLiteError (mark SimpleSQLiteException) SQLite.Connection)
        <- lift $ prompt $ return $ SimpleSQLiteOpen path
      case unwrap x of
        Except e -> throw $ SimpleSQLiteError e
        Accept a -> return $ pure a

    simpleSQLiteClose
      :: mark SQLite.Connection
      -> SimpleSQLiteTT mark t m ()
    simpleSQLiteClose conn = SimpleSQLiteTT $ toOverTT $ do
      x :: mark (Except SimpleSQLiteError (mark SimpleSQLiteException) ())
        <- lift $ prompt $ return $ SimpleSQLiteClose conn
      case unwrap x of
        Except e  -> throw $ SimpleSQLiteError e
        Accept () -> return ()

    simpleSQLiteQuery
      :: ( SQLite.ToRow q, SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> SimpleSQLiteTT mark t m (mark [r])
    simpleSQLiteQuery conn query q = SimpleSQLiteTT $ toOverTT $ do
      x :: mark (Except SimpleSQLiteError (mark SimpleSQLiteException) [r])
        <- lift $ prompt $ return $ SimpleSQLiteQuery conn query q
      case unwrap x of
        Except e -> throw $ SimpleSQLiteError e
        Accept a -> return $ pure a

    simpleSQLiteQuery_
      :: ( SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> SimpleSQLiteTT mark t m (mark [r])
    simpleSQLiteQuery_ conn query = SimpleSQLiteTT $ toOverTT $ do
      x :: mark (Except SimpleSQLiteError (mark SimpleSQLiteException) [r])
        <- lift $ prompt $ return $ SimpleSQLiteQuery_ conn query
      case unwrap x of
        Except e -> throw $ SimpleSQLiteError e
        Accept a -> return $ pure a

    simpleSQLiteQueryNamed
      :: ( SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> SimpleSQLiteTT mark t m (mark [r])
    simpleSQLiteQueryNamed conn query params = SimpleSQLiteTT $ toOverTT $ do
      x :: mark (Except SimpleSQLiteError (mark SimpleSQLiteException) [r])
        <- lift $ prompt $ return $ SimpleSQLiteQueryNamed conn query params
      case unwrap x of
        Except e -> throw $ SimpleSQLiteError e
        Accept a -> return $ pure a

    simpleSQLiteLastInsertRowId
      :: SQLite.Connection
      -> SimpleSQLiteTT mark t m (mark Int64)
    simpleSQLiteLastInsertRowId conn = SimpleSQLiteTT $ toOverTT $ do
      x :: mark (Except SimpleSQLiteError (mark SimpleSQLiteException) Int64)
        <- lift $ prompt $ return $ SimpleSQLiteLastInsertRowId conn
      case unwrap x of
        Except e -> throw $ SimpleSQLiteError e
        Accept a -> return $ pure a

    simpleSQLiteChanges
      :: SQLite.Connection
      -> SimpleSQLiteTT mark t m (mark Int)
    simpleSQLiteChanges conn = SimpleSQLiteTT $ toOverTT $ do
      x :: mark (Except SimpleSQLiteError (mark SimpleSQLiteException) Int)
        <- lift $ prompt $ return $ SimpleSQLiteChanges conn
      case unwrap x of
        Except e -> throw $ SimpleSQLiteError e

    simpleSQLiteExecute
      :: ( SQLite.ToRow q )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> SimpleSQLiteTT mark t m (mark ())
    simpleSQLiteExecute conn query q = SimpleSQLiteTT $ toOverTT $ do
      x :: mark (Except SimpleSQLiteError (mark SimpleSQLiteException) ())
        <- lift $ prompt $ return $ SimpleSQLiteExecute conn query q
      case unwrap x of
        Except e -> throw $ SimpleSQLiteError e
        Accept a -> return $ pure a

    simpleSQLiteExecute_
      :: SQLite.Connection
      -> SQLite.Query
      -> SimpleSQLiteTT mark t m (mark ())
    simpleSQLiteExecute_ conn query = SimpleSQLiteTT $ toOverTT $ do
      x :: mark (Except SimpleSQLiteError (mark SimpleSQLiteException) ())
        <- lift $ prompt $ return $ SimpleSQLiteExecute_ conn query
      case unwrap x of
        Except e -> throw $ SimpleSQLiteError e
        Accept a -> return $ pure a

    simpleSQLiteExecuteNamed
      :: SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> SimpleSQLiteTT mark t m (mark ())
    simpleSQLiteExecuteNamed conn query params = SimpleSQLiteTT $ toOverTT $ do
      x :: mark (Except SimpleSQLiteError (mark SimpleSQLiteException) ())
        <- lift $ prompt $ return $ SimpleSQLiteExecuteNamed conn query params
      case unwrap x of
        Except e -> throw $ SimpleSQLiteError e
        Accept a -> return $ pure a

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadSimpleSQLite mark (t x)
  ) => MonadSimpleSQLite mark (SimpleSQLiteTT mark1 t m)
  where
    simpleSQLiteOpen
      :: String
      -> SimpleSQLiteTT mark1 t m (mark SQLite.Connection)
    simpleSQLiteOpen path =
      liftT $ simpleSQLiteOpen path

    simpleSQLiteClose
      :: mark SQLite.Connection
      -> SimpleSQLiteTT mark1 t m ()
    simpleSQLiteClose conn =
      liftT $ simpleSQLiteClose conn

    simpleSQLiteQuery
      :: ( SQLite.ToRow q, SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> SimpleSQLiteTT mark1 t m (mark [r])
    simpleSQLiteQuery conn query q =
      liftT $ simpleSQLiteQuery conn query q

    simpleSQLiteQuery_
      :: ( SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> SimpleSQLiteTT mark1 t m (mark [r])
    simpleSQLiteQuery_ conn query =
      liftT $ simpleSQLiteQuery_ conn query

    simpleSQLiteQueryNamed
      :: ( SQLite.FromRow r )
      => SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> SimpleSQLiteTT mark1 t m (mark [r])
    simpleSQLiteQueryNamed conn query params =
      liftT $ simpleSQLiteQueryNamed conn query params

    simpleSQLiteLastInsertRowId
      :: SQLite.Connection
      -> SimpleSQLiteTT mark1 t m (mark Int64)
    simpleSQLiteLastInsertRowId conn =
      liftT $ simpleSQLiteLastInsertRowId conn

    simpleSQLiteChanges
      :: SQLite.Connection
      -> SimpleSQLiteTT mark1 t m (mark Int)
    simpleSQLiteChanges conn =
      liftT $ simpleSQLiteChanges conn

    simpleSQLiteExecute
      :: ( SQLite.ToRow q )
      => SQLite.Connection
      -> SQLite.Query
      -> q
      -> SimpleSQLiteTT mark1 t m (mark ())
    simpleSQLiteExecute conn query q =
      liftT $ simpleSQLiteExecute conn query q

    simpleSQLiteExecute_
      :: SQLite.Connection
      -> SQLite.Query
      -> SimpleSQLiteTT mark1 t m (mark ())
    simpleSQLiteExecute_ conn query =
      liftT $ simpleSQLiteExecute_ conn query

    simpleSQLiteExecuteNamed
      :: SQLite.Connection
      -> SQLite.Query
      -> [SQLite.NamedParam]
      -> SimpleSQLiteTT mark1 t m (mark ())
    simpleSQLiteExecuteNamed conn query params =
      liftT $ simpleSQLiteExecuteNamed conn query params



instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadTeletype mark (t x)
  ) => MonadTeletype mark (SimpleSQLiteTT mark1 t m)
  where
    readLine
      :: SimpleSQLiteTT mark1 t m (mark String)
    readLine = liftT readLine

    printLine
      :: mark String
      -> SimpleSQLiteTT mark1 t m ()
    printLine = liftT . printLine



instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadSystemClock mark (t x)
  ) => MonadSystemClock mark (SimpleSQLiteTT mark1 t m)
  where
    getSystemTime
      :: SimpleSQLiteTT mark1 t m (mark SystemTime)
    getSystemTime = liftT getSystemTime



instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadSimpleHttp mark (t x)
  ) => MonadSimpleHttp mark (SimpleSQLiteTT mark1 t m)
  where
    simpleHttpReq
      :: ( Req.HttpMethod method, Req.HttpBody body, Req.HttpResponse response
         , Req.HttpBodyAllowed (Req.AllowsBody method) (Req.ProvidesBody body) )
      => method
      -> Req.Url scheme
      -> body
      -> Proxy response
      -> Req.Option scheme
      -> SimpleSQLiteTT mark1 t m (mark response)
    simpleHttpReq method scheme body response opt =
      liftT $ simpleHttpReq method scheme body response opt



instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadState mark s (t x)
  ) => MonadState mark s (SimpleSQLiteTT mark1 t m)
  where
    get
      :: SimpleSQLiteTT mark1 t m (mark s)
    get = SimpleSQLiteTT $ toOverTT $ lift $ liftT get

    put
      :: mark s
      -> SimpleSQLiteTT mark1 t m ()
    put = SimpleSQLiteTT . toOverTT . lift . liftT . put



-- instance {-# OVERLAPPABLE #-}
--   ( Monad m, MonadTrans t, MonadIdentity mark
--   , MonadIdentity mark1, Commutant mark1
--   , forall x. (Monad x) => MonadExcept mark e (t x)
--   ) => MonadExcept mark e (SimpleSQLiteTT mark1 t m)
--   where
--     throw
--       :: mark e
--       -> SimpleSQLiteTT mark1 t m a
--     throw = SimpleSQLiteTT . OverTT . lift . liftT . throw
-- 
--     catch
--       :: SimpleSQLiteTT mark1 t m a
--       -> (mark e -> SimpleSQLiteTT mark1 t m a)
--       -> SimpleSQLiteTT mark1 t m a
--     catch x h = SimpleSQLiteTT $ OverTT $
--       liftCatch (liftCatchT catch)
--         (unOverTT $ unSimpleSQLiteTT x)
--         (unOverTT . unSimpleSQLiteTT . h)



instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadIdentity mark1, Commutant mark1
  , forall x. (Monad x) => MonadReadOnly mark r (t x)
  ) => MonadReadOnly mark r (SimpleSQLiteTT mark1 t m)
  where
    ask
      :: SimpleSQLiteTT mark1 t m (mark r)
    ask = SimpleSQLiteTT $ toOverTT $ lift $ liftT ask

    local
      :: (mark r -> mark r)
      -> SimpleSQLiteTT mark1 t m a
      -> SimpleSQLiteTT mark1 t m a
    local f (SimpleSQLiteTT x) =
      SimpleSQLiteTT $ toOverTT $ local f $ unOverTT x



instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadIdentity mark1, Commutant mark1, Monoid w
  , forall x. (Monad x) => MonadAppendOnly mark w (t x)
  ) => MonadAppendOnly mark w (SimpleSQLiteTT mark1 t m)
  where
    jot
      :: mark w
      -> SimpleSQLiteTT mark1 t m ()
    jot = SimpleSQLiteTT . toOverTT . lift . liftT . jot

    look
      :: SimpleSQLiteTT mark1 t m (mark w)
    look = SimpleSQLiteTT $ toOverTT $ lift $ liftT look



instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadIdentity mark1, Commutant mark1
  , forall x. (Monad x) => MonadWriteOnce mark w (t x)
  ) => MonadWriteOnce mark w (SimpleSQLiteTT mark1 t m)
  where
    etch
      :: mark w
      -> SimpleSQLiteTT mark1 t m Bool
    etch = SimpleSQLiteTT . toOverTT . lift . liftT . etch

    press
      :: SimpleSQLiteTT mark1 t m (Maybe (mark w))
    press = SimpleSQLiteTT $ toOverTT $ lift $ liftT press



-- instance
--   ( Monad m, MonadTrans t, MonadIdentity mark
--   , MonadIdentity mark1, Commutant mark1, Monoid w
--   , forall x. (Monad x) => MonadWriteOnly mark w (t x)
--   ) => MonadWriteOnly mark w (SimpleSQLiteTT mark1 t m)
--   where
--     tell
--       :: mark w
--       -> SimpleSQLiteTT mark1 t m ()
--     tell = SimpleSQLiteTT . OverTT . lift . liftT . tell
-- 
--     draft
--       :: SimpleSQLiteTT mark1 t m a
--       -> SimpleSQLiteTT mark1 t m (Pair (mark w) a)
--     draft = SimpleSQLiteTT . OverTT . draft . unOverTT . unSimpleSQLiteTT



instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadIdentity mark1, Commutant mark1
  , forall x. (Monad x) => MonadPrompt mark p (t x)
  ) => MonadPrompt mark p (SimpleSQLiteTT mark1 t m)
  where
    prompt
      :: mark (p a)
      -> SimpleSQLiteTT mark1 t m (mark a)
    prompt = SimpleSQLiteTT . toOverTT . lift . liftT . prompt



instance
  ( Monad m, MonadTrans t, MonadIdentity mark1, MonadIdentity mark
  , forall x. (Monad x) => MonadHalt mark (t x)
  ) => MonadHalt mark (SimpleSQLiteTT mark1 t m)
  where
    halt
      :: mark ()
      -> SimpleSQLiteTT mark1 t m a
    halt = SimpleSQLiteTT . toOverTT . lift . liftT . halt



instance
  ( Monad m, MonadTrans t, MonadIdentity mark, MonadIdentity mark1
  , forall x. (Monad x) => MonadStack mark f d (t x), IsStack f
  ) => MonadStack mark f d (SimpleSQLiteTT mark1 t m)
  where
    push
      :: Proxy f
      -> mark d
      -> SimpleSQLiteTT mark1 t m ()
    push proxy = SimpleSQLiteTT . toOverTT . lift . liftT . push proxy

    pop
      :: Proxy f
      -> SimpleSQLiteTT mark1 t m (mark (Maybe d))
    pop proxy = SimpleSQLiteTT $ toOverTT $ lift $ liftT $ pop proxy
