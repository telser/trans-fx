{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.Trans.Trans.IO.Class (
    MonadTeletype(..)
  , MonadSystemClock(..)
  , SystemTime(..)
) where



import Control.FX
import Control.FX.Data

import Data.Time.Clock.System ( SystemTime )



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
    -- | Read a line of input
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
