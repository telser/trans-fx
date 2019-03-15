{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.IO.Monad.Trans.Trans.Class (
    MonadTeletype(..)
) where



import Control.FX



-- | Class representing monads which can interact with a teletype-style interface
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
  , MonadTeletype mark (u t m)
  ) => MonadTeletype mark (OverTT u w t m)
  where
    readLine
      :: OverTT u w t m (mark String)
    readLine = OverTT $ lift readLine

    printLine
      :: mark String
      -> OverTT u w t m ()
    printLine = OverTT . lift . printLine

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
