{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.Data.Class (
    MonadStack(..)
) where



import Data.Typeable (Proxy)

import Control.FX
import Control.FX.Structure.Stack



-- | Class representing monads with access to a stack
-- of data with type @d@ and stack functor @f@. Instances
-- should satisfy the following laws:
--
-- > (1) push p a >> pop p  ===  return (Just <$> a)
class
  ( Monad m, MonadIdentity mark, IsStack f
  ) => MonadStack mark f d m
  where
    -- | Push a value to the stack
    push :: Proxy f -> mark d -> m ()

    default push
      :: ( Monad m1, MonadTrans t1, m ~ t1 m1
         , MonadStack mark f d m1 )
      => Proxy f
      -> mark d
      -> m ()
    push proxy = lift . push proxy

    -- | Try to pop a value from the stack
    pop :: Proxy f -> m (mark (Maybe d))

    default pop
      :: ( Monad m1, MonadTrans t1, m ~ t1 m1
         , MonadStack mark f d m1 )
      => Proxy f
      -> m (mark (Maybe d))
    pop proxy = lift $ pop proxy



instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadStack mark f d m
  ) => MonadStack mark f d (ExceptT mark1 e m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadStack mark f d m
  ) => MonadStack mark f d (ReadOnlyT mark1 r m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadStack mark f d m, Monoid w
  ) => MonadStack mark f d (WriteOnlyT mark1 w m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadStack mark f d m, Monoid w
  ) => MonadStack mark f d (AppendOnlyT mark1 w m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadStack mark f d m
  ) => MonadStack mark f d (WriteOnceT mark1 w m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadStack mark f d m
  ) => MonadStack mark f d (StateT mark1 s m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadStack mark f d m
  ) => MonadStack mark f d (HaltT mark1 m)

instance
  ( Monad m, MonadIdentity mark
  , MonadStack mark f d m
  ) => MonadStack mark f d (IdentityT m)





instance
  ( Monad m, MonadTrans t, IsStack f
  , MonadStack mark f d (t m)
  ) => MonadStack mark f d (IdentityTT t m)
  where
    push
      :: Proxy f
      -> mark d
      -> IdentityTT t m ()
    push proxy = IdentityTT . push proxy

    pop
      :: Proxy f
      -> IdentityTT t m (mark (Maybe d))
    pop proxy = IdentityTT $ pop proxy

instance
  ( Monad m, MonadTrans t, MonadIdentity mark1
  , MonadStack mark f d (t m), IsStack f
  ) => MonadStack mark f d (PromptTT mark1 p t m)
  where
    push
      :: Proxy f
      -> mark d
      -> PromptTT mark1 p t m ()
    push proxy = liftT . push proxy

    pop
      :: Proxy f
      -> PromptTT mark1 p t m (mark (Maybe d))
    pop proxy = liftT $ pop proxy

instance
  ( Monad m, MonadTrans t, MonadIdentity mark1
  , MonadStack mark f d (t m), IsStack f
  ) => MonadStack mark f d (StateTT mark1 s t m)
  where
    push
      :: Proxy f
      -> mark d
      -> StateTT mark1 s t m ()
    push proxy = StateTT . push proxy

    pop
      :: Proxy f
      -> StateTT mark1 s t m (mark (Maybe d))
    pop proxy = StateTT $ pop proxy

instance
  ( Monad m, MonadTrans t, MonadIdentity mark1
  , MonadStack mark f d (t m), IsStack f
  ) => MonadStack mark f d (ReadOnlyTT mark1 r t m)
  where
    push
      :: Proxy f
      -> mark d
      -> ReadOnlyTT mark1 r t m ()
    push proxy = ReadOnlyTT . push proxy

    pop
      :: Proxy f
      -> ReadOnlyTT mark1 r t m (mark (Maybe d))
    pop proxy = ReadOnlyTT $ pop proxy

instance
  ( Monad m, MonadTrans t, MonadIdentity mark1
  , MonadStack mark f d (t m), IsStack f, Monoid w
  ) => MonadStack mark f d (WriteOnlyTT mark1 w t m)
  where
    push
      :: Proxy f
      -> mark d
      -> WriteOnlyTT mark1 w t m ()
    push proxy = WriteOnlyTT . push proxy

    pop
      :: Proxy f
      -> WriteOnlyTT mark1 w t m (mark (Maybe d))
    pop proxy = WriteOnlyTT $ pop proxy

instance
  ( Monad m, MonadTrans t, MonadIdentity mark1
  , MonadStack mark f d (t m), IsStack f
  ) => MonadStack mark f d (ExceptTT mark1 e t m)
  where
    push
      :: Proxy f
      -> mark d
      -> ExceptTT mark1 e t m ()
    push proxy = ExceptTT . push proxy

    pop
      :: Proxy f
      -> ExceptTT mark1 e t m (mark (Maybe d))
    pop proxy = ExceptTT $ pop proxy

instance
  ( Monad m, MonadTrans t, MonadIdentity mark1
  , MonadStack mark f d (t m), IsStack f
  ) => MonadStack mark f d (HaltTT mark1 t m)
  where
    push
      :: Proxy f
      -> mark d
      -> HaltTT mark1 t m ()
    push proxy = HaltTT . push proxy

    pop
      :: Proxy f
      -> HaltTT mark1 t m (mark (Maybe d))
    pop proxy = HaltTT $ pop proxy

instance
  ( Monad m, MonadTrans t, MonadIdentity mark1
  , MonadStack mark f d (t m), IsStack f, Monoid w
  ) => MonadStack mark f d (AppendOnlyTT mark1 w t m)
  where
    push
      :: Proxy f
      -> mark d
      -> AppendOnlyTT mark1 w t m ()
    push proxy = AppendOnlyTT . push proxy

    pop
      :: Proxy f
      -> AppendOnlyTT mark1 w t m (mark (Maybe d))
    pop proxy = AppendOnlyTT $ pop proxy

instance
  ( Monad m, MonadTrans t, MonadIdentity mark1
  , MonadStack mark f d (t m), IsStack f
  ) => MonadStack mark f d (WriteOnceTT mark1 w t m)
  where
    push
      :: Proxy f
      -> mark d
      -> WriteOnceTT mark1 w t m ()
    push proxy = WriteOnceTT . push proxy

    pop
      :: Proxy f
      -> WriteOnceTT mark1 w t m (mark (Maybe d))
    pop proxy = WriteOnceTT $ pop proxy
