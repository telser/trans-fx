-- | Module      : Control.FX.Monad.Trans.Trans.OverTT
--   Description : Concrete monad functor application monad transformer transformer
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.Trans.Trans.OverTT (
    OverableT(..)
  , OverTT(..)
  , runOverTT
  , Context(..)
  , InputTT(..)
  , OutputTT(..)
) where



import Data.Typeable (Typeable, typeOf)
import Control.Monad (ap)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans
import Control.FX.Monad.Trans.Trans.Class



class
  ( MonadTrans v
  ) => OverableT v
  where
    -- | Concrete monad transformer transformer which applies a monad functor
    data
      OverTT
        (v :: (* -> *) -> * -> *)
        (u :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *)
        (t :: (* -> *) -> * -> *)
        (m :: * -> *)
        (a :: *)

    toOverTT
      :: v (u t m) a
      -> OverTT v u t m a

    unOverTT
      :: OverTT v u t m a
      -> v (u t m) a

instance
  OverableT IdentityT
  where
    newtype (OverTT IdentityT u t m a) =
      OverTT_IdentityT
        { unOverTT_IdentityT :: IdentityT (u t m) a
        } deriving (Typeable)

    toOverTT = OverTT_IdentityT
    unOverTT = unOverTT_IdentityT

instance
  ( MonadIdentity mark
  ) => OverableT (HaltT mark)
  where
    newtype (OverTT (HaltT mark) u t m a) =
      OverTT_HaltT
        { unOverTT_HaltT :: HaltT mark (u t m) a
        } deriving (Typeable)

    toOverTT = OverTT_HaltT
    unOverTT = unOverTT_HaltT

instance
  ( MonadIdentity mark
  ) => OverableT (StateT mark s)
  where
    newtype (OverTT (StateT mark s) u t m a) =
      OverTT_StateT
        { unOverTT_StateT :: StateT mark s (u t m) a
        } deriving (Typeable)

    toOverTT = OverTT_StateT
    unOverTT = unOverTT_StateT

instance
  ( MonadIdentity mark
  ) => OverableT (ReadOnlyT mark r)
  where
    newtype (OverTT (ReadOnlyT mark r) u t m a) =
      OverTT_ReadOnlyT
        { unOverTT_ReadOnlyT :: ReadOnlyT mark r (u t m) a
        } deriving (Typeable)

    toOverTT = OverTT_ReadOnlyT
    unOverTT = unOverTT_ReadOnlyT

instance
  ( MonadIdentity mark, Monoid w
  ) => OverableT (WriteOnlyT mark w)
  where
    newtype (OverTT (WriteOnlyT mark w) u t m a) =
      OverTT_WriteOnlyT
        { unOverTT_WriteOnlyT :: WriteOnlyT mark w (u t m) a
        } deriving (Typeable)

    toOverTT = OverTT_WriteOnlyT
    unOverTT = unOverTT_WriteOnlyT

instance
  ( MonadIdentity mark
  ) => OverableT (WriteOnceT mark w)
  where
    newtype (OverTT (WriteOnceT mark w) u t m a) =
      OverTT_WriteOnceT
        { unOverTT_WriteOnceT :: WriteOnceT mark w (u t m) a
        } deriving (Typeable)

    toOverTT = OverTT_WriteOnceT
    unOverTT = unOverTT_WriteOnceT

instance
  ( MonadIdentity mark, Monoid w
  ) => OverableT (AppendOnlyT mark w)
  where
    newtype (OverTT (AppendOnlyT mark w) u t m a) =
      OverTT_AppendOnlyT
        { unOverTT_AppendOnlyT :: AppendOnlyT mark w (u t m) a
        } deriving (Typeable)

    toOverTT = OverTT_AppendOnlyT
    unOverTT = unOverTT_AppendOnlyT

instance
  ( MonadIdentity mark
  ) => OverableT (ExceptT mark e)
  where
    newtype (OverTT (ExceptT mark e) u t m a) =
      OverTT_ExceptT
        { unOverTT_ExceptT :: ExceptT mark e (u t m) a
        } deriving (Typeable)

    toOverTT = OverTT_ExceptT
    unOverTT = unOverTT_ExceptT



instance
  ( Show (v (u t m) a), OverableT v
  ) => Show (OverTT v u t m a)
  where
    show = show . unOverTT

instance
  ( Monad m, MonadTrans t, MonadTrans v
  , MonadTransTrans u, OverableT v
  ) => Functor (OverTT v u t m)
  where
    fmap
      :: (a -> b)
      -> OverTT v u t m a
      -> OverTT v u t m b
    fmap f = toOverTT . fmap f . unOverTT

instance
  ( Monad m, MonadTrans t, MonadTrans v
  , MonadTransTrans u, OverableT v
  ) => Applicative (OverTT v u t m)
  where
    pure
      :: a
      -> OverTT v u t m a
    pure = return

    (<*>)
      :: OverTT v u t m (a -> b)
      -> OverTT v u t m a
      -> OverTT v u t m b
    (<*>) = ap

instance
  ( Monad m, MonadTrans t, MonadTrans v
  , MonadTransTrans u, OverableT v
  ) => Monad (OverTT v u t m)
  where
    return
      :: a
      -> OverTT v u t m a
    return = toOverTT . return

    (>>=)
      :: OverTT v u t m a
      -> (a -> OverTT v u t m b)
      -> OverTT v u t m b
    x >>= f =
      toOverTT $ (unOverTT x) >>= (unOverTT . f)

instance
  ( MonadTrans t, MonadTrans v
  , MonadTransTrans u, OverableT v
  ) => MonadTrans (OverTT v u t)
  where
    lift
      :: ( Monad m )
      => m a
      -> OverTT v u t m a
    lift = toOverTT . lift . lift

instance
  ( MonadTrans v, MonadTransTrans u, OverableT v
  ) => MonadTransTrans (OverTT v u)
  where
    liftT
      :: ( Monad m, MonadTrans t )
      => t m a
      -> OverTT v u t m a
    liftT = toOverTT . lift . liftT



instance
  ( MonadIdentity (v (u t m))
  , OverableT v, Eq a
  ) => Eq (OverTT v u t m a)
  where
    (==)
      :: OverTT v u t m a
      -> OverTT v u t m a
      -> Bool
    x == y =
      (==)
        (unwrap $ unOverTT x)
        (unwrap $ unOverTT y)

instance
  ( MonadIdentity (v (u t m))
  , OverableT v, Semigroup a
  ) => Semigroup (OverTT v u t m a)
  where
    (<>)
      :: OverTT v u t m a
      -> OverTT v u t m a
      -> OverTT v u t m a
    x <> y =
      toOverTT $ pure $ (<>)
        (unwrap $ unOverTT x)
        (unwrap $ unOverTT y)

instance
  ( MonadIdentity (v (u t m))
  , OverableT v, Monoid a
  ) => Monoid (OverTT v u t m a)
  where
    mempty
      :: OverTT v u t m a
    mempty = toOverTT $ pure mempty


instance
  ( Monad m, MonadTrans t, MonadTrans v, MonadTransTrans u
  , RunMonadTransTrans u, RunMonadTrans v, OverableT v
  , forall x. (Eq x) => Eq (OutputTT u (OutputT v x))
  , EqIn (t m)
  ) => EqIn (OverTT v u t m)
  where
    newtype Context (OverTT v u t m)
      = OverTTCtx
          { unOverTTCtx :: ((InputTT u m, InputT v), Context (t m))
          } deriving (Typeable)

    eqIn
      :: (Eq a)
      => Context (OverTT v u t m)
      -> OverTT v u t m a
      -> OverTT v u t m a
      -> Bool
    eqIn (OverTTCtx (k,h)) x y =
      eqIn h
        (fmap unOverTTOut $ runTT (OverTTIn k) x)
        (fmap unOverTTOut $ runTT (OverTTIn k) y)

deriving instance
  ( Eq (InputTT u m), Eq (InputT v), Eq (Context (t m))
  ) => Eq (Context (OverTT v u t m))

deriving instance
  ( Show (InputTT u m), Show (InputT v), Show (Context (t m))
  ) => Show (Context (OverTT v u t m))



instance
  ( RunMonadTransTrans u, RunMonadTrans v
  , MonadTrans v, OverableT v
  ) => RunMonadTransTrans (OverTT v u)
  where
    newtype InputTT (OverTT v u) m
      = OverTTIn
          { unOverTTIn :: (InputTT u m, InputT v)
          } deriving (Typeable)

    newtype OutputTT (OverTT v u) a
      = OverTTOut
          { unOverTTOut :: Compose (OutputTT u) (OutputT v) a
          } deriving (Typeable)

    runTT
      :: ( Monad m, MonadTrans t )
      => InputTT (OverTT v u) m
      -> OverTT v u t m a
      -> t m (OutputTT (OverTT v u) a)
    runTT (OverTTIn (z1,z2)) =
      fmap (OverTTOut . Compose) . runTT z1 . runT z2 . unOverTT

deriving instance
  ( Eq (InputTT u m), Eq (InputT v)
  ) => Eq (InputTT (OverTT v u) m)

deriving instance
  ( Show (InputTT u m), Show (InputT v)
  ) => Show (InputTT (OverTT v u) m)

deriving instance
  ( Eq (OutputTT u a), Eq (OutputTT u (OutputT v a))
  ) => Eq (OutputTT (OverTT v u) a)

deriving instance
  ( Show (OutputTT u a) , Show (OutputTT u (OutputT v a))
  ) => Show (OutputTT (OverTT v u) a)

runOverTT
  :: ( RunMonadTransTrans u, RunMonadTrans v, OverableT v
     , Monad m, MonadTrans t, MonadTrans v )
  => InputTT u m
  -> InputT v
  -> OverTT v u t m a
  -> t m (OutputTT u (OutputT v a))
runOverTT z1 z2 =
  fmap (unCompose . unOverTTOut) . runTT (OverTTIn (z1,z2))





{- Effect Instances -}

instance
  ( Monad m, MonadTrans t, MonadTrans v, OverableT v
  , MonadTransTrans u, MonadIdentity (v (u t m))
  ) => MonadIdentity (OverTT v u t m)
  where
    unwrap
      :: OverTT v u t m a
      -> a
    unwrap = unwrap . unOverTT



instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t
  , MonadTransTrans u, MonadIdentity mark
  ) => MonadExcept mark e (OverTT (ExceptT mark e) u t m)
  where
    throw
      :: mark e
      -> OverTT (ExceptT mark e) u t m a
    throw = toOverTT . throw

    catch
      :: OverTT (ExceptT mark e) u t m a
      -> (mark e -> OverTT (ExceptT mark e) u t m a)
      -> OverTT (ExceptT mark e) u t m a
    catch x h = toOverTT $ catch (unOverTT x) (unOverTT . h)

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadTrans v, OverableT v
  , MonadTransTrans u, MonadIdentity mark, LiftCatch v
  , forall x. (Monad x) => MonadExcept mark e (u t x)
  ) => MonadExcept mark e (OverTT v u t m)
  where
    throw
      :: mark e
      -> OverTT v u t m a
    throw = toOverTT . lift . throw

    catch
      :: OverTT v u t m a
      -> (mark e -> OverTT v u t m a)
      -> OverTT v u t m a
    catch x h = toOverTT $ liftCatch catch (unOverTT x) (unOverTT . h)



instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t, Monoid w
  , MonadTransTrans u, MonadIdentity mark
  ) => MonadWriteOnly mark w (OverTT (WriteOnlyT mark w) u t m)
  where
    tell
      :: mark w
      -> OverTT (WriteOnlyT mark w) u t m ()
    tell = toOverTT . tell

    draft
      :: OverTT (WriteOnlyT mark w) u t m a
      -> OverTT (WriteOnlyT mark w) u t m (Pair (mark w) a)
    draft = toOverTT . draft . unOverTT

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadTrans v, Monoid w, LiftDraft v
  , MonadTransTrans u, MonadIdentity mark, OverableT v
  , forall x. (Monad x) => MonadWriteOnly mark w (u t x)
  ) => MonadWriteOnly mark w (OverTT v u t m)
  where
    tell
      :: mark w
      -> OverTT v u t m ()
    tell = toOverTT . lift . tell

    draft
      :: OverTT v u t m a
      -> OverTT v u t m (Pair (mark w) a)
    draft = toOverTT . liftDraft draft . unOverTT



instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t, Monoid w
  , MonadTransTrans u, MonadIdentity mark
  ) => MonadAppendOnly mark w (OverTT (AppendOnlyT mark w) u t m)
  where
    jot
      :: mark w
      -> OverTT (AppendOnlyT mark w) u t m ()
    jot = toOverTT . jot

    look
      :: OverTT (AppendOnlyT mark w) u t m (mark w)
    look = toOverTT look

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadTrans v, Monoid w
  , MonadTransTrans u, MonadIdentity mark, OverableT v
  , forall x. (Monad x) => MonadAppendOnly mark w (u t x)
  ) => MonadAppendOnly mark w (OverTT v u t m)
  where
    jot
      :: mark w
      -> OverTT v u t m ()
    jot = toOverTT . lift . jot

    look
      :: OverTT v u t m (mark w)
    look = toOverTT $ lift look



instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t
  , MonadTransTrans u, MonadIdentity mark
  ) => MonadWriteOnce mark w (OverTT (WriteOnceT mark w) u t m)
  where
    etch
      :: mark w
      -> OverTT (WriteOnceT mark w) u t m Bool
    etch = toOverTT . etch

    press
      :: OverTT (WriteOnceT mark w) u t m (Maybe (mark w))
    press = toOverTT press

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadTrans v, OverableT v
  , MonadTransTrans u, MonadIdentity mark
  , forall x. (Monad x) => MonadWriteOnce mark w (u t x)
  ) => MonadWriteOnce mark w (OverTT v u t m)
  where
    etch
      :: mark w
      -> OverTT v u t m Bool
    etch = toOverTT . lift . etch

    press
      :: OverTT v u t m (Maybe (mark w))
    press = toOverTT $ lift press



instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t
  , MonadTransTrans u, MonadIdentity mark
  ) => MonadHalt mark (OverTT (HaltT mark) u t m)
  where
    halt
      :: mark ()
      -> OverTT (HaltT mark) u t m a
    halt = toOverTT . halt

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadTrans v
  , MonadTransTrans u, MonadIdentity mark, OverableT v
  , forall x. (Monad x) => MonadHalt mark (u t x)
  ) => MonadHalt mark (OverTT v u t m)
  where
    halt
      :: mark ()
      -> OverTT v u t m a
    halt = toOverTT . lift . halt



instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t
  , MonadTransTrans u, MonadIdentity mark
  ) => MonadReadOnly mark r (OverTT (ReadOnlyT mark r) u t m)
  where
    ask
      :: OverTT (ReadOnlyT mark r) u t m (mark r)
    ask = toOverTT ask

    local
      :: (mark r -> mark r)
      -> OverTT (ReadOnlyT mark r) u t m a
      -> OverTT (ReadOnlyT mark r) u t m a
    local f = toOverTT . local f . unOverTT

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadTrans v, OverableT v
  , MonadTransTrans u, MonadIdentity mark, LiftLocal v
  , forall x. (Monad x) => MonadReadOnly mark r (u t x)
  ) => MonadReadOnly mark r (OverTT v u t m)
  where
    ask
      :: OverTT v u t m (mark r)
    ask = toOverTT $ lift ask

    local
      :: (mark r -> mark r)
      -> OverTT v u t m a
      -> OverTT v u t m a
    local f = toOverTT . liftLocal local f . unOverTT



instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t
  , MonadTransTrans u, MonadIdentity mark
  ) => MonadState mark s (OverTT (StateT mark s) u t m)
  where
    get
      :: OverTT (StateT mark s) u t m (mark s)
    get = toOverTT get

    put
      :: mark s
      -> OverTT (StateT mark s) u t m ()
    put = toOverTT . put

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadTrans v, OverableT v
  , MonadTransTrans u, MonadIdentity mark
  , forall x. (Monad x) => MonadState mark s (u t x)
  ) => MonadState mark s (OverTT v u t m)
  where
    get
      :: OverTT v u t m (mark s)
    get = toOverTT $ lift get

    put
      :: mark s
      -> OverTT v u t m ()
    put = toOverTT . lift . put



instance
  ( Monad m, MonadTrans t, MonadTrans v, OverableT v
  , MonadTransTrans u, MonadPrompt mark p (u t m)
  ) => MonadPrompt mark p (OverTT v u t m)
  where
    prompt
      :: mark (p a)
      -> OverTT v u t m (mark a)
    prompt = toOverTT . lift . prompt
