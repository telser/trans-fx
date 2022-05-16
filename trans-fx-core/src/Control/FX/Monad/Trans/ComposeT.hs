-- | Module      : Control.FX.Monad.Trans.ComposeT
--   Description : Concrete composite monad transformer
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.Trans.ComposeT (
    ComposableT(..)
  , ComposeT(..)
  , Context(..)
  , InputT(..)
  , OutputT(..)
) where



import Data.Typeable (Typeable)
import Control.Monad (ap)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans.Class
import Control.FX.Monad.Trans.IdentityT
import Control.FX.Monad.Trans.ExceptT
import Control.FX.Monad.Trans.WriteOnlyT
import Control.FX.Monad.Trans.StateT
import Control.FX.Monad.Trans.ReadOnlyT
import Control.FX.Monad.Trans.HaltT
import Control.FX.Monad.Trans.AppendOnlyT
import Control.FX.Monad.Trans.WriteOnceT



-- | Class representing monad transformers which can be
-- composed on top of an arbitrary monad transformer.
class
  ( MonadTrans t1
  ) => ComposableT t1
  where
    -- | Concrete composite monad transformer
    data
      ComposeT
        (t1 :: (* -> *) -> * -> *)
        (t2 :: (* -> *) -> * -> *)
        (m :: * -> *)
        (a :: *)

    toComposeT
      :: t1 (t2 m) a
      -> ComposeT t1 t2 m a

    unComposeT
      :: ComposeT t1 t2 m a
      -> t1 (t2 m) a

instance
  ComposableT IdentityT
  where
    newtype (ComposeT IdentityT t2 m a) =
      ComposeT_IdentityT
        { unComposeT_IdentityT :: IdentityT (t2 m) a
        } deriving (Typeable)

    toComposeT = ComposeT_IdentityT
    unComposeT = unComposeT_IdentityT

instance
  ( MonadIdentity mark
  ) => ComposableT (HaltT mark)
  where
    newtype (ComposeT (HaltT mark) t2 m a) =
      ComposeT_HaltT
        { unComposeT_HaltT :: HaltT mark (t2 m) a
        } deriving (Typeable)

    toComposeT = ComposeT_HaltT
    unComposeT = unComposeT_HaltT

instance
  ( MonadIdentity mark
  ) => ComposableT (StateT mark s)
  where
    newtype (ComposeT (StateT mark s) t2 m a) =
      ComposeT_StateT
        { unComposeT_StateT :: StateT mark s (t2 m) a
        } deriving (Typeable)

    toComposeT = ComposeT_StateT
    unComposeT = unComposeT_StateT

instance
  ( MonadIdentity mark
  ) => ComposableT (ReadOnlyT mark r)
  where
    newtype (ComposeT (ReadOnlyT mark r) t2 m a) =
      ComposeT_ReadOnlyT
        { unComposeT_ReadOnlyT :: ReadOnlyT mark r (t2 m) a
        } deriving (Typeable)

    toComposeT = ComposeT_ReadOnlyT
    unComposeT = unComposeT_ReadOnlyT

instance
  ( MonadIdentity mark, Monoid w
  ) => ComposableT (WriteOnlyT mark w)
  where
    newtype (ComposeT (WriteOnlyT mark w) t2 m a) =
      ComposeT_WriteOnlyT
        { unComposeT_WriteOnlyT :: WriteOnlyT mark w (t2 m) a
        } deriving (Typeable)

    toComposeT = ComposeT_WriteOnlyT
    unComposeT = unComposeT_WriteOnlyT

instance
  ( MonadIdentity mark
  ) => ComposableT (WriteOnceT mark w)
  where
    newtype (ComposeT (WriteOnceT mark w) t2 m a) =
      ComposeT_WriteOnceT
        { unComposeT_WriteOnceT :: WriteOnceT mark w (t2 m) a
        } deriving (Typeable)

    toComposeT = ComposeT_WriteOnceT
    unComposeT = unComposeT_WriteOnceT

instance
  ( MonadIdentity mark, Monoid w
  ) => ComposableT (AppendOnlyT mark w)
  where
    newtype (ComposeT (AppendOnlyT mark w) t2 m a) =
      ComposeT_AppendOnlyT
        { unComposeT_AppendOnlyT :: AppendOnlyT mark w (t2 m) a
        } deriving (Typeable)

    toComposeT = ComposeT_AppendOnlyT
    unComposeT = unComposeT_AppendOnlyT

instance
  ( MonadIdentity mark
  ) => ComposableT (ExceptT mark e)
  where
    newtype (ComposeT (ExceptT mark e) t2 m a) =
      ComposeT_ExceptT
        { unComposeT_ExceptT :: ExceptT mark e (t2 m) a
        } deriving (Typeable)

    toComposeT = ComposeT_ExceptT
    unComposeT = unComposeT_ExceptT





instance
  ( Show (t1 (t2 m) a), ComposableT t1
  ) => Show (ComposeT t1 t2 m a)
  where
    show = show . unComposeT

instance
  ( Monad m, MonadTrans t1, MonadTrans t2, ComposableT t1
  ) => Functor (ComposeT t1 t2 m)
  where
    fmap
      :: (a -> b)
      -> ComposeT t1 t2 m a
      -> ComposeT t1 t2 m b
    fmap f = toComposeT . fmap f . unComposeT

instance
  ( Monad m, MonadTrans t1, MonadTrans t2, ComposableT t1
  ) => Applicative (ComposeT t1 t2 m)
  where
    pure
      :: a
      -> ComposeT t1 t2 m a
    pure = return

    (<*>)
      :: ComposeT t1 t2 m (a -> b)
      -> ComposeT t1 t2 m a
      -> ComposeT t1 t2 m b
    (<*>) = ap

instance
  ( Monad m, MonadTrans t1, MonadTrans t2, ComposableT t1
  ) => Monad (ComposeT t1 t2 m)
  where
    return
      :: a
      -> ComposeT t1 t2 m a
    return = toComposeT . return

    (>>=)
      :: ComposeT t1 t2 m a
      -> (a -> ComposeT t1 t2 m b)
      -> ComposeT t1 t2 m b
    x >>= f =
      toComposeT ((unComposeT x) >>= (unComposeT . f))



instance
  ( MonadIdentity m, MonadTrans t1, MonadTrans t2, Eq a
  , MonadIdentity (t2 m), MonadIdentity (t1 (t2 m))
  , ComposableT t1
  ) => Eq (ComposeT t1 t2 m a)
  where
    (==)
      :: ComposeT t1 t2 m a
      -> ComposeT t1 t2 m a
      -> Bool
    x == y = (==)
      (unwrap $ unComposeT x)
      (unwrap $ unComposeT y)

instance
  ( MonadIdentity m, MonadTrans t1, MonadTrans t2, Semigroup a
  , MonadIdentity (t2 m) , MonadIdentity (t1 (t2 m))
  , ComposableT t1
  ) => Semigroup (ComposeT t1 t2 m a)
  where
    (<>)
      :: ComposeT t1 t2 m a
      -> ComposeT t1 t2 m a
      -> ComposeT t1 t2 m a
    a <> b = toComposeT $
      (unComposeT a) <> (unComposeT b)

instance
  ( MonadIdentity m, MonadTrans t1, MonadTrans t2, Monoid a
  , MonadIdentity (t2 m) , MonadIdentity (t1 (t2 m))
  , ComposableT t1
  ) => Monoid (ComposeT t1 t2 m a)
  where
    mempty
      :: ComposeT t1 t2 m a
    mempty = toComposeT mempty

instance
  ( MonadTrans t1, MonadTrans t2, ComposableT t1
  ) => MonadTrans (ComposeT t1 t2)
  where
    lift
      :: ( Monad m )
      => m a
      -> ComposeT t1 t2 m a
    lift = toComposeT . lift . lift

instance
  ( MonadFunctor t1, MonadFunctor t2, ComposableT t1
  ) => MonadFunctor (ComposeT t1 t2)
  where
    hoist
      :: ( Monad m, Monad n )
      => (forall u. m u -> n u)
      -> ComposeT t1 t2 m a
      -> ComposeT t1 t2 n a
    hoist f = toComposeT . hoist (hoist f) . unComposeT





instance
  ( EqIn (t1 (t2 m)), ComposableT t1
  ) => EqIn (ComposeT t1 t2 m)
  where
    newtype Context (ComposeT t1 t2 m)
      = ComposeTCtx
          { unComposeTCtx :: Context (t1 (t2 m))
          } deriving (Typeable)

    eqIn
      :: (Eq a)
      => Context (ComposeT t1 t2 m)
      -> ComposeT t1 t2 m a
      -> ComposeT t1 t2 m a
      -> Bool
    eqIn (ComposeTCtx h) x y =
      eqIn h (unComposeT x) (unComposeT y)

deriving instance
  ( Eq (Context (t1 (t2 m)))
  ) => Eq (Context (ComposeT t1 t2 m))

deriving instance
  ( Show (Context (t1 (t2 m)))
  ) => Show (Context (ComposeT t1 t2 m))



instance
  ( RunMonadTrans t1, RunMonadTrans t2, ComposableT t1
  ) => RunMonadTrans (ComposeT t1 t2)
  where
    newtype InputT (ComposeT t1 t2)
      = ComposeTIn
          { unComposeTIn :: (InputT t1, InputT t2)
          } deriving (Typeable)

    newtype OutputT (ComposeT t1 t2) a
      = ComposeTOut
          { unComposeTOut :: Compose (OutputT t2) (OutputT t1) a
          } deriving (Typeable)

    runT
      :: ( Monad m )
      => InputT (ComposeT t1 t2)
      -> ComposeT t1 t2 m a
      -> m (OutputT (ComposeT t1 t2) a)
    runT (ComposeTIn (z1,z2)) =
      fmap (ComposeTOut . Compose) . runT z2 . runT z1 . unComposeT

deriving instance
  ( Eq (InputT t1), Eq (InputT t2)
  ) => Eq (InputT (ComposeT t1 t2))

deriving instance
  ( Show (InputT t1), Show (InputT t2)
  ) => Show (InputT (ComposeT t1 t2))

deriving instance
  ( Eq (OutputT t2 (OutputT t1 a))
  ) => Eq (OutputT (ComposeT t1 t2) a)

deriving instance
  ( Show (OutputT t2 (OutputT t1 a))
  ) => Show (OutputT (ComposeT t1 t2) a)





{- Specialized Lifts -}

instance
  ( LiftCatch t1, LiftCatch t2, ComposableT t1
  ) => LiftCatch (ComposeT t1 t2)
  where
    liftCatch
      :: forall e a m
       . ( Monad m )
      => Catch e m (OutputT (ComposeT t1 t2) a)
      -> Catch e (ComposeT t1 t2 m) a
    liftCatch catch x h = do
      let
        catch' :: Catch e m (OutputT t2 (OutputT t1 a))
        catch' x' h' = fmap (unCompose . unComposeTOut) $
          catch (fmap (ComposeTOut . Compose) x') (fmap (ComposeTOut . Compose) . h')
      toComposeT $ liftCatch (liftCatch catch') (unComposeT x) (unComposeT . h)

instance
  ( LiftDraft t1, LiftDraft t2, ComposableT t1
  ) => LiftDraft (ComposeT t1 t2)
  where
    liftDraft
      :: forall w m a
       . ( Monad m, Monoid w )
      => Draft w m (OutputT (ComposeT t1 t2) a)
      -> Draft w (ComposeT t1 t2 m) a
    liftDraft draft x = do
      let
        draft' :: Draft w m (OutputT t2 (OutputT t1 a))
        draft' y = fmap (fmap (unCompose . unComposeTOut)) $
          draft (fmap (ComposeTOut . Compose) y)
      toComposeT $ liftDraft (liftDraft draft') (unComposeT x)

instance
  ( LiftLocal t1, LiftLocal t2, ComposableT t1
  ) => LiftLocal (ComposeT t1 t2)
  where
    liftLocal
      :: forall r m a
       . ( Monad m )
      => Local r m (OutputT (ComposeT t1 t2) a)
      -> Local r (ComposeT t1 t2 m) a
    liftLocal local f x = do
      let
        local' :: Local r m (OutputT t2 (OutputT t1 a))
        local' g y = fmap (unCompose . unComposeTOut) $
          local g (fmap (ComposeTOut . Compose) y)
      toComposeT $ liftLocal (liftLocal local') f $ unComposeT x





{- Effect Classes -}

instance
  ( MonadIdentity m, MonadTrans t1, MonadTrans t2, ComposableT t1
  , forall x. (MonadIdentity x) => MonadIdentity (t1 x)
  , forall x. (MonadIdentity x) => MonadIdentity (t2 x)
  ) => MonadIdentity (ComposeT t1 t2 m)
  where
    unwrap
      :: ComposeT t1 t2 m a
      -> a
    unwrap = unwrap . unComposeT



instance  {-# OVERLAPS #-}
  ( Monad m, MonadTrans t2, MonadIdentity mark
  ) => MonadExcept mark e (ComposeT (ExceptT mark e) t2 m)
  where
    throw
      :: mark e
      -> ComposeT (ExceptT mark e) t2 m a
    throw = toComposeT . throw

    catch
      :: ComposeT (ExceptT mark e) t2 m a
      -> (mark e -> ComposeT (ExceptT mark e) t2 m a)
      -> ComposeT (ExceptT mark e) t2 m a
    catch x h = toComposeT $ catch (unComposeT x) (unComposeT . h)

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t1, MonadTrans t2, ComposableT t1
  , MonadIdentity mark, LiftCatch t1
  , forall x. (Monad x) => MonadExcept mark e (t2 x)
  ) => MonadExcept mark e (ComposeT t1 t2 m)
  where
    throw
      :: mark e
      -> ComposeT t1 t2 m a
    throw = toComposeT . lift . throw

    catch
      :: ComposeT t1 t2 m a
      -> (mark e -> ComposeT t1 t2 m a)
      -> ComposeT t1 t2 m a
    catch x h = toComposeT $
      liftCatch catch (unComposeT x) (unComposeT . h)



instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t2, Monoid w, MonadIdentity mark
  ) => MonadWriteOnly mark w (ComposeT (WriteOnlyT mark w) t2 m)
  where
    draft
      :: ComposeT (WriteOnlyT mark w) t2 m a
      -> ComposeT (WriteOnlyT mark w) t2 m (Pair (mark w) a)
    draft = toComposeT . draft . unComposeT

    tell
      :: mark w
      -> ComposeT (WriteOnlyT mark w) t2 m ()
    tell = toComposeT . tell

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t1, MonadTrans t2, Monoid w, ComposableT t1
  , LiftDraft t1, MonadIdentity mark
  , forall x. (Monad x) => MonadWriteOnly mark w (t2 x)
  ) => MonadWriteOnly mark w (ComposeT t1 t2 m)
  where
    draft
      :: ComposeT t1 t2 m a
      -> ComposeT t1 t2 m (Pair (mark w) a)
    draft = toComposeT . liftDraft draft . unComposeT

    tell
      :: mark w
      -> ComposeT t1 t2 m ()
    tell = toComposeT . lift . tell



instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t2, MonadIdentity mark
  ) => MonadState mark s (ComposeT (StateT mark s) t2 m)
  where
    get
      :: ComposeT (StateT mark s) t2 m (mark s)
    get = toComposeT get

    put
      :: mark s -> ComposeT (StateT mark s) t2 m ()
    put = toComposeT . put

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t1, MonadTrans t2, MonadIdentity mark, ComposableT t1
  , forall x. (Monad x) => MonadState mark s (t2 x)
  ) => MonadState mark s (ComposeT t1 t2 m)
  where
    get
      :: ComposeT t1 t2 m (mark s)
    get = toComposeT $ lift get

    put
      :: mark s -> ComposeT t1 t2 m ()
    put = toComposeT . lift . put



instance  {-# OVERLAPS #-}
  ( Monad m, MonadTrans t2, MonadIdentity mark
  ) => MonadReadOnly mark r (ComposeT (ReadOnlyT mark r) t2 m)
  where
    ask
      :: ComposeT (ReadOnlyT mark r) t2 m (mark r)
    ask = toComposeT ask

    local
      :: (mark r -> mark r)
      -> ComposeT (ReadOnlyT mark r) t2 m a
      -> ComposeT (ReadOnlyT mark r) t2 m a
    local f = toComposeT . local f . unComposeT

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t1, MonadTrans t2, ComposableT t1
  , LiftLocal t1, MonadIdentity mark
  , forall x. (Monad x) => MonadReadOnly mark r (t2 x)
  ) => MonadReadOnly mark r (ComposeT t1 t2 m)
  where
    ask
      :: ComposeT t1 t2 m (mark r)
    ask = toComposeT $ lift ask

    local
      :: (mark r -> mark r)
      -> ComposeT t1 t2 m a
      -> ComposeT t1 t2 m a
    local f = toComposeT . liftLocal local f . unComposeT



instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t2, MonadIdentity mark
  ) => MonadHalt mark (ComposeT (HaltT mark) t2 m)
  where
    halt
      :: mark ()
      -> ComposeT (HaltT mark) t2 m a
    halt = toComposeT . halt

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t1, MonadTrans t2, ComposableT t1
  , LiftLocal t1, MonadIdentity mark
  , forall x. (Monad x) => MonadHalt mark (t2 x)
  ) => MonadHalt mark (ComposeT t1 t2 m)
  where
    halt
      :: mark ()
      -> ComposeT t1 t2 m a
    halt = toComposeT . lift . halt



instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t2, Monoid w, MonadIdentity mark
  ) => MonadAppendOnly mark w (ComposeT (AppendOnlyT mark w) t2 m)
  where
    look
      :: ComposeT (AppendOnlyT mark w) t2 m (mark w)
    look = toComposeT look

    jot
      :: mark w
      -> ComposeT (AppendOnlyT mark w) t2 m ()
    jot = toComposeT . jot

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t1, MonadTrans t2, Monoid w, ComposableT t1
  , LiftDraft t1, MonadIdentity mark
  , forall x. (Monad x) => MonadAppendOnly mark w (t2 x)
  ) => MonadAppendOnly mark w (ComposeT t1 t2 m)
  where
    look
      :: ComposeT t1 t2 m (mark w)
    look = toComposeT $ lift look

    jot
      :: mark w
      -> ComposeT t1 t2 m ()
    jot = toComposeT . lift . jot



instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t2, MonadIdentity mark
  ) => MonadWriteOnce mark w (ComposeT (WriteOnceT mark w) t2 m)
  where
    press
      :: ComposeT (WriteOnceT mark w) t2 m (Maybe (mark w))
    press = toComposeT press

    etch
      :: mark w
      -> ComposeT (WriteOnceT mark w) t2 m Bool
    etch = toComposeT . etch

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t1, MonadTrans t2, ComposableT t1
  , LiftDraft t1, MonadIdentity mark
  , forall x. (Monad x) => MonadWriteOnce mark w (t2 x)
  ) => MonadWriteOnce mark w (ComposeT t1 t2 m)
  where
    press
      :: ComposeT t1 t2 m (Maybe (mark w))
    press = toComposeT $ lift press

    etch
      :: mark w
      -> ComposeT t1 t2 m Bool
    etch = toComposeT . lift . etch
