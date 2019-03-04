-- | Module      : Control.FX.Monad.Trans.ComposeT
--   Description : Concrete composite monad transformer
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

module Control.FX.Monad.Trans.ComposeT (
    ComposeT(..)
) where



import Data.Typeable (Typeable)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans.Class
import Control.FX.Monad.Trans.ApplyT
import Control.FX.Monad.Trans.ExceptT
import Control.FX.Monad.Trans.WriteOnlyT
import Control.FX.Monad.Trans.StateT
import Control.FX.Monad.Trans.ReadOnlyT
import Control.FX.Monad.Trans.HaltT
import Control.FX.Monad.Trans.AppendOnlyT



-- | Concrete composite monad transformer
newtype ComposeT
  (t1 :: (* -> *) -> * -> *)
  (t2 :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = ComposeT
        { unComposeT :: t1 (t2 m) a
        } deriving (Typeable)

type instance Context (ComposeT t1 t2 m)
  = Context (t1 (t2 m))

instance
  ( EqIn (t1 (t2 m))
  ) => EqIn (ComposeT t1 t2 m)
  where
    eqIn
      :: (Eq a)
      => Context (t1 (t2 m))
      -> ComposeT t1 t2 m a
      -> ComposeT t1 t2 m a
      -> Bool
    eqIn h (ComposeT x) (ComposeT y) =
      eqIn h x y

deriving instance
  ( Show (t1 (t2 m) a)
  ) => Show (ComposeT t1 t2 m a)

instance
  ( Monad m, MonadTrans t1, MonadTrans t2
  ) => Functor (ComposeT t1 t2 m)
  where
    fmap
      :: (a -> b)
      -> ComposeT t1 t2 m a
      -> ComposeT t1 t2 m b
    fmap f = ComposeT . fmap f . unComposeT
      

instance
  ( Monad m, MonadTrans t1, MonadTrans t2
  ) => Applicative (ComposeT t1 t2 m)
  where
    pure
      :: a
      -> ComposeT t1 t2 m a
    pure = ComposeT . pure

    (<*>)
      :: ComposeT t1 t2 m (a -> b)
      -> ComposeT t1 t2 m a
      -> ComposeT t1 t2 m b
    (ComposeT f) <*> (ComposeT x) =
      ComposeT (f <*> x)

instance
  ( Monad m, MonadTrans t1, MonadTrans t2
  ) => Monad (ComposeT t1 t2 m)
  where
    return
      :: a
      -> ComposeT t1 t2 m a
    return = ComposeT . return

    (>>=)
      :: ComposeT t1 t2 m a
      -> (a -> ComposeT t1 t2 m b)
      -> ComposeT t1 t2 m b
    (ComposeT x) >>= f =
      ComposeT (x >>= (unComposeT . f))

instance
  ( MonadIdentity m, MonadTrans t1, MonadTrans t2, Eq a
  , MonadIdentity (t2 m), MonadIdentity (t1 (t2 m))
  ) => Eq (ComposeT t1 t2 m a)
  where
    (==)
      :: ComposeT t1 t2 m a
      -> ComposeT t1 t2 m a
      -> Bool
    (ComposeT x) == (ComposeT y) =
      (unwrap x) == (unwrap y)

instance
  ( MonadIdentity m, MonadTrans t1, MonadTrans t2, Semigroup a
  , MonadIdentity (t2 m) , MonadIdentity (t1 (t2 m))
  ) => Semigroup (ComposeT t1 t2 m a)
  where
    (<>)
      :: ComposeT t1 t2 m a
      -> ComposeT t1 t2 m a
      -> ComposeT t1 t2 m a
    (ComposeT a) <> (ComposeT b) =
      ComposeT (a <> b)

instance
  ( MonadIdentity m, MonadTrans t1, MonadTrans t2, Monoid a
  , MonadIdentity (t2 m) , MonadIdentity (t1 (t2 m))
  ) => Monoid (ComposeT t1 t2 m a)
  where
    mempty
      :: ComposeT t1 t2 m a
    mempty = ComposeT mempty

    mappend
      :: ComposeT t1 t2 m a
      -> ComposeT t1 t2 m a
      -> ComposeT t1 t2 m a
    mappend = (<>)

instance
  ( MonadTrans t1, MonadTrans t2
  ) => MonadTrans (ComposeT t1 t2)
  where
    lift
      :: ( Monad m )
      => m a
      -> ComposeT t1 t2 m a
    lift = ComposeT . lift . lift

instance
  ( MonadFunctor t1, MonadFunctor t2
  ) => MonadFunctor (ComposeT t1 t2)
  where
    hoist
      :: ( Monad m, Monad n )
      => (forall u. m u -> n u)
      -> ComposeT t1 t2 m a
      -> ComposeT t1 t2 n a
    hoist f = ComposeT . hoist (hoist f) . unComposeT

instance
  ( RunMonadTrans z1 t1 f1, RunMonadTrans z2 t2 f2
  ) => RunMonadTrans (z1,z2) (ComposeT t1 t2) (Compose f2 f1)
  where
    runT
      :: ( Monad m )
      => (z1,z2)
      -> ComposeT t1 t2 m a
      -> m (Compose f2 f1 a)
    runT (z1,z2) = fmap Compose . runT z2 . runT z1 . unComposeT





{- Specialized Lifts -}

instance
  ( LiftCatch z1 t1 f1, LiftCatch z2 t2 f2
  ) => LiftCatch (z1,z2) (ComposeT t1 t2) (Compose f2 f1)
  where
    liftCatch
      :: forall e a m
       . ( Monad m )
      => Catch e m (Compose f2 f1 a)
      -> Catch e (ComposeT t1 t2 m) a
    liftCatch catch x h = do
      let
        catch' :: Catch e m (f2 (f1 a))
        catch' x' h' = fmap unCompose $ catch (fmap Compose x') (fmap Compose . h')
      ComposeT $ liftCatch (liftCatch catch') (unComposeT x) (unComposeT . h)

instance
  ( LiftDraft z1 t1 f1, LiftDraft z2 t2 f2
  ) => LiftDraft (z1,z2) (ComposeT t1 t2) (Compose f2 f1)
  where
    liftDraft
      :: forall w m a
       . ( Monad m, Monoid w )
      => Draft w m (Compose f2 f1 a)
      -> Draft w (ComposeT t1 t2 m) a
    liftDraft draft x = do
      let
        draft' :: Draft w m (f2 (f1 a))
        draft' y = fmap (fmap unCompose) $ draft (fmap Compose y)
      ComposeT $ liftDraft (liftDraft draft') (unComposeT x)

instance
  ( LiftLocal z1 t1 f1, LiftLocal z2 t2 f2
  ) => LiftLocal (z1,z2) (ComposeT t1 t2) (Compose f2 f1)
  where
    liftLocal
      :: forall r m a
       . ( Monad m )
      => Local r m (Compose f2 f1 a)
      -> Local r (ComposeT t1 t2 m) a
    liftLocal local f x = do
      let
        local' :: Local r m (f2 (f1 a))
        local' g y = fmap unCompose $ local g (fmap Compose y)
      ComposeT $ liftLocal (liftLocal local') f $ unComposeT x





{- Effect Classes -}

instance
  ( MonadIdentity m, MonadTrans t1, MonadTrans t2
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
    throw = ComposeT . throw

    catch
      :: ComposeT (ExceptT mark e) t2 m a
      -> (mark e -> ComposeT (ExceptT mark e) t2 m a)
      -> ComposeT (ExceptT mark e) t2 m a
    catch x h = ComposeT $ catch (unComposeT x) (unComposeT . h)

instance  {-# OVERLAPS #-}
  ( Monad m, MonadTrans t1, MonadTrans t2, MonadIdentity mark
  , forall x. (Monad x) => MonadExcept mark e (t1 x)
  ) => MonadExcept mark e (ComposeT (ApplyT t1) t2 m)
  where
    throw
      :: mark e
      -> ComposeT (ApplyT t1) t2 m a
    throw = ComposeT . ApplyT . throw

    catch
      :: ComposeT (ApplyT t1) t2 m a
      -> (mark e -> ComposeT (ApplyT t1) t2 m a)
      -> ComposeT (ApplyT t1) t2 m a
    catch x h = ComposeT $ ApplyT $
      catch
        (unApplyT $ unComposeT x)
        (unApplyT . unComposeT . h)

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t1, MonadTrans t2
  , MonadIdentity mark, LiftCatch z1 t1 f1
  , forall x. (Monad x) => MonadExcept mark e (t2 x)
  ) => MonadExcept mark e (ComposeT t1 t2 m)
  where
    throw
      :: mark e
      -> ComposeT t1 t2 m a
    throw = ComposeT . lift . throw

    catch
      :: ComposeT t1 t2 m a
      -> (mark e -> ComposeT t1 t2 m a)
      -> ComposeT t1 t2 m a
    catch x h = ComposeT $
      liftCatch catch (unComposeT x) (unComposeT . h)



instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t2, Monoid w, MonadIdentity mark
  ) => MonadWriteOnly mark w (ComposeT (WriteOnlyT mark w) t2 m)
  where
    draft
      :: ComposeT (WriteOnlyT mark w) t2 m a
      -> ComposeT (WriteOnlyT mark w) t2 m (Pair (mark w) a)
    draft = ComposeT . draft . unComposeT

    tell
      :: mark w
      -> ComposeT (WriteOnlyT mark w) t2 m ()
    tell = ComposeT . tell

instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t1, MonadTrans t2, Monoid w, MonadIdentity mark
  , forall x. (Monad x) => MonadWriteOnly mark w (t1 x)
  ) => MonadWriteOnly mark w (ComposeT (ApplyT t1) t2 m)
  where
    draft
      :: ComposeT (ApplyT t1) t2 m a
      -> ComposeT (ApplyT t1) t2 m (Pair (mark w) a)
    draft = ComposeT . ApplyT . draft . unApplyT . unComposeT

    tell
      :: mark w
      -> ComposeT (ApplyT t1) t2 m ()
    tell = ComposeT . ApplyT . tell

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t1, MonadTrans t2, Monoid w
  , LiftDraft z1 t1 f1, MonadIdentity mark
  , forall x. (Monad x) => MonadWriteOnly mark w (t2 x)
  ) => MonadWriteOnly mark w (ComposeT t1 t2 m)
  where
    draft
      :: ComposeT t1 t2 m a
      -> ComposeT t1 t2 m (Pair (mark w) a)
    draft = ComposeT . liftDraft draft . unComposeT

    tell
      :: mark w
      -> ComposeT t1 t2 m ()
    tell = ComposeT . lift . tell



instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t2, MonadIdentity mark
  ) => MonadState mark s (ComposeT (StateT mark s) t2 m)
  where
    get
      :: ComposeT (StateT mark s) t2 m (mark s)
    get = ComposeT get

    put
      :: mark s -> ComposeT (StateT mark s) t2 m ()
    put = ComposeT . put

instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t1, MonadTrans t2, MonadIdentity mark
  , forall x. (Monad x) => MonadState mark s (t1 x)
  ) => MonadState mark s (ComposeT (ApplyT t1) t2 m)
  where
    get
      :: ComposeT (ApplyT t1) t2 m (mark s)
    get = ComposeT $ ApplyT get

    put
      :: mark s -> ComposeT (ApplyT t1) t2 m ()
    put = ComposeT . ApplyT . put

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t1, MonadTrans t2, MonadIdentity mark
  , forall x. (Monad x) => MonadState mark s (t2 x)
  ) => MonadState mark s (ComposeT t1 t2 m)
  where
    get
      :: ComposeT t1 t2 m (mark s)
    get = ComposeT $ lift get

    put
      :: mark s -> ComposeT t1 t2 m ()
    put = ComposeT . lift . put



instance  {-# OVERLAPS #-}
  ( Monad m, MonadTrans t2, MonadIdentity mark
  ) => MonadReadOnly mark r (ComposeT (ReadOnlyT mark r) t2 m)
  where
    ask
      :: ComposeT (ReadOnlyT mark r) t2 m (mark r)
    ask = ComposeT ask

    local
      :: (mark r -> mark r)
      -> ComposeT (ReadOnlyT mark r) t2 m a
      -> ComposeT (ReadOnlyT mark r) t2 m a
    local f = ComposeT . local f . unComposeT

instance  {-# OVERLAPS #-}
  ( Monad m, MonadTrans t1, MonadTrans t2, MonadIdentity mark
  , forall x. (Monad x) => MonadReadOnly mark r (t1 x)
  ) => MonadReadOnly mark r (ComposeT (ApplyT t1) t2 m)
  where
    ask
      :: ComposeT (ApplyT t1) t2 m (mark r)
    ask = ComposeT $ ApplyT ask

    local
      :: (mark r -> mark r)
      -> ComposeT (ApplyT t1) t2 m a
      -> ComposeT (ApplyT t1) t2 m a
    local f = ComposeT . ApplyT . local f . unApplyT . unComposeT

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t1, MonadTrans t2
  , LiftLocal z1 t1 f1, MonadIdentity mark
  , forall x. (Monad x) => MonadReadOnly mark r (t2 x)
  ) => MonadReadOnly mark r (ComposeT t1 t2 m)
  where
    ask
      :: ComposeT t1 t2 m (mark r)
    ask = ComposeT $ lift ask

    local
      :: (mark r -> mark r)
      -> ComposeT t1 t2 m a
      -> ComposeT t1 t2 m a
    local f = ComposeT . liftLocal local f . unComposeT



instance  {-# OVERLAPS #-}
  ( Monad m, MonadTrans t2, MonadIdentity mark
  ) => MonadHalt mark (ComposeT (HaltT mark) t2 m)
  where
    halt
      :: mark ()
      -> ComposeT (HaltT mark) t2 m r
    halt = ComposeT . halt

instance  {-# OVERLAPS #-}
  ( Monad m, MonadTrans t1, MonadTrans t2, MonadIdentity mark
  , forall x. (Monad x) => MonadHalt mark (t1 x)
  ) => MonadHalt mark (ComposeT (ApplyT t1) t2 m)
  where
    halt
      :: mark ()
      -> ComposeT (ApplyT t1) t2 m r
    halt = ComposeT . halt

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t1, MonadTrans t2
  , LiftLocal z1 t1 f1, MonadIdentity mark
  , forall x. (Monad x) => MonadHalt mark (t2 x)
  ) => MonadHalt mark (ComposeT t1 t2 m)
  where
    halt
      :: mark ()
      -> ComposeT t1 t2 m r
    halt = ComposeT . lift . halt



instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t2, Monoid w, MonadIdentity mark
  ) => MonadAppendOnly mark w (ComposeT (AppendOnlyT mark w) t2 m)
  where
    look
      :: ComposeT (AppendOnlyT mark w) t2 m (mark w)
    look = ComposeT look

    jot
      :: mark w
      -> ComposeT (AppendOnlyT mark w) t2 m ()
    jot = ComposeT . jot

instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t1, MonadTrans t2, Monoid w, MonadIdentity mark
  , forall x. (Monad x) => MonadAppendOnly mark w (t1 x)
  ) => MonadAppendOnly mark w (ComposeT (ApplyT t1) t2 m)
  where
    look
      :: ComposeT (ApplyT t1) t2 m (mark w)
    look = ComposeT $ ApplyT look

    jot
      :: mark w
      -> ComposeT (ApplyT t1) t2 m ()
    jot = ComposeT . ApplyT . jot

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t1, MonadTrans t2, Monoid w
  , LiftDraft z1 t1 f1, MonadIdentity mark
  , forall x. (Monad x) => MonadAppendOnly mark w (t2 x)
  ) => MonadAppendOnly mark w (ComposeT t1 t2 m)
  where
    look
      :: ComposeT t1 t2 m (mark w)
    look = ComposeT $ lift look

    jot
      :: mark w
      -> ComposeT t1 t2 m ()
    jot = ComposeT . lift . jot
