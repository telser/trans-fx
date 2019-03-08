-- | Module      : Control.FX.Monad.Trans.ReadOnlyT
--   Description : Concrete read-only state monad transformer
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.Trans.ReadOnlyT (
    ReadOnlyT(..)
  , Context(..)
  , InputT(..)
  , OutputT(..)
) where



import Data.Typeable (Typeable)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans.Class



-- | Concrete @ReadOnly@ monad transformer
newtype ReadOnlyT
  (mark :: * -> *)
  (r :: *)
  (m :: * -> *)
  (a :: *)
    = ReadOnlyT
        { unReadOnlyT :: ReadOnly mark r (m a)
        } deriving (Typeable)



deriving instance
  ( Typeable r, Typeable m, Typeable a, Typeable mark
  ) => Show (ReadOnlyT mark r m a)

instance
  ( Functor m, MonadIdentity mark
  ) => Functor (ReadOnlyT mark r m)
  where
    fmap
      :: (a -> b)
      -> ReadOnlyT mark r m a
      -> ReadOnlyT mark r m b
    fmap f (ReadOnlyT x) =
      ReadOnlyT $ fmap (fmap f) x

instance
  ( Applicative m, MonadIdentity mark
  ) => Applicative (ReadOnlyT mark r m)
  where
    pure
      :: a
      -> ReadOnlyT mark r m a
    pure x =
      ReadOnlyT $ ReadOnly $ \_ -> pure x

    (<*>)
      :: ReadOnlyT mark r m (a -> b)
      -> ReadOnlyT mark r m a
      -> ReadOnlyT mark r m b
    (ReadOnlyT f) <*> (ReadOnlyT x) =
      ReadOnlyT $ ReadOnly $ \r ->
        (unReadOnly f r) <*> (unReadOnly x r)

instance
  ( Monad m, MonadIdentity mark
  ) => Monad (ReadOnlyT mark r m)
  where
    return
      :: a
      -> ReadOnlyT mark r m a
    return x =
      ReadOnlyT $ ReadOnly $ \_ -> return x

    (>>=)
      :: ReadOnlyT mark r m a
      -> (a -> ReadOnlyT mark r m b)
      -> ReadOnlyT mark r m b
    (ReadOnlyT x) >>= f =
      ReadOnlyT $ ReadOnly $ \r ->
        (unReadOnly x r >>= (($ r) . unReadOnly . unReadOnlyT . f))

instance
  ( MonadIdentity mark
  ) => MonadTrans (ReadOnlyT mark r)
  where
    lift
      :: ( Monad m )
      => m a
      -> ReadOnlyT mark r m a
    lift x = ReadOnlyT $ ReadOnly $ \_ -> x

instance
  ( MonadIdentity mark
  ) => MonadFunctor (ReadOnlyT mark r)
  where
    hoist
      :: ( Monad m, Monad n )
      => (forall u. m u -> n u)
      -> ReadOnlyT mark r m a
      -> ReadOnlyT mark r n a
    hoist f (ReadOnlyT x) =
      ReadOnlyT $ ReadOnly $ \r ->
        f (unReadOnly x r)





instance
  ( EqIn m, Functor m, MonadIdentity mark
  ) => EqIn (ReadOnlyT mark r m)
  where
    newtype Context (ReadOnlyT mark r m)
      = ReadOnlyTCtx
          { unReadOnlyTCtx :: (mark r, Context m)
          } deriving (Typeable)

    eqIn
      :: (Eq a)
      => Context (ReadOnlyT mark r m)
      -> ReadOnlyT mark r m a
      -> ReadOnlyT mark r m a
      -> Bool
    eqIn (ReadOnlyTCtx (r,h)) (ReadOnlyT x) (ReadOnlyT y) =
      eqIn h (unReadOnly x $ unwrap r) (unReadOnly y $ unwrap r)

deriving instance
  ( Eq (mark r), Eq (Context m)
  ) => Eq (Context (ReadOnlyT mark r m))

deriving instance
  ( Show (mark r), Show (Context m)
  ) => Show (Context (ReadOnlyT mark r m))



instance
  ( MonadIdentity mark, Commutant mark
  ) => RunMonadTrans (ReadOnlyT mark r)
  where
    newtype InputT (ReadOnlyT mark r)
      = ReadOnlyTIn
          { unReadOnlyTIn :: mark r
          } deriving (Typeable)

    newtype OutputT (ReadOnlyT mark r) a
      = ReadOnlyTOut
          { unReadOnlyTOut :: mark a
          } deriving (Typeable)

    runT
      :: ( Monad m )
      => InputT (ReadOnlyT mark r)
      -> ReadOnlyT mark r m a
      -> m (OutputT (ReadOnlyT mark r) a)
    runT (ReadOnlyTIn r) (ReadOnlyT x) =
      fmap pure $ unReadOnly x (unwrap r)

deriving instance
  ( Eq (mark r)
  ) => Eq (InputT (ReadOnlyT mark r))

deriving instance
  ( Show (mark r)
  ) => Show (InputT (ReadOnlyT mark r))

deriving instance
  ( Eq (mark a)
  ) => Eq (OutputT (ReadOnlyT mark r) a)

deriving instance
  ( Show (mark a)
  ) => Show (OutputT (ReadOnlyT mark r) a)

instance
  ( MonadIdentity mark
  ) => Functor (OutputT (ReadOnlyT mark r))
  where
    fmap f (ReadOnlyTOut x) = ReadOnlyTOut (fmap f x)

instance
  ( MonadIdentity mark
  ) => Applicative (OutputT (ReadOnlyT mark r))
  where
    pure = ReadOnlyTOut . pure

    (ReadOnlyTOut f) <*> (ReadOnlyTOut x) =
      ReadOnlyTOut (f <*> x)

instance
  ( MonadIdentity mark
  ) => Monad (OutputT (ReadOnlyT mark r))
  where
    return = ReadOnlyTOut . return

    (ReadOnlyTOut x) >>= f = ReadOnlyTOut (x >>= (unReadOnlyTOut . f))

instance
  ( Semigroup a, MonadIdentity mark
  ) => Semigroup (OutputT (ReadOnlyT mark r) a)
  where
    (ReadOnlyTOut x) <> (ReadOnlyTOut y) =
      ReadOnlyTOut (x <> y)

instance
  ( Monoid a, MonadIdentity mark
  ) => Monoid (OutputT (ReadOnlyT mark r) a)
  where
    mempty = ReadOnlyTOut mempty

instance
  ( MonadIdentity mark
  ) => MonadIdentity (OutputT (ReadOnlyT mark r))
  where
    unwrap = unwrap . unReadOnlyTOut



{- Specialized Lifts -}

instance
  ( MonadIdentity mark, Commutant mark
  ) => LiftCatch (ReadOnlyT mark r)
  where
    liftCatch
      :: ( Monad m )
      => Catch e m (OutputT (ReadOnlyT mark r) a)
      -> Catch e (ReadOnlyT mark r m) a
    liftCatch catch (ReadOnlyT x) h = ReadOnlyT $ ReadOnly $ \r ->
      fmap unwrap $ catch
        (fmap pure $ unReadOnly x r)
        (\e -> fmap pure $ unReadOnly (unReadOnlyT $ h e) r)

instance
  ( MonadIdentity mark, Commutant mark
  ) => LiftDraft (ReadOnlyT mark r)
  where
    liftDraft
      :: ( Monad m )
      => Draft w m (OutputT (ReadOnlyT mark r) a)
      -> Draft w (ReadOnlyT mark r m) a
    liftDraft draft =
      ReadOnlyT . fmap (fmap (fmap unwrap) . draft . fmap pure) . unReadOnlyT

instance
  ( MonadIdentity mark, Commutant mark
  ) => LiftLocal (ReadOnlyT mark r)
  where
    liftLocal
      :: ( Monad m )
      => Local r2 m (OutputT (ReadOnlyT mark r) a)
      -> Local r2 (ReadOnlyT mark r m) a
    liftLocal local f x =
      ReadOnlyT $ ReadOnly $ \r ->
        fmap unwrap $
          local f (fmap pure $ (unReadOnly $ unReadOnlyT x) r)





{- Effect Class -}

instance {-# OVERLAPPING #-}
  ( Monad m, MonadIdentity mark
  ) => MonadReadOnly mark r (ReadOnlyT mark r m)
  where
    ask
      :: ReadOnlyT mark r m (mark r)
    ask = ReadOnlyT $ ReadOnly $ \r ->
      return (pure r)

    local
      :: (mark r -> mark r)
      -> ReadOnlyT mark r m a
      -> ReadOnlyT mark r m a
    local f (ReadOnlyT (ReadOnly x)) =
      ReadOnlyT $ ReadOnly $ x . unwrap . f . pure

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadReadOnly mark r m, Commutant mark1
  ) => MonadReadOnly mark r (ReadOnlyT mark1 r1 m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadExcept mark e m, Commutant mark1
  ) => MonadExcept mark e (ReadOnlyT mark1 r m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadWriteOnly mark w m, Commutant mark1, Monoid w
  ) => MonadWriteOnly mark w (ReadOnlyT mark1 r m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadState mark s m, Commutant mark1
  ) => MonadState mark s (ReadOnlyT mark1 r m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadPrompt mark p m, Commutant mark1
  ) => MonadPrompt mark p (ReadOnlyT mark1 r m)

instance
  ( Monad m, MonadIdentity mark1, Commutant mark1, MonadIdentity mark
  , MonadHalt mark m
  ) => MonadHalt mark (ReadOnlyT mark1 r m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadAppendOnly mark w m, Commutant mark1, Monoid w
  ) => MonadAppendOnly mark w (ReadOnlyT mark1 r m)
