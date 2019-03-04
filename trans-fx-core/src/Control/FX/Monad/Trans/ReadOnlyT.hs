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
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.Trans.ReadOnlyT (
    ReadOnlyT(..)
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

type instance Context (ReadOnlyT mark r m)
  = (mark r, Context m)

instance
  ( EqIn m, Functor m, MonadIdentity mark
  ) => EqIn (ReadOnlyT mark r m)
  where
    eqIn
      :: (Eq a)
      => (mark r, Context m)
      -> ReadOnlyT mark r m a
      -> ReadOnlyT mark r m a
      -> Bool
    eqIn (r,h) (ReadOnlyT x) (ReadOnlyT y) =
      eqIn h (unReadOnly x $ unwrap r) (unReadOnly y $ unwrap r)

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
  ( MonadIdentity mark, Commutant mark
  ) => RunMonadTrans (mark r) (ReadOnlyT mark r) mark
  where
    runT
      :: ( Monad m )
      => mark r
      -> ReadOnlyT mark r m a
      -> m (mark a)
    runT r (ReadOnlyT x) =
      fmap pure $ unReadOnly x (unwrap r)



{- Specialized Lifts -}

instance
  ( MonadIdentity mark, Commutant mark
  ) => LiftCatch (mark r) (ReadOnlyT mark r) mark
  where
    liftCatch
      :: ( Monad m )
      => Catch e m (mark a)
      -> Catch e (ReadOnlyT mark r m) a
    liftCatch catch (ReadOnlyT x) h = ReadOnlyT $ ReadOnly $ \r ->
      fmap unwrap $ catch
        (fmap pure $ unReadOnly x r)
        (\e -> fmap pure $ unReadOnly (unReadOnlyT $ h e) r)

instance
  ( MonadIdentity mark, Commutant mark
  ) => LiftDraft (mark r) (ReadOnlyT mark r) mark
  where
    liftDraft
      :: ( Monad m )
      => Draft w m (mark a)
      -> Draft w (ReadOnlyT mark r m) a
    liftDraft draft =
      ReadOnlyT . fmap (fmap (fmap unwrap) . draft . fmap pure) . unReadOnlyT

instance
  ( MonadIdentity mark, Commutant mark
  ) => LiftLocal (mark r) (ReadOnlyT mark r) mark
  where
    liftLocal
      :: ( Monad m )
      => Local r2 m (mark a)
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
