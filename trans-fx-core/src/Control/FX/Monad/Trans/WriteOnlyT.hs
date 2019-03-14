-- | Module      : Control.FX.Monad.Trans.WriteOnlyT
--   Description : Concrete write-only state monad transformer
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
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.Trans.WriteOnlyT (
    WriteOnlyT(..)
  , Context(..)
  , InputT(..)
  , OutputT(..)
) where



import Data.Typeable (Typeable, typeOf)
import Control.Applicative (liftA2)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans.Class



-- | Concrete write-only state monad transformer
newtype WriteOnlyT
  (mark :: * -> *)
  (w :: *)
  (m :: * -> *)
  (a :: *)
    = WriteOnlyT
        { unWriteOnlyT :: m (WriteOnly mark w a)
        } deriving (Typeable)



deriving instance
  ( Show (m (WriteOnly mark w a))
  ) => Show (WriteOnlyT mark w m a)

instance
  ( Monoid w, Monad m, MonadIdentity mark
  ) => Functor (WriteOnlyT mark w m)
  where
    fmap
      :: (a -> b)
      -> WriteOnlyT mark w m a
      -> WriteOnlyT mark w m b
    fmap f = WriteOnlyT . fmap (fmap f) . unWriteOnlyT

instance
  ( Monoid w, Monad m, MonadIdentity mark
  ) => Applicative (WriteOnlyT mark w m)
  where
    pure
      :: a
      -> WriteOnlyT mark w m a
    pure = WriteOnlyT . pure . pure

    (<*>)
      :: WriteOnlyT mark w m (a -> b)
      -> WriteOnlyT mark w m a
      -> WriteOnlyT mark w m b
    (WriteOnlyT f) <*> (WriteOnlyT x) =
      WriteOnlyT $ (liftA2 (<*>) f x)

instance
  ( Monoid w, Monad m, MonadIdentity mark
  ) => Monad (WriteOnlyT mark w m)
  where
    return
      :: a
      -> WriteOnlyT mark w m a
    return = WriteOnlyT . return . return

    (>>=)
      :: WriteOnlyT mark w m a
      -> (a -> WriteOnlyT mark w m b)
      -> WriteOnlyT mark w m b
    (WriteOnlyT x) >>= f =
      WriteOnlyT $ do
        WriteOnly (Pair w1 a) <- x
        WriteOnly (Pair w2 b) <- unWriteOnlyT $ f a
        return $ WriteOnly $ Pair (w1 <> w2) b

instance
  ( Monoid w, Central c, MonadIdentity mark
  ) => Commutant (WriteOnlyT mark w c)
  where
    commute
      :: ( Applicative f )
      => WriteOnlyT mark w c (f a)
      -> f (WriteOnlyT mark w c a)
    commute =
      fmap (WriteOnlyT) . commute . fmap commute . unWriteOnlyT

instance
  ( Monoid w, Central c, MonadIdentity mark
  ) => Central (WriteOnlyT mark w c)

instance
  ( Monoid w, MonadIdentity mark
  ) => MonadTrans (WriteOnlyT mark w)
  where
    lift
      :: ( Monad m )
      => m a
      -> WriteOnlyT mark w m a
    lift x = WriteOnlyT $ (x >>= (return . pure))

instance
  ( Monoid w, MonadIdentity mark
  ) => MonadFunctor (WriteOnlyT mark w)
  where
    hoist
      :: ( Monad m, Monad n )
      => (forall u. m u -> n u)
      -> WriteOnlyT mark w m a
      -> WriteOnlyT mark w n a
    hoist f = WriteOnlyT . f . unWriteOnlyT





instance
  ( EqIn m, MonadIdentity mark, Eq w
  ) => EqIn (WriteOnlyT mark w m)
  where
    data Context (WriteOnlyT mark w m)
      = WriteOnlyTCtx
          { unWriteOnlyTCtx :: (mark (), Context m)
          } deriving (Typeable)

    eqIn
      :: (Eq a)
      => Context (WriteOnlyT mark w m)
      -> WriteOnlyT mark w m a
      -> WriteOnlyT mark w m a
      -> Bool
    eqIn (WriteOnlyTCtx (_,h)) x y =
      eqIn h (unWriteOnlyT x) (unWriteOnlyT y)

deriving instance
  ( Show (mark ()), Show (Context m)
  ) => Show (Context (WriteOnlyT mark w m))

deriving instance
  ( Eq (mark ()), Eq (Context m)
  ) => Eq (Context (WriteOnlyT mark w m))



instance
  ( Monoid w, MonadIdentity mark
  ) => RunMonadTrans (WriteOnlyT mark w)
  where
    data InputT (WriteOnlyT mark w)
      = WriteOnlyTIn
          { unWriteOnlyTIn :: mark ()
          } deriving (Typeable)

    data OutputT (WriteOnlyT mark w) a
      = WriteOnlyTOut
          { unWriteOnlyTOut :: Pair (mark w) a
          } deriving (Typeable)

    runT
      :: ( Monad m )
      => InputT (WriteOnlyT mark w)
      -> WriteOnlyT mark w m a
      -> m (OutputT (WriteOnlyT mark w) a)
    runT _ (WriteOnlyT x) =
      fmap (WriteOnlyTOut . bimap1 pure . unWriteOnly) x

deriving instance
  ( Show (mark ())
  ) => Show (InputT (WriteOnlyT mark w))

deriving instance
  ( Eq (mark ())
  ) => Eq (InputT (WriteOnlyT mark w))

deriving instance
  ( Show a, Show (mark w)
  ) => Show (OutputT (WriteOnlyT mark w) a)

deriving instance
  ( Eq a, Eq (mark w)
  ) => Eq (OutputT (WriteOnlyT mark w) a)

instance
  ( Monoid w, MonadIdentity mark
  ) => Functor (OutputT (WriteOnlyT mark w))
  where
    fmap
      :: (a -> b)
      -> OutputT (WriteOnlyT mark w) a
      -> OutputT (WriteOnlyT mark w) b
    fmap f (WriteOnlyTOut x) =
      WriteOnlyTOut (fmap f x)

instance
  ( Monoid w, MonadIdentity mark
  ) => Applicative (OutputT (WriteOnlyT mark w))
  where
    pure
      :: a
      -> OutputT (WriteOnlyT mark w) a
    pure = WriteOnlyTOut . pure

    (<*>)
      :: OutputT (WriteOnlyT mark w) (a -> b)
      -> OutputT (WriteOnlyT mark w) a
      -> OutputT (WriteOnlyT mark w) b
    (WriteOnlyTOut f) <*> (WriteOnlyTOut x) =
      WriteOnlyTOut (f <*> x)





{- Specialized Lifts -}

instance
  ( Monoid w, MonadIdentity mark
  ) => LiftCatch (WriteOnlyT mark w)
  where
    liftCatch
      :: ( Monad m )
      => Catch e m (OutputT (WriteOnlyT mark w) a)
      -> Catch e (WriteOnlyT mark w m) a
    liftCatch catch x h = WriteOnlyT $
      fmap (WriteOnly . bimap1 unwrap . unWriteOnlyTOut) $ catch
        (fmap (WriteOnlyTOut . bimap1 pure . unWriteOnly) $ unWriteOnlyT x)
        (\e -> fmap (WriteOnlyTOut . bimap1 pure . unWriteOnly) $ unWriteOnlyT $ h e)

instance
  ( Monoid w, MonadIdentity mark
  , forall x. (Monoid x) => Monoid (mark x)
  ) => LiftDraft (WriteOnlyT mark w)
  where
    liftDraft
      :: ( Monad m )
      => Draft w2 m (OutputT (WriteOnlyT mark w) a)
      -> Draft w2 (WriteOnlyT mark w m) a
    liftDraft draft =
      WriteOnlyT . fmap (WriteOnly . bimap1 unwrap . unWriteOnlyTOut) . fmap commute
        . draft . fmap (WriteOnlyTOut . bimap1 pure . unWriteOnly) . unWriteOnlyT

instance
  ( Monoid w, MonadIdentity mark
  ) => LiftLocal (WriteOnlyT mark w)
  where
    liftLocal
      :: ( Monad m )
      => Local r m (OutputT (WriteOnlyT mark w) a)
      -> Local r (WriteOnlyT mark w m) a
    liftLocal local f =
      WriteOnlyT . fmap (WriteOnly . bimap1 unwrap . unWriteOnlyTOut) . local f
        . fmap (WriteOnlyTOut . bimap1 pure . unWriteOnly) . unWriteOnlyT



{- Effect Classes -}

instance {-# OVERLAPPING #-}
  ( Monoid w, Monad m, MonadIdentity mark
  ) => MonadWriteOnly mark w (WriteOnlyT mark w m)
  where
    draft
      :: WriteOnlyT mark w m a
      -> WriteOnlyT mark w m (Pair (mark w) a)
    draft = WriteOnlyT . fmap draft . unWriteOnlyT

    tell
      :: mark w
      -> WriteOnlyT mark w m ()
    tell = WriteOnlyT . return . tell

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadWriteOnly mark w m, Monoid w, Monoid w1
  ) => MonadWriteOnly mark w (WriteOnlyT mark1 w1 m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadReadOnly mark r m, Monoid w
  ) => MonadReadOnly mark r (WriteOnlyT mark1 w m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadState mark s m, Monoid w
  ) => MonadState mark s (WriteOnlyT mark1 w m)

instance
  ( Monad m, MonadIdentity mark1, Monoid w, MonadIdentity mark
  , MonadHalt mark m
  ) => MonadHalt mark (WriteOnlyT mark1 w m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1, Monoid w
  , MonadExcept mark e m
  ) => MonadExcept mark e (WriteOnlyT mark1 w m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1, Monoid w
  , MonadPrompt mark p m
  ) => MonadPrompt mark p (WriteOnlyT mark1 w m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadAppendOnly mark w m, Monoid w, Monoid w1
  ) => MonadAppendOnly mark w (WriteOnlyT mark1 w1 m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadWriteOnce mark w m, Monoid w1
  ) => MonadWriteOnce mark w (WriteOnlyT mark1 w1 m)
