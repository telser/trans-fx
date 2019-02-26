-- | Module      : Control.FX.Monad.Trans.WriteOnlyT
--   Description : Concrete write-only state monad transformer
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.Trans.WriteOnlyT (
    WriteOnlyT(..)
) where



import Data.Typeable (Typeable)
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

instance
  ( EqIn h (m (WriteOnly mark w a)), MonadIdentity mark
  ) => EqIn (mark (), h) (WriteOnlyT mark w m a)
  where
    eqIn
      :: (mark (), h)
      -> WriteOnlyT mark w m a
      -> WriteOnlyT mark w m a
      -> Bool
    eqIn (_,h) (WriteOnlyT x) (WriteOnlyT y) =
      eqIn h x y

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
    commute = fmap (WriteOnlyT) . commute . fmap commute . unWriteOnlyT

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
  ( Monoid w, MonadIdentity mark
  ) => RunMonadTrans (mark ()) (WriteOnlyT mark w) (Pair (mark w))
  where
    runT
      :: ( Monad m )
      => mark ()
      -> WriteOnlyT mark w m a
      -> m (Pair (mark w) a)
    runT _ (WriteOnlyT x) =
      fmap (bimap1 pure . unWriteOnly) x



{- Specialized Lifts -}

instance
  ( Monoid w, MonadIdentity mark
  ) => LiftCatch (mark ()) (WriteOnlyT mark w) (Pair (mark w))
  where
    liftCatch
      :: ( Monad m )
      => Catch e m (Pair (mark w) a)
      -> Catch e (WriteOnlyT mark w m) a
    liftCatch catch x h = WriteOnlyT $
      fmap (WriteOnly . bimap1 unwrap) $ catch
        (fmap (bimap1 pure . unWriteOnly) $ unWriteOnlyT x)
        (\e -> fmap (bimap1 pure . unWriteOnly) $ unWriteOnlyT $ h e)

instance
  ( Monoid w, MonadIdentity mark
  , forall x. (Monoid x) => Monoid (mark x)
  ) => LiftDraft (mark ()) (WriteOnlyT mark w) (Pair (mark w))
  where
    liftDraft
      :: ( Monad m )
      => Draft w2 m (Pair (mark w) a)
      -> Draft w2 (WriteOnlyT mark w m) a
    liftDraft draft =
      WriteOnlyT . fmap (WriteOnly . bimap1 unwrap) . fmap commute
        . draft . fmap (bimap1 pure . unWriteOnly) . unWriteOnlyT

instance
  ( Monoid w, MonadIdentity mark
  ) => LiftLocal (mark ()) (WriteOnlyT mark w) (Pair (mark w))
  where
    liftLocal
      :: ( Monad m )
      => Local r m (Pair (mark w) a)
      -> Local r (WriteOnlyT mark w m) a
    liftLocal local f =
      WriteOnlyT . fmap (WriteOnly . bimap1 unwrap) . local f
        . fmap (bimap1 pure . unWriteOnly) . unWriteOnlyT



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
