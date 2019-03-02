-- Following Mario Blažević's Coroutine monad from _Coroutine Pipelines_ in The Monad.Reader Issue 19. https://themonadreader.files.wordpress.com/2011/10/issue19.pdf

{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}



module Control.FX.Monad.Trans.Trans.CoroutineTT (
    CoroutineTT(..)
  , runCoroutineTT
  , PogoM(..)
) where



import Control.Monad (liftM)
import Data.Typeable (Typeable, typeOf)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans
import Control.FX.Monad.Trans.Trans.Class



newtype CoroutineTT
  (mark :: * -> *)
  (sus :: * -> *)
  (t :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = CoroutineTT
        { unCoroutineTT :: t m (Muse sus (CoroutineTT mark sus t m) a)
        } deriving (Typeable)



instance
  ( Typeable mark, Typeable sus, Typeable t, Typeable m, Typeable a
  ) => Show (CoroutineTT mark sus t m a)
  where
    show
      :: CoroutineTT mark sus t m a
      -> String
    show = show . typeOf

instance
  ( Functor sus, MonadTrans t, Monad m, MonadIdentity mark
  ) => Functor (CoroutineTT mark sus t m)
  where
    fmap
      :: (a -> b)
      -> CoroutineTT mark sus t m a
      -> CoroutineTT mark sus t m b
    fmap f x = x >>= (return . f)

instance
  ( Functor sus, MonadTrans t, Monad m, MonadIdentity mark
  ) => Applicative (CoroutineTT mark sus t m)
  where
    pure
      :: a
      -> CoroutineTT mark sus t m a
    pure = CoroutineTT . return . Idea

    (<*>)
      :: CoroutineTT mark sus t m (a -> b)
      -> CoroutineTT mark sus t m a
      -> CoroutineTT mark sus t m b
    f' <*> x' = do
      f <- f'
      x <- x'
      return (f x)

instance
  ( Functor sus, MonadTrans t, Monad m, MonadIdentity mark
  ) => Monad (CoroutineTT mark sus t m)
  where
    return
      :: a
      -> CoroutineTT mark sus t m a
    return = CoroutineTT . return . Idea

    (>>=)
      :: CoroutineTT mark sus t m a
      -> (a -> CoroutineTT mark sus t m b)
      -> CoroutineTT mark sus t m b
    (CoroutineTT x) >>= f =
      let
        f' z = case z of
          Idea a -> unCoroutineTT (f a)
          Muse s -> return $ Muse (fmap (>>= f) s)
      in
        CoroutineTT (x >>= f')

instance
  ( Functor sus, MonadTrans t, MonadIdentity mark
  ) => MonadTrans (CoroutineTT mark sus t)
  where
    lift
      :: ( Monad m )
      => m a
      -> CoroutineTT mark sus t m a
    lift = CoroutineTT . lift . liftM Idea

instance
  ( Functor sus, MonadIdentity mark
  ) => MonadTransTrans (CoroutineTT mark sus)
  where
    liftT
      :: ( Monad m, MonadTrans t )
      => t m a
      -> CoroutineTT mark sus t m a
    liftT = CoroutineTT . liftM Idea

newtype PogoM
  (mark :: * -> *)
  (sus :: * -> *)
  (m :: * -> *)
    = PogoM
        { unPogoM
            :: forall a t
             . ( MonadTrans t )
            => sus (CoroutineTT mark sus t m a)
            -> m (CoroutineTT mark sus t m a)
        } deriving (Typeable)

instance
  ( Typeable m, Typeable mark, Typeable sus
  ) => Show (PogoM mark sus m)
  where
    show
      :: PogoM mark sus m
      -> String
    show = show . typeOf



instance
  ( Functor sus, MonadIdentity mark, Commutant mark
  ) => RunMonadTransTrans
    (PogoM mark sus) (CoroutineTT mark sus) mark
  where
    runTT
      :: ( Monad m, MonadTrans t )
      => PogoM mark sus m
      -> CoroutineTT mark sus t m a
      -> t m (mark a)
    runTT (PogoM k) = loop
      where
        loop (CoroutineTT c) =
          c >>= \x -> case x of
            Idea a -> return (pure a)
            Muse z -> (lift $ k z) >>= loop



runCoroutineTT
  :: ( Functor sus, MonadIdentity mark, Commutant mark, Monad m, MonadTrans t )
  => (forall a t1. (MonadTrans t1)
       => sus (CoroutineTT mark sus t1 m a)
       -> m (CoroutineTT mark sus t1 m a))
  -> CoroutineTT mark sus t m a
  -> t m (mark a)
runCoroutineTT p = runTT (PogoM p)

instance
  ( Functor sus, MonadIdentity mark, Commutant mark, Monad m, MonadTrans t
  , EqIn h (t m (mark a))
  ) => EqIn (PogoM mark sus m, h) (CoroutineTT mark sus t m a)
  where
    eqIn
      :: (PogoM mark sus m, h)
      -> CoroutineTT mark sus t m a
      -> CoroutineTT mark sus t m a
      -> Bool
    eqIn (pogo, h) x y =
      eqIn h (runTT pogo x) (runTT pogo y)





{- Specialized Lifts -}





{- Effect Classes -}

instance
  ( Functor sus, MonadIdentity mark, Monad m, MonadTrans t
  ) => MonadCoroutine mark sus (CoroutineTT mark sus t m)
  where
    suspend
      :: sus (CoroutineTT mark sus t m a)
      -> Thunk mark sus (CoroutineTT mark sus t m) a
    suspend = Thunk . CoroutineTT . return . Muse

    resume
      :: Thunk mark sus (CoroutineTT mark sus t m) a
      -> CoroutineTT mark sus t m (Muse sus (CoroutineTT mark sus t m) a)
    resume = liftT . unCoroutineTT . unThunk

instance
  ( Functor sus, MonadIdentity mark, MonadIdentity mark1, Monad m, MonadTrans t
  , forall x. (Monad x) => MonadState mark s (t x)
  ) => MonadState mark s (CoroutineTT mark1 sus t m)
  where
    get
      :: CoroutineTT mark1 sus t m (mark s)
    get = liftT get

    put
      :: mark s
      -> CoroutineTT mark1 sus t m ()
    put = liftT . put

{-
instance
  ( Functor sus, Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadHalt mark m
  ) => MonadHalt mark (CoroutineT mark1 sus m)

instance
  ( Functor sus, Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadPrompt mark p m
  ) => MonadPrompt mark p (CoroutineT mark1 sus m)

instance
  ( Functor sus, MonadIdentity mark, MonadIdentity mark1, Monad m
  , MonadExcept mark e m
  ) => MonadExcept mark e (CoroutineT mark1 sus m)
  where
    catch
      :: CoroutineT mark1 sus m a
      -> (mark e -> CoroutineT mark1 sus m a)
      -> CoroutineT mark1 sus m a
    catch x h = CoroutineT $
      catch (unCoroutineT x) (unCoroutineT . h)

instance
  ( Functor sus, MonadIdentity mark, MonadIdentity mark1, Monad m
  , MonadReadOnly mark r m
  ) => MonadReadOnly mark r (CoroutineT mark1 sus m)
  where
    local
      :: (mark r -> mark r)
      -> CoroutineT mark1 sus m a
      -> CoroutineT mark1 sus m a
    local f = CoroutineT . local f . unCoroutineT

instance
  ( Functor sus, Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadWriteOnly mark w m, Monoid w
  ) => MonadWriteOnly mark w (CoroutineT mark1 sus m)
  where
    draft
      :: CoroutineT mark1 sus m a
      -> CoroutineT mark1 sus m (Pair (mark w) a)
    draft =
      let
        f (Pair w x) = case x of
          Idea a -> Idea $ Pair w a
          Muse z -> Muse $ fmap (fmap (Pair w)) z
      in
        CoroutineT . fmap f . draft . unCoroutineT

instance
  ( Functor sus, Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadAppendOnly mark w m, Monoid w
  ) => MonadAppendOnly mark w (CoroutineT mark1 sus m)

-}
