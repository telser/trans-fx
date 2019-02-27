-- Following Mario Blažević's Coroutine monad from _Coroutine Pipelines_ in The Monad.Reader Issue 19. https://themonadreader.files.wordpress.com/2011/10/issue19.pdf

{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}



module Control.FX.Monad.Trans.CoroutineT (
    CoroutineT(..)
  , runCoroutineT
  , Pogo(..)
) where



import Control.Monad (liftM)
import Data.Typeable (Typeable, typeOf)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans.Class



newtype CoroutineT
  (mark :: * -> *)
  (sus :: * -> *)
  (m :: * -> *)
  (a :: *)
    = CoroutineT
        { unCoroutineT :: m (Muse sus (CoroutineT mark sus m) a)
        } deriving (Typeable)



instance
  ( Typeable mark, Typeable sus, Typeable m, Typeable a
  ) => Show (CoroutineT mark sus m a)
  where
    show
      :: CoroutineT mark sus m a
      -> String
    show = show . typeOf

instance
  ( Functor sus, Monad m, MonadIdentity mark
  ) => Functor (CoroutineT mark sus m)
  where
    fmap
      :: (a -> b)
      -> CoroutineT mark sus m a
      -> CoroutineT mark sus m b
    fmap f x = x >>= (return . f)

instance
  ( Functor sus, Monad m, MonadIdentity mark
  ) => Applicative (CoroutineT mark sus m)
  where
    pure
      :: a
      -> CoroutineT mark sus m a
    pure = CoroutineT . return . Idea

    (<*>)
      :: CoroutineT mark sus m (a -> b)
      -> CoroutineT mark sus m a
      -> CoroutineT mark sus m b
    f' <*> x' = do
      f <- f'
      x <- x'
      return (f x)

instance
  ( Functor sus, Monad m, MonadIdentity mark
  ) => Monad (CoroutineT mark sus m)
  where
    return
      :: a
      -> CoroutineT mark sus m a
    return = CoroutineT . return . Idea

    (>>=)
      :: CoroutineT mark sus m a
      -> (a -> CoroutineT mark sus m b)
      -> CoroutineT mark sus m b
    (CoroutineT x) >>= f =
      let
        f' z = case z of
          Idea a -> unCoroutineT (f a)
          Muse s -> return $ Muse (fmap (>>= f) s)
      in
        CoroutineT (x >>= f')

instance
  ( Functor sus, MonadIdentity mark
  ) => MonadTrans (CoroutineT mark sus)
  where
    lift
      :: ( Monad m )
      => m a
      -> CoroutineT mark sus m a
    lift = CoroutineT . liftM Idea

instance
  ( Functor sus, MonadIdentity mark
  ) => MonadFunctor (CoroutineT mark sus)
  where
    hoist
      :: ( Monad m, Monad n )
      => (forall u. m u -> n u)
      -> CoroutineT mark sus m a
      -> CoroutineT mark sus n a
    hoist f (CoroutineT x) = CoroutineT $
      f $ do
        y <- x
        return $ case y of
          Idea a -> Idea a
          Muse z -> Muse $ fmap (hoist f) z

newtype Pogo
  (mark :: * -> *)
  (sus :: * -> *)
    = Pogo
        { unPogo
            :: forall a m
             . ( Monad m )
            => sus (CoroutineT mark sus m a)
            -> CoroutineT mark sus m a
        } deriving (Typeable)

instance
  ( Typeable mark, Typeable sus
  ) => Show (Pogo mark sus)
  where
    show
      :: Pogo mark sus
      -> String
    show = show . typeOf



instance
  ( Functor sus, MonadIdentity mark, Commutant mark
  ) => RunMonadTrans
    (Pogo mark sus) (CoroutineT mark sus) mark
  where
    runT
      :: ( Monad m )
      => Pogo mark sus
      -> CoroutineT mark sus m a
      -> m (mark a)
    runT (Pogo k) = loop
      where
        loop (CoroutineT c) =
          c >>= \x -> case x of
            Idea a -> return (pure a)
            Muse z -> loop $ k z

runCoroutineT
  :: ( Functor sus, MonadIdentity mark, Commutant mark, Monad m )
  => (forall a m1. (Monad m1)
       => sus (CoroutineT mark sus m1 a)
       -> CoroutineT mark sus m1 a)
  -> CoroutineT mark sus m a
  -> m (mark a)
runCoroutineT p = runT (Pogo p)

instance
  ( Functor sus, MonadIdentity mark, Commutant mark, Monad m
  , EqIn h (m (mark a))
  ) => EqIn (Pogo mark sus, h) (CoroutineT mark sus m a)
  where
    eqIn
      :: (Pogo mark sus, h)
      -> CoroutineT mark sus m a
      -> CoroutineT mark sus m a
      -> Bool
    eqIn (pogo, h) x y =
      eqIn h (runT pogo x) (runT pogo y)





{- Effect Classes -}

instance {-# OVERLAPPING #-}
  ( Functor sus, MonadIdentity mark, Monad m
  ) => MonadCoroutine mark sus m (CoroutineT mark sus m)
  where
    suspend
      :: mark (sus (CoroutineT mark sus m a))
      -> mark (CoroutineT mark sus m a)
    suspend = pure . CoroutineT . return . Muse . unwrap

    resume
      :: mark (CoroutineT mark sus m a)
      -> m (Muse sus (CoroutineT mark sus m) a)
    resume = unCoroutineT . unwrap

instance {-# OVERLAPPABLE #-}
  ( Functor sus, MonadIdentity mark, Functor sus1, MonadIdentity mark1
  , MonadCoroutine mark sus n m, Applicative sus
  ) => MonadCoroutine mark sus n (CoroutineT mark1 sus1 m)
  where
    suspend
      :: mark (sus (CoroutineT mark1 sus1 m a))
      -> mark (CoroutineT mark1 sus1 m a)
    suspend = fmap CoroutineT . suspend . pure . fmap unCoroutineT . unwrap

    resume
      :: mark (CoroutineT mark1 sus1 m a)
      -> n (Muse sus (CoroutineT mark1 sus1 m) a)
    resume =
      let
        f
          :: Muse sus m (Muse sus1 (CoroutineT mark1 sus1 m) a)
          -> Muse sus (CoroutineT mark1 sus1 m) a
        f z = case z of
          Idea v -> case v of
            Idea a -> Idea a
            Muse b ->
              let
                g :: a -> mark1 a
                g = pure
              in
                Muse $ pure $ CoroutineT $ resume $ suspend $ g b
          Muse u -> Muse $ fmap CoroutineT u
      in
        fmap f . resume . fmap unCoroutineT

instance
  ( Functor sus, MonadIdentity mark, MonadIdentity mark1, Monad m
  , MonadState mark s m
  ) => MonadState mark s (CoroutineT mark1 sus m)

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
