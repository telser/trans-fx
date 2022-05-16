{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.Trans.Data.StackT (
    StackT(..)
  , runStackT
  , Context(..)
  , InputT(..)
  , OutputT(..)
) where



import Data.Typeable (Typeable, Proxy, typeOf)

import Control.FX
import Control.FX.Structure.Stack
import Control.FX.Monad.Data.Class



newtype StackT
  (mark :: * -> *)
  (f :: * -> *)
  (d :: *)
  (m :: * -> *)
  (a :: *)
    = StackT
        { unStackT :: f d -> m (Pair (f d) a)
        } deriving (Typeable)



instance
  ( Typeable f, Typeable d, Typeable m, Typeable a, Typeable mark
  ) => Show (StackT mark f d m a)
  where
    show
      :: StackT mark f d m a
      -> String
    show = show . typeOf

instance
  ( Monad m, MonadIdentity mark
  ) => Functor (StackT mark f d m)
  where
    fmap
      :: (a -> b)
      -> StackT mark f d m a
      -> StackT mark f d m b
    fmap f (StackT x) =
      StackT $ \s1 -> do
        Pair s2 a <- x s1
        return $ Pair s2 (f a)

instance
  ( Monad m, MonadIdentity mark
  ) => Applicative (StackT mark f d m)
  where
    pure
      :: a
      -> StackT mark f d m a
    pure x =
      StackT $ \s -> pure $ Pair s x

    (<*>)
      :: StackT mark f d m (a -> b)
      -> StackT mark f d m a
      -> StackT mark f d m b
    (StackT f) <*> (StackT x) =
      StackT $ \s1 -> do
        Pair s2 g <- f s1
        Pair s3 a <- x s2
        return $ Pair s3 (g a)

instance
  ( Monad m, MonadIdentity mark
  ) => Monad (StackT mark f d m)
  where
    return
      :: a
      -> StackT mark f d m a
    return x =
      StackT $ \s -> return $ Pair s x

    (>>=)
      :: StackT mark f d m a
      -> (a -> StackT mark f d m b)
      -> StackT mark f d m b
    (StackT x) >>= f =
      StackT $ \s1 -> do
        Pair s2 a <- x s1
        unStackT (f a) s2

instance
  ( MonadIdentity mark
  ) => MonadTrans (StackT mark f d)
  where
    lift
      :: ( Monad m )
      => m a
      -> StackT mark f d m a
    lift x = StackT $ \s -> fmap (\a -> Pair s a) x

instance
  ( MonadIdentity mark
  ) => MonadFunctor (StackT mark f d)
  where
    hoist
      :: ( Monad m, Monad n )
      => (forall u. m u -> n u)
      -> StackT mark f d m a
      -> StackT mark f d n a
    hoist f (StackT x) =
      StackT $ \s -> do
        a <- f $ fmap slot2 (x s)
        return $ Pair s a



instance
  ( MonadIdentity mark
  ) => ComposableT (StackT mark f d)
  where
    newtype (ComposeT (StackT mark f d) t2 m a) =
      ComposeT_StackT
        { unComposeT_StackT :: StackT mark f d (t2 m) a
        } deriving (Typeable)

    toComposeT = ComposeT_StackT
    unComposeT = unComposeT_StackT

instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t2, MonadIdentity mark, IsStack f
  ) => MonadStack mark f d (ComposeT (StackT mark f d) t2 m)
  where
    push
      :: Proxy f
      -> mark d
      -> ComposeT (StackT mark f d) t2 m ()
    push proxy = toComposeT . push proxy

    pop
      :: Proxy f
      -> ComposeT (StackT mark f d) t2 m (mark (Maybe d))
    pop proxy = toComposeT $ pop proxy

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t1, MonadTrans t2, MonadIdentity mark, ComposableT t1
  , forall x. (Monad x) => MonadStack mark f d (t2 x), IsStack f
  ) => MonadStack mark f d (ComposeT t1 t2 m)
  where
    push
      :: Proxy f
      -> mark d
      -> ComposeT t1 t2 m ()
    push proxy = toComposeT . lift . push proxy

    pop
      :: Proxy f
      -> ComposeT t1 t2 m (mark (Maybe d))
    pop proxy = toComposeT $ lift $ pop proxy



instance
  ( MonadIdentity mark
  ) => OverableT (StackT mark f d)
  where
    newtype (OverTT (StackT mark f d) u t m a) =
      OverTT_StackT
        { unOverTT_StackT :: StackT mark f d (u t m) a
        } deriving (Typeable)

    toOverTT = OverTT_StackT
    unOverTT = unOverTT_StackT

instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t, IsStack f
  , MonadTransTrans u, MonadIdentity mark
  ) => MonadStack mark f d (OverTT (StackT mark f d) u t m)
  where
    push
      :: Proxy f
      -> mark d
      -> OverTT (StackT mark f d) u t m ()
    push proxy = toOverTT . push proxy

    pop
      :: Proxy f
      -> OverTT (StackT mark f d) u t m (mark (Maybe d))
    pop proxy = toOverTT $ pop proxy

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadTrans v, OverableT v
  , MonadTransTrans u, MonadIdentity mark, IsStack f
  , forall x. (Monad x) => MonadStack mark f d (u t x)
  ) => MonadStack mark f d (OverTT v u t m)
  where
    push
      :: Proxy f
      -> mark d
      -> OverTT v u t m ()
    push proxy = toOverTT . lift . push proxy

    pop
      :: Proxy f
      -> OverTT v u t m (mark (Maybe d))
    pop proxy = toOverTT $ lift $ pop proxy



instance
  ( EqIn m, MonadIdentity mark, Eq (f d)
  ) => EqIn (StackT mark f d m)
  where
    newtype Context (StackT mark f d m)
      = StackTCtx
          { unStackTCtx :: (mark (f d), Context m)
          } deriving (Typeable)

    eqIn
      :: (Eq a)
      => Context (StackT mark f d m)
      -> StackT mark f d m a
      -> StackT mark f d m a
      -> Bool
    eqIn (StackTCtx (s,h)) (StackT x) (StackT y) =
      eqIn h (x $ unwrap s) (y $ unwrap s)

deriving instance
  ( Eq (mark (f d)), Eq (Context m)
  ) => Eq (Context (StackT mark f d m))

deriving instance
  ( Show (mark (f d)), Show (Context m)
  ) => Show (Context (StackT mark f d m))



instance
  ( MonadIdentity mark
  ) => RunMonadTrans (StackT mark f d)
  where
    newtype InputT (StackT mark f d)
      = StackTIn
          { unStackTIn :: mark (f d)
          } deriving (Typeable)

    newtype OutputT (StackT mark f d) a
      = StackTOut
          { unStackTOut :: Pair (mark (f d)) a
          } deriving (Typeable)

    runT
      :: ( Monad m )
      => InputT (StackT mark f d)
      -> StackT mark f d m a
      -> m (OutputT (StackT mark f d) a)
    runT (StackTIn s) (StackT x) = do
      Pair s1 a <- x (unwrap s)
      return $ StackTOut $ Pair (pure s1) a

runStackT
  :: ( Monad m, MonadIdentity mark )
  => mark (f d)
  -> StackT mark f d m a
  -> m (Pair (mark (f d)) a)
runStackT inp =
  fmap unStackTOut . runT (StackTIn inp)

deriving instance
  ( Eq (mark (f d))
  ) => Eq (InputT (StackT mark f d))

deriving instance
  ( Show (mark (f d))
  ) => Show (InputT (StackT mark f d))

deriving instance
  ( Eq (mark (f d)), Eq a
  ) => Eq (OutputT (StackT mark f d) a)

deriving instance
  ( Show (mark (f d)), Show a
  ) => Show (OutputT (StackT mark f d) a)





{- Specialized Lifts -}

instance
  ( MonadIdentity mark
  ) => LiftCatch (StackT mark f d)
  where
    liftCatch
      :: ( Monad m )
      => Catch e m (OutputT (StackT mark f d) a)
      -> Catch e (StackT mark f d m) a
    liftCatch catch x h = StackT $ \s ->
      fmap (bimap1 unwrap . unStackTOut) $ catch
        (fmap (StackTOut . bimap1 pure) $ unStackT x s)
        (\e -> fmap (StackTOut . bimap1 pure) $ unStackT (h e) s)

instance
  ( MonadIdentity mark
  ) => LiftDraft (StackT mark f d)
  where
    liftDraft
      :: ( Monad m )
      => Draft w m (OutputT (StackT mark f d) a)
      -> Draft w (StackT mark f d m) a
    liftDraft draft x =
      StackT $ \s -> do
        Pair w (StackTOut (Pair s a)) <- draft $ fmap (StackTOut . bimap1 pure) $ unStackT x s
        return $ Pair (unwrap s) (Pair w a)

instance
  ( MonadIdentity mark
  ) => LiftLocal (StackT mark f d)
  where
    liftLocal
      :: ( Monad m )
      => Local r m (OutputT (StackT mark f d) a)
      -> Local r (StackT mark f d m) a
    liftLocal local f x =
      StackT $ \s -> do
        StackTOut (Pair s1 a) <- local f $ fmap (StackTOut . bimap1 pure) $ unStackT x s
        return $ Pair (unwrap s1) a





{- Effect Classes -}

instance {-# OVERLAPPING #-}
  ( Monad m, MonadIdentity mark, IsStack f
  ) => MonadStack mark f d (StackT mark f d m)
  where
    push
      :: Proxy f
      -> mark d
      -> StackT mark f d m ()
    push _ d = StackT $ \s ->
      return (Pair (stackPush (unwrap d) s) ())

    pop
      :: Proxy f
      -> StackT mark f d m (mark (Maybe d))
    pop _ = StackT $ \s ->
      case stackPop s of
        Nothing ->
          return $ Pair s $ pure Nothing
        Just (a,as) ->
          return $ Pair as $ pure (Just a)

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadStack mark f d m, IsStack f
  ) => MonadStack mark f d (StackT mark1 f1 d1 m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadState mark s m
  ) => MonadState mark s (StackT mark1 f d1 m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadReadOnly mark r m
  ) => MonadReadOnly mark r (StackT mark1 f d m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadWriteOnly mark w m, Monoid w
  ) => MonadWriteOnly mark w (StackT mark1 f d m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadExcept mark e m
  ) => MonadExcept mark e (StackT mark1 f d m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadPrompt mark p m
  ) => MonadPrompt mark p (StackT mark1 f d m)

instance
  ( Monad m, MonadIdentity mark1, MonadIdentity mark
  , MonadHalt mark m
  ) => MonadHalt mark (StackT mark1 f d m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadAppendOnly mark w m, Monoid w
  ) => MonadAppendOnly mark w (StackT mark1 f d m)

instance
  ( Monad m, MonadIdentity mark, MonadIdentity mark1
  , MonadWriteOnce mark w m
  ) => MonadWriteOnce mark w (StackT mark1 f d m)
