-- | Module      : Control.FX.Monad.Trans.Trans.PromptTT
--   Description : Concrete prompt monad transformer transformer
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.Trans.Trans.PromptTT (
    PromptTT(..)
  , Eval(..)
) where



import Data.Typeable (Typeable, typeOf)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans
import Control.FX.Monad.Trans.Trans.Class
import Control.FX.Monad.Trans.Trans.IdentityTT
import Control.FX.Monad.Trans.Trans.ApplyTT



-- | Concrete prompt monad transformer transformer
data PromptTT
  (mark :: * -> *)
  (p :: * -> *)
  (t :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
    = PromptTT
        { unPromptTT
            :: forall v. (a -> t m v)
                -> (forall u. p u -> (u -> t m v) -> t m v)
                -> t m v
        } deriving (Typeable)

instance
  ( Typeable p, Typeable t, Typeable m, Typeable a, Typeable mark
  ) => Show (PromptTT mark p t m a)
  where
    show
      :: PromptTT mark p t m a
      -> String
    show = show . typeOf

instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  ) => Monad (PromptTT mark p t m)
  where
    return
      :: a -> PromptTT mark p t m a
    return x = PromptTT $ \end _ -> end x

    (>>=)
      :: PromptTT mark p t m a
      -> (a -> PromptTT mark p t m b)
      -> PromptTT mark p t m b
    (PromptTT x) >>= f = PromptTT $ \end cont -> do
      let end' y = unPromptTT (f y) end cont
      x end' cont

instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  ) => Applicative (PromptTT mark p t m)
  where
    pure
      :: a -> PromptTT mark p t m a
    pure = return

    (<*>)
      :: PromptTT mark p t m (a -> b)
      -> PromptTT mark p t m a
      -> PromptTT mark p t m b
    f <*> x = do
      f' <- f
      x' <- x
      return (f' x')

instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  ) => Functor (PromptTT mark p t m)
  where
    fmap
      :: (a -> b)
      -> PromptTT mark p t m a
      -> PromptTT mark p t m b
    fmap f x = x >>= (return . f)

instance
  ( MonadTrans t, MonadIdentity mark
  ) => MonadTrans (PromptTT mark p t)
  where
    lift
      :: ( Monad m )
      => m a
      -> PromptTT mark p t m a
    lift x = PromptTT $ \end _ ->
      lift x >>= end

instance
  ( MonadIdentity mark
  ) => MonadTransTrans (PromptTT mark p)
  where
    liftT
      :: ( Monad m, MonadTrans t )
      => t m a
      -> PromptTT mark p t m a
    liftT x = PromptTT $ \end cont ->
      x >>= end

instance
  ( MonadIdentity mark, Commutant mark
  ) => RunMonadTransTrans (Eval p) (PromptTT mark p) mark
  where
    runTT
      :: (Monad m, MonadTrans t)
      => Eval p m
      -> PromptTT mark p t m a
      -> t m (mark a)
    runTT (Eval eval) (PromptTT x) = fmap pure $
      x return (\p cont -> (lift $ eval p) >>= cont)

-- | Helper type for running prompt computations
data Eval
  (p :: * -> *)
  (m :: * -> *)
    = Eval
        { unEval :: forall u. p u -> m u
        } deriving (Typeable)

instance
  ( Typeable p, Typeable m
  ) => Show (Eval p m)
  where
    show
      :: Eval p m
      -> String
    show = show . typeOf

instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , Commutant mark, EqIn h (t m (mark a))
  ) => EqIn (Eval p m, h) (PromptTT mark p t m a)
  where
    eqIn
      :: (Eval p m, h)
      -> PromptTT mark p t m a
      -> PromptTT mark p t m a
      -> Bool
    eqIn (eval, h) x y =
      eqIn h (runTT eval x) (runTT eval y)



{- Specialized Lifts -}

instance
  ( MonadIdentity mark, Commutant mark
  ) => LiftCatchT (Eval p) (PromptTT mark p) mark
  where
    liftCatchT
      :: ( Monad m, MonadTrans t )
      => (forall x. Catch e (t m) (mark x))
      -> (forall x. Catch e (PromptTT mark p t m) x)

    liftCatchT catch x h = PromptTT $ \end cont ->
      fmap unwrap $ catch
        (fmap pure $ unPromptTT x end cont)
        (\e -> fmap pure $ unPromptTT (h e) end cont)

instance
  ( MonadIdentity mark, Commutant mark
  ) => LiftDraftT (Eval p) (PromptTT mark p) mark
  where
    liftDraftT
      :: ( Monad m, MonadTrans t, Monoid w )
      => (forall x. Draft w (t m) (mark x))
      -> (forall x. Draft w (PromptTT mark p t m) x)
    liftDraftT draft x = PromptTT $ \end cont ->
      fmap (unwrap . slot2) $
        draft (fmap pure $ unPromptTT x (end . pure) cont)

instance
  ( MonadIdentity mark, Commutant mark
  ) => LiftLocalT (Eval p) (PromptTT mark p) mark
  where
    liftLocalT
      :: ( Monad m, MonadTrans t )
      => (forall x. Local r (t m) (mark x))
      -> (forall x. Local r (PromptTT mark p t m) x)
    liftLocalT local f x = PromptTT $ \end cont ->
      fmap unwrap $ local f $ fmap pure $ unPromptTT x end cont



{- Effect Class -}

instance {-# OVERLAPS #-}
  ( Monad m, MonadTrans t, MonadIdentity mark
  ) => MonadPrompt mark p (PromptTT mark p t m)
  where
    prompt
      :: mark (p a)
      -> PromptTT mark p t m (mark a)
    prompt p = fmap return $ PromptTT $ \end cont ->
      cont (unwrap p) end

instance {-# OVERLAPPABLE #-}
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadIdentity mark1, Commutant mark1
  , forall x. (Monad x) => MonadPrompt mark p (t x)
  ) => MonadPrompt mark p (PromptTT mark1 p1 t m)
  where
    prompt
      :: mark (p a)
      -> PromptTT mark1 p1 t m (mark a)
    prompt = liftT . prompt

instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadIdentity mark1
  , forall x. (Monad x) => MonadState mark s (t x)
  ) => MonadState mark s (PromptTT mark1 p t m)
  where
    get
      :: PromptTT mark1 p t m (mark s)
    get = liftT get

    put
      :: mark s
      -> PromptTT mark1 p t m ()
    put = liftT . put

instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadIdentity mark1, Commutant mark1, Monoid w
  , forall x. (Monad x) => MonadWriteOnly mark w (t x)
  ) => MonadWriteOnly mark w (PromptTT mark1 p t m)
  where
    tell
      :: mark w
      -> PromptTT mark1 p t m ()
    tell = liftT . tell

    draft
      :: PromptTT mark1 p t m a
      -> PromptTT mark1 p t m (Pair (mark w) a)
    draft = liftDraftT draft

instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadIdentity mark1, Commutant mark1, Monoid w
  , forall x. (Monad x) => MonadAppendOnly mark w (t x)
  ) => MonadAppendOnly mark w (PromptTT mark1 p t m)
  where
    jot
      :: mark w
      -> PromptTT mark1 p t m ()
    jot = liftT . jot

    look
      :: PromptTT mark1 p t m (mark w)
    look = liftT look

instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadIdentity mark1, Commutant mark1
  , forall x. (Monad x) => MonadReadOnly mark r (t x)
  ) => MonadReadOnly mark r (PromptTT mark1 p t m)
  where
    ask
      :: PromptTT mark1 p t m (mark r)
    ask = liftT ask

    local
      :: (mark r -> mark r)
      -> PromptTT mark1 p t m a
      -> PromptTT mark1 p t m a
    local = liftLocalT local

instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , MonadIdentity mark1, Commutant mark1
  , forall x. (Monad x) => MonadExcept mark e (t x)
  ) => MonadExcept mark e (PromptTT mark1 p t m)
  where
    throw
      :: mark e
      -> PromptTT mark1 p t m a
    throw = liftT . throw

    catch
      :: PromptTT mark1 p t m a
      -> (mark e -> PromptTT mark1 p t m a)
      -> PromptTT mark1 p t m a
    catch = liftCatchT catch
