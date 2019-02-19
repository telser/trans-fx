{-#
  LANGUAGE
    Rank2Types,
    InstanceSigs,
    KindSignatures,
    FlexibleInstances,
    QuantifiedConstraints,
    MultiParamTypeClasses
#-}

module Control.FX.Monad.Trans.Trans.PromptTT (
    PromptTT(..)
  , runPromptTT
  , Eval(..)
) where

import Data.Typeable (Typeable)

import Control.FX.Monad
import Control.FX.Monad.Trans
import Control.FX.Monad.Trans.Trans.Class
import Control.FX.Monad.Trans.Trans.IdentityTT
import Control.FX.Monad.Trans.Trans.ApplyTT

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

data Eval
  (p :: * -> *)
  (m :: * -> *)
    = Eval
        { unEval :: forall u. p u -> m u
        } deriving (Typeable)

instance
  ( Monad m, MonadTrans t
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
  ( Monad m, MonadTrans t
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
  ( Monad m, MonadTrans t
  ) => Functor (PromptTT mark p t m)
  where
    fmap
      :: (a -> b)
      -> PromptTT mark p t m a
      -> PromptTT mark p t m b
    fmap f x = x >>= (return . f)

instance
  ( MonadTrans t
  ) => MonadTrans (PromptTT mark p t)
  where
    lift
      :: ( Monad m )
      => m a
      -> PromptTT mark p t m a
    lift x = PromptTT $ \end _ ->
      lift x >>= end

instance
  MonadTransTrans (PromptTT mark p)
  where
    liftT
      :: ( Monad m, MonadTrans t )
      => t m a
      -> PromptTT mark p t m a
    liftT x = PromptTT $ \end cont ->
      x >>= end

instance
  RunMonadTransTrans (Eval p) (PromptTT mark p) Identity
  where
    runTT
      :: (Monad m, MonadTrans t)
      => Eval p m
      -> PromptTT mark p t m a
      -> t m (Identity a)
    runTT (Eval eval) x = fmap Identity $ runPromptTT eval x

runPromptTT
  :: ( Monad m, MonadTrans t )
  => (forall u. p u -> m u)
  -> PromptTT mark p t m a
  -> t m a
runPromptTT eval (PromptTT x) =
  x return (\p cont -> (lift $ eval p) >>= cont)





instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  ) => MonadPrompt mark p (PromptTT mark p t m)
  where
    prompt :: mark (p a) -> PromptTT mark p t m (mark a)
    prompt p = fmap return $ PromptTT $ \end cont ->
      cont (unwrap p) end
