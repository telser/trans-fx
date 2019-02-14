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
  , Eval(..)
  , runPromptTT
  , prompt
) where

import Data.Typeable (Typeable)

import Control.FX.Monad
import Control.FX.Monad.Trans
import Control.FX.Monad.Trans.Trans.Class
import Control.FX.Monad.Trans.Trans.IdentityTT
import Control.FX.Monad.Trans.Trans.ApplyTT

data PromptTT
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
  ) => Monad (PromptTT p t m)
  where
    return :: a -> PromptTT p t m a
    return x = PromptTT $ \end _ -> end x

    (PromptTT x) >>= f = PromptTT $ \end cont -> do
      let end' y = unPromptTT (f y) end cont
      x end' cont

instance
  ( Monad m, MonadTrans t
  ) => Applicative (PromptTT p t m)
  where
    pure :: a -> PromptTT p t m a
    pure = return

    f <*> x = do
      f' <- f
      x' <- x
      return (f' x')

instance
  ( Monad m, MonadTrans t
  ) => Functor (PromptTT p t m)
  where
    fmap :: (a -> b) -> PromptTT p t m a -> PromptTT p t m b
    fmap f x = x >>= (return . f)

instance
  ( MonadTrans t
  ) => MonadTrans (PromptTT p t)
  where
    lift x = PromptTT $ \end _ ->
      lift x >>= end

instance MonadTransTrans (PromptTT p) where
  liftT x = PromptTT $ \end cont ->
    x >>= end

instance RunMonadTransTrans (Eval p) (PromptTT p) Identity where
  runTT
    :: (Monad m, MonadTrans t)
    => Eval p m -> PromptTT p t m a -> t m (Identity a)
  runTT (Eval eval) x = fmap Identity $ runPromptTT eval x

runPromptTT
  :: ( Monad m, MonadTrans t )
  => (forall u. p u -> m u)
  -> PromptTT p t m a
  -> t m a
runPromptTT eval (PromptTT x) =
  x return (\p cont -> (lift $ eval p) >>= cont)



prompt :: p a -> PromptTT p t m a
prompt p = PromptTT $ \end cont ->
  cont p end
