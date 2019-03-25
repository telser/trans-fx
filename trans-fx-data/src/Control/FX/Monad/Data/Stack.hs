{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FX.Monad.Data.Stack (
    Stack(..)
  , runStack
  , Context(..)
  , Input(..)
  , Output(..)
) where



import Data.Typeable (Typeable, Proxy, typeOf)

import Control.FX
import Control.FX.Structure.Stack
import Control.FX.Monad.Data.Class



newtype Stack
  (mark :: * -> *)
  (f :: * -> *)
  (d :: *)
  (a :: *)
    = Stack
        { unStack :: f d -> Pair (f d) a
        } deriving (Typeable)

instance
  ( Typeable f, Typeable d, Typeable a, Typeable mark
  ) => Show (Stack mark f d a)
  where
    show
      :: Stack mark f d a
      -> String
    show = show . typeOf

instance
  ( MonadIdentity mark
  ) => Functor (Stack mark f d)
  where
    fmap
      :: (a -> b)
      -> Stack mark f d a
      -> Stack mark f d b
    fmap f (Stack x) = Stack $ \s1 ->
      let Pair s2 a = x s1 in
      Pair s2 (f a)

instance
  ( MonadIdentity mark
  ) => Applicative (Stack mark f d)
  where
    pure
      :: a
      -> Stack mark f d a
    pure a = Stack $ \s -> Pair s a

    (<*>)
      :: Stack mark f d (a -> b)
      -> Stack mark f d a
      -> Stack mark f d b
    (Stack f') <*> (Stack x') = Stack $ \s1 ->
      let Pair s2 f = f' s1 in
      let Pair s3 x = x' s2 in
      Pair s3 (f x)

instance
  ( MonadIdentity mark
  ) => Monad (Stack mark f d)
  where
    return
      :: a
      -> Stack mark f d a
    return a = Stack $ \s -> Pair s a

    (>>=)
      :: Stack mark f d a
      -> (a -> Stack mark f d b)
      -> Stack mark f d b
    (Stack x') >>= f = Stack $ \s1 ->
      let Pair s2 x = x' s1 in
      (unStack . f) x s2



instance
  ( Eq (f d), MonadIdentity mark
  ) => EqIn (Stack mark f d)
  where
    data Context (Stack mark f d)
      = StackCtx
          { unStackCtx :: mark (f d)
          } deriving (Typeable)

    eqIn
      :: (Eq a)
      => Context (Stack mark f d)
      -> Stack mark f d a
      -> Stack mark f d a
      -> Bool
    eqIn (StackCtx s) (Stack x) (Stack y) =
      (x $ unwrap s) == (y $ unwrap s)

deriving instance
  ( Eq (mark (f d))
  ) => Eq (Context (Stack mark f d))

deriving instance
  ( Show (mark (f d))
  ) => Show (Context (Stack mark f d))



instance
  ( MonadIdentity mark
  ) => RunMonad (Stack mark f d)
  where
    data Input (Stack mark f d)
      = StackIn
          { unStackIn :: mark (f d)
          } deriving (Typeable)

    data Output (Stack mark f d) a
      = StackOut
          { unStackOut :: Pair (mark (f d)) a
          } deriving (Typeable)

    run
      :: Input (Stack mark f d)
      -> Stack mark f d a
      -> Output (Stack mark f d) a
    run (StackIn s) (Stack x) =
      let Pair s1 a = x (unwrap s)
      in StackOut $ Pair (return s1) a

deriving instance
  ( Eq (mark (f d))
  ) => Eq (Input (Stack mark f d))

deriving instance
  ( Show (mark (f d))
  ) => Show (Input (Stack mark f d))

deriving instance
  ( Eq (mark (f d)), Eq a
  ) => Eq (Output (Stack mark f d) a)

deriving instance
  ( Show (mark (f d)), Show a
  ) => Show (Output (Stack mark f d) a)

runStack
  :: ( MonadIdentity mark, IsStack f )
  => f d
  -> Stack mark f d a
  -> Pair (mark (f d)) a
runStack s x = unStackOut $ run (StackIn $ pure s) x



{- Effect Class -}

instance
  ( MonadIdentity mark, IsStack f
  ) => MonadStack mark f d (Stack mark f d)
  where
    push
      :: Proxy f
      -> mark d
      -> Stack mark f d ()
    push _ x = Stack $ \s ->
      Pair (stackPush (unwrap x) s) ()

    pop
      :: Proxy f
      -> Stack mark f d (mark (Maybe d))
    pop _ = Stack $ \s ->
      case stackPop s of
        Nothing ->
          Pair s (pure Nothing)
        Just (a,as) ->
          Pair as (pure $ Just a)
