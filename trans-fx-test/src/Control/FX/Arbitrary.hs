{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Control.FX.Arbitrary () where

import Test.QuickCheck (Arbitrary(..), CoArbitrary(..), Gen)

import Control.FX.EqIn
import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans
import Control.FX.Monad.Trans.Trans



{-----------}
{- Functor -}
{-----------}

{- Pair -}

instance
  ( Arbitrary a, Arbitrary b
  ) => Arbitrary (Pair a b)
  where
    arbitrary = Pair <$> arbitrary <*> arbitrary

instance
  Arbitrary (Context (Pair a))
  where
    arbitrary = PairCtx <$> arbitrary



{- LeftZero -}

instance
  ( Arbitrary a
  ) => Arbitrary (LeftZero a)
  where
    arbitrary = do
      p <- arbitrary
      if p
        then return LeftUnit
        else LeftZero <$> arbitrary

instance
  Arbitrary (Context LeftZero)
  where
    arbitrary = LeftZeroCtx <$> arbitrary



{- RightZero -}

instance
  ( Arbitrary a
  ) => Arbitrary (RightZero a)
  where
    arbitrary = do
      p <- arbitrary
      if p
        then return RightUnit
        else RightZero <$> arbitrary

instance
  Arbitrary (Context RightZero)
  where
    arbitrary = RightZeroCtx <$> arbitrary





{---------}
{- Monad -}
{---------}

{- Identity -}

instance
  ( Arbitrary a
  ) => Arbitrary (Identity a)
  where
    arbitrary = Identity <$> arbitrary

instance
  ( CoArbitrary a
  ) => CoArbitrary (Identity a)
  where
    coarbitrary (Identity x) gen =
      coarbitrary x gen

instance
  Arbitrary (Context Identity)
  where
    arbitrary = return $ IdentityCtx ()

instance
  Arbitrary (Input Identity)
  where
    arbitrary = return $ IdentityIn ()



{- Halt -}

instance
  ( Arbitrary a
  ) => Arbitrary (Halt mark a)
  where
    arbitrary = do
      p <- arbitrary
      if p
        then return Halt
        else Step <$> arbitrary

instance
  ( Arbitrary (mark ())
  ) => Arbitrary (Context (Halt mark))
  where
    arbitrary = HaltCtx <$> arbitrary

instance
  ( Arbitrary (mark ())
  ) => Arbitrary (Input (Halt mark))
  where
    arbitrary = HaltIn <$> arbitrary



{- Except -}

instance
  ( Arbitrary a, Arbitrary e
  ) => Arbitrary (Except mark e a)
  where
    arbitrary = do
      q <- arbitrary
      if q
        then Accept <$> arbitrary
        else Except <$> arbitrary

instance
  ( Arbitrary (mark ())
  ) => Arbitrary (Context (Except mark e))
  where
    arbitrary = ExceptCtx <$> arbitrary

instance
  ( Arbitrary (mark ())
  ) => Arbitrary (Input (Except mark e))
  where
    arbitrary = ExceptIn <$> arbitrary



{- WriteOnly -}

instance
  ( Arbitrary a, Arbitrary w, Monoid w
  ) => Arbitrary (WriteOnly mark w a)
  where
    arbitrary = WriteOnly <$> arbitrary

instance
  ( Arbitrary (mark ())
  ) => Arbitrary (Context (WriteOnly mark w))
  where
    arbitrary = WriteOnlyCtx <$> arbitrary

instance
  ( Arbitrary (mark ())
  ) => Arbitrary (Input (WriteOnly mark w))
  where
    arbitrary = WriteOnlyIn <$> arbitrary



{- AppendOnly -}

instance
  ( Arbitrary a, Arbitrary w, Monoid w
  ) => Arbitrary (AppendOnly mark w a)
  where
    arbitrary = do
      w1 <- arbitrary
      a <- arbitrary
      return $ AppendOnly $ \w -> Pair (w <> w1) a

instance
  ( Arbitrary (mark ())
  ) => Arbitrary (Context (AppendOnly mark w))
  where
    arbitrary = AppendOnlyCtx <$> arbitrary

instance
  ( Arbitrary (mark ())
  ) => Arbitrary (Input (AppendOnly mark w))
  where
    arbitrary = AppendOnlyIn <$> arbitrary



{- ReadOnly -}

instance
  ( Arbitrary a, CoArbitrary r
  ) => Arbitrary (ReadOnly mark r a)
  where
    arbitrary = ReadOnly <$> arbitrary

instance
  ( Arbitrary (mark r)
  ) => Arbitrary (Context (ReadOnly mark r))
  where
    arbitrary = ReadOnlyCtx <$> arbitrary

instance
  ( Arbitrary (mark r)
  ) => Arbitrary (Input (ReadOnly mark r))
  where
    arbitrary = ReadOnlyIn <$> arbitrary



{- State -}

instance
  ( Arbitrary a, Arbitrary s, CoArbitrary s
  ) => Arbitrary (State mark s a)
  where
    arbitrary = State <$> arbitrary

instance
  ( Arbitrary (mark s)
  ) => Arbitrary (Context (State mark s))
  where
    arbitrary = StateCtx <$> arbitrary

instance
  ( Arbitrary (mark s)
  ) => Arbitrary (Input (State mark s))
  where
    arbitrary = StateIn <$> arbitrary





{--------------}
{- MonadTrans -}
{--------------}

{- IdentityT -}

instance
  ( Arbitrary (m a), Functor m
  ) => Arbitrary (IdentityT m a)
  where
    arbitrary = IdentityT <$> arbitrary

instance
  ( Arbitrary (Context m)
  ) => Arbitrary (Context (IdentityT m))
  where
    arbitrary = IdentityTCtx <$> arbitrary

instance
  Arbitrary (InputT IdentityT)
  where
    arbitrary = IdentityTIn <$> arbitrary



{- HaltT -}

instance
  ( Arbitrary (m a), Functor m, MonadIdentity mark
  ) => Arbitrary (HaltT mark m a)
  where
    arbitrary = do
      x <- arbitrary
      p <- arbitrary
      return $ HaltT $
        fmap (if p then Step else const Halt) x

instance
  ( Arbitrary (mark ()), Arbitrary (Context m)
  ) => Arbitrary (Context (HaltT mark m))
  where
    arbitrary = HaltTCtx <$> arbitrary

instance
  ( Arbitrary (mark ())
  ) => Arbitrary (InputT (HaltT mark))
  where
    arbitrary = HaltTIn <$> arbitrary



{- ExceptT -}

instance
  ( Arbitrary (m a), Arbitrary e, Functor m
  ) => Arbitrary (ExceptT mark e m a)
  where
    arbitrary = do
      x <- arbitrary
      p <- arbitrary
      if p
        then return $ ExceptT $ fmap Accept x
        else do
          e <- arbitrary
          return $ ExceptT $ fmap (const (Except e)) x

instance
  ( Arbitrary (mark ()), Arbitrary (Context m)
  ) => Arbitrary (Context (ExceptT mark e m))
  where
    arbitrary = ExceptTCtx <$> arbitrary

instance
  ( Arbitrary (mark ())
  ) => Arbitrary (InputT (ExceptT mark m))
  where
    arbitrary = ExceptTIn <$> arbitrary



{- WriteOnlyT -}

instance
  ( Arbitrary (m (WriteOnly mark w a)), Functor m
  ) => Arbitrary (WriteOnlyT mark w m a)
  where
    arbitrary = WriteOnlyT <$> arbitrary

instance
  ( Arbitrary (mark ()), Arbitrary (Context m)
  ) => Arbitrary (Context (WriteOnlyT mark w m))
  where
    arbitrary = WriteOnlyTCtx <$> arbitrary

instance
  ( Arbitrary (mark ())
  ) => Arbitrary (InputT (WriteOnlyT mark w))
  where
    arbitrary = WriteOnlyTIn <$> arbitrary



{- AppendOnlyT -}

instance
  ( Arbitrary (m (Pair w a)), Arbitrary w, Monoid w, CoArbitrary w
  ) => Arbitrary (AppendOnlyT mark w m a)
  where
    arbitrary = AppendOnlyT <$> arbitrary

instance
  ( Arbitrary (mark w), Arbitrary (Context m)
  ) => Arbitrary (Context (AppendOnlyT mark w m))
  where
    arbitrary = AppendOnlyTCtx <$> arbitrary

instance
  ( Arbitrary (mark ())
  ) => Arbitrary (InputT (AppendOnlyT mark w))
  where
    arbitrary = AppendOnlyTIn <$> arbitrary



{- ReadOnlyT -}

instance
  ( Arbitrary (m a), CoArbitrary r, Functor m
  ) => Arbitrary (ReadOnlyT mark r m a)
  where
    arbitrary = do
      f <- arbitrary
      return (ReadOnlyT $ ReadOnly f)

instance
  ( Arbitrary (mark r), Arbitrary (Context m)
  ) => Arbitrary (Context (ReadOnlyT mark r m))
  where
    arbitrary = ReadOnlyTCtx <$> arbitrary

instance
  ( Arbitrary (mark r)
  ) => Arbitrary (InputT (ReadOnlyT mark r))
  where
    arbitrary = ReadOnlyTIn <$> arbitrary



{- StateT -}

instance
  ( Arbitrary (m (Pair s a)), CoArbitrary s
  ) => Arbitrary (StateT mark s m a)
  where
    arbitrary = StateT <$> arbitrary

instance
  ( Arbitrary (mark s), Arbitrary (Context m)
  ) => Arbitrary (Context (StateT mark s m))
  where
    arbitrary = StateTCtx <$> arbitrary

instance
  ( Arbitrary (mark s)
  ) => Arbitrary (InputT (StateT mark s))
  where
    arbitrary = StateTIn <$> arbitrary



{- ApplyT -}

instance
  ( Arbitrary (t m a)
  ) => Arbitrary (ApplyT t m a)
  where
    arbitrary = ApplyT <$> arbitrary

instance
  ( Arbitrary (InputT t)
  ) => Arbitrary (InputT (ApplyT t))
  where
    arbitrary = ApplyTIn <$> arbitrary



{- ComposeT -}

instance
  ( Arbitrary (t1 (t2 m) a)
  ) => Arbitrary (ComposeT t1 t2 m a)
  where
    arbitrary = ComposeT <$> arbitrary

instance
  ( Arbitrary (Context (t1 (t2 m)))
  ) => Arbitrary (Context (ComposeT t1 t2 m))
  where
    arbitrary = ComposeTCtx <$> arbitrary

instance
  ( Arbitrary (InputT t1), Arbitrary (InputT t2)
  ) => Arbitrary (InputT (ComposeT t1 t2))
  where
    arbitrary = ComposeTIn <$> arbitrary





{-------------------}
{- MonadTransTrans -}
{-------------------}

{- IdentityTT -}

instance
  ( Arbitrary (t m a)
  ) => Arbitrary (IdentityTT t m a)
  where
    arbitrary = IdentityTT <$> arbitrary

instance
  ( Arbitrary (Context (t m))
  ) => Arbitrary (Context (IdentityTT t m))
  where
    arbitrary = IdentityTTCtx <$> arbitrary

instance
  Arbitrary (InputTT IdentityTT m)
  where
    arbitrary = IdentityTTIn <$> arbitrary



{- HaltTT -}

instance
  ( Arbitrary (t m a), Functor (t m), MonadIdentity mark
  ) => Arbitrary (HaltTT mark t m a)
  where
    arbitrary = HaltTT <$> arbitrary

instance
  ( Arbitrary (mark ()), Arbitrary (Context (t m))
  ) => Arbitrary (Context (HaltTT mark t m))
  where
    arbitrary = HaltTTCtx <$> arbitrary

instance
  ( Arbitrary (mark ())
  ) => Arbitrary (InputTT (HaltTT mark) m)
  where
    arbitrary = HaltTTIn <$> arbitrary



{- ExceptTT -}

instance
  ( Arbitrary (t m a), Arbitrary e, Functor (t m)
  ) => Arbitrary (ExceptTT mark e t m a)
  where
    arbitrary = ExceptTT <$> arbitrary

instance
  ( Arbitrary (mark ()), Arbitrary (Context (t m))
  ) => Arbitrary (Context (ExceptTT mark e t m))
  where
    arbitrary = ExceptTTCtx <$> arbitrary

instance
  ( Arbitrary (mark ())
  ) => Arbitrary (InputTT (ExceptTT mark e) m)
  where
    arbitrary = ExceptTTIn <$> arbitrary



{- WriteOnlyTT -}

instance
  ( Arbitrary (t m (WriteOnly mark w a)), Functor (t m)
  ) => Arbitrary (WriteOnlyTT mark w t m a)
  where
    arbitrary = WriteOnlyTT <$> arbitrary

instance
  ( Arbitrary (mark ()), Arbitrary (Context (t m))
  ) => Arbitrary (Context (WriteOnlyTT mark w t m))
  where
    arbitrary = WriteOnlyTTCtx <$> arbitrary

instance
  ( Arbitrary (mark ())
  ) => Arbitrary (InputTT (WriteOnlyTT mark w) m)
  where
    arbitrary = WriteOnlyTTIn <$> arbitrary



{- AppendOnlyTT -}

instance
  ( Arbitrary (t m (Pair w a)), Arbitrary w, Monoid w, CoArbitrary w
  ) => Arbitrary (AppendOnlyTT mark w t m a)
  where
    arbitrary = AppendOnlyTT <$> arbitrary

instance
  ( Arbitrary (mark ()), Arbitrary (Context (t m))
  ) => Arbitrary (Context (AppendOnlyTT mark w t m))
  where
    arbitrary = AppendOnlyTTCtx <$> arbitrary

instance
  ( Arbitrary (mark ())
  ) => Arbitrary (InputTT (AppendOnlyTT mark w) m)
  where
    arbitrary = AppendOnlyTTIn <$> arbitrary



{- ReadOnlyTT -}

instance
  ( Arbitrary (t m a), CoArbitrary r, Functor m
  ) => Arbitrary (ReadOnlyTT mark r t m a)
  where
    arbitrary = do
      f <- arbitrary
      return (ReadOnlyTT $ ReadOnlyT f)

instance
  ( Arbitrary (mark r), Arbitrary (Context (t m))
  ) => Arbitrary (Context (ReadOnlyTT mark r t m))
  where
    arbitrary = ReadOnlyTTCtx <$> arbitrary

instance
  ( Arbitrary (mark r)
  ) => Arbitrary (InputTT (ReadOnlyTT mark r) m)
  where
    arbitrary = ReadOnlyTTIn <$> arbitrary



{- StateTT -}

instance
  ( Arbitrary (t m (Pair s a)), CoArbitrary s
  ) => Arbitrary (StateTT mark s t m a)
  where
    arbitrary = StateTT <$> arbitrary

instance
  ( Arbitrary (mark s), Arbitrary (Context (t m))
  ) => Arbitrary (Context (StateTT mark s t m))
  where
    arbitrary = StateTTCtx <$> arbitrary

instance
  ( Arbitrary (mark s)
  ) => Arbitrary (InputTT (StateTT mark s) m)
  where
    arbitrary = StateTTIn <$> arbitrary



{- PromptTT -}

instance
  ( Monad m, MonadTrans t, MonadIdentity mark
  , Arbitrary (mark (p a)), Arbitrary (t m a)
  ) => Arbitrary (PromptTT mark p t m a)
  where
    arbitrary = do
      w <- arbitrary
      if w
        then do
          p <- arbitrary :: Gen (mark (p a))
          return $ fmap unwrap $ prompt p
        else liftT <$> arbitrary

instance
  ( Arbitrary (Context (t m)), Monad m
  ) => Arbitrary (Context (PromptTT mark Identity t m))
  where
    arbitrary = do
      h <- arbitrary
      return $ PromptTTCtx (Eval $ return . unIdentity, h)



{- OverTT -}

instance
  ( Monad m, MonadTrans t, MonadTrans w
  , MonadTransTrans u, Arbitrary (w (u t m) a)
  ) => Arbitrary (OverTT u w t m a)
  where
    arbitrary = OverTT <$> arbitrary

instance
  ( Arbitrary (Context (t m)), Arbitrary (InputTT u m), Arbitrary (InputT v)
  ) => Arbitrary (Context (OverTT u v t m))
  where
    arbitrary = OverTTCtx <$> arbitrary

instance
  ( Arbitrary (InputTT u m), Arbitrary (InputT v)
  ) => Arbitrary (InputTT (OverTT u v) m)
  where
    arbitrary = OverTTIn <$> arbitrary





















instance
  ( Monad m
  ) => Arbitrary (Eval Identity m)
  where
    arbitrary = do
      return $ Eval (return . unIdentity)








instance
  Arbitrary (Context Maybe)
  where
    arbitrary = MaybeCtx <$> arbitrary

