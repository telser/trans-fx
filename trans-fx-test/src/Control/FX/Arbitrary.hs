{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Control.FX.Arbitrary () where

import Test.QuickCheck (Arbitrary(..), CoArbitrary(..), Gen, elements)
import System.IO.Error
import Network.HTTP.Req ( HttpException(..) )

import Control.FX
import Control.FX.IO
import Control.FX.Data

import Data.Time.Clock.System
  ( SystemTime(..) )



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
  ( CoArbitrary a
  ) => CoArbitrary (LeftZero a)
  where
    coarbitrary x gen =
      case x of
        LeftUnit -> gen
        LeftZero a -> coarbitrary a gen

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

instance
  ( Arbitrary a
  ) => Arbitrary (Output Identity a)
  where
    arbitrary = IdentityOut <$> arbitrary



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

instance
  ( Arbitrary a
  ) => Arbitrary (Output (Halt mark) a)
  where
    arbitrary = HaltOut <$> arbitrary



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

instance
  ( Arbitrary a, Arbitrary e
  ) => Arbitrary (Output (Except mark e) a)
  where
    arbitrary = ExceptOut <$> arbitrary



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

instance
  ( Arbitrary a, Arbitrary (mark w)
  ) => Arbitrary (Output (WriteOnly mark w) a)
  where
    arbitrary = WriteOnlyOut <$> arbitrary



{- AppendOnly -}

instance
  ( Arbitrary a, Arbitrary w, Monoid w
  ) => Arbitrary (AppendOnly mark w a)
  where
    arbitrary = do
      w <- arbitrary
      a <- arbitrary
      return $ AppendOnly $ \_ -> Pair w a

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

instance
  ( Arbitrary a, Arbitrary (mark w)
  ) => Arbitrary (Output (AppendOnly mark w) a)
  where
    arbitrary = AppendOnlyOut <$> arbitrary



{- WriteOnce -}

instance
  ( Arbitrary a, Arbitrary w
  ) => Arbitrary (WriteOnce mark w a)
  where
    arbitrary = do
      w <- arbitrary
      a <- arbitrary
      return $ WriteOnce $ \_ -> Pair w a

instance
  ( Arbitrary (mark ())
  ) => Arbitrary (Context (WriteOnce mark w))
  where
    arbitrary = WriteOnceCtx <$> arbitrary

instance
  ( Arbitrary (mark ())
  ) => Arbitrary (Input (WriteOnce mark w))
  where
    arbitrary = WriteOnceIn <$> arbitrary

instance
  ( Arbitrary a, Arbitrary (mark (Maybe w))
  ) => Arbitrary (Output (WriteOnce mark w) a)
  where
    arbitrary = WriteOnceOut <$> arbitrary



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

instance
  ( Arbitrary (mark a)
  ) => Arbitrary (Output (ReadOnly mark r) a)
  where
    arbitrary = ReadOnlyOut <$> arbitrary



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

instance
  ( Arbitrary a, Arbitrary (mark s)
  ) => Arbitrary (Output (State mark s) a)
  where
    arbitrary = StateOut <$> arbitrary



{- Stack -}

instance
  ( Arbitrary a, Arbitrary (f d), CoArbitrary (f d)
  ) => Arbitrary (Stack mark f d a)
  where
    arbitrary = Stack <$> arbitrary

instance
  ( Arbitrary (mark (f d))
  ) => Arbitrary (Context (Stack mark f d))
  where
    arbitrary = StackCtx <$> arbitrary

instance
  ( Arbitrary (mark (f d))
  ) => Arbitrary (Input (Stack mark f d))
  where
    arbitrary = StackIn <$> arbitrary

instance
  ( Arbitrary a, Arbitrary (mark (f d))
  ) => Arbitrary (Output (Stack mark f d) a)
  where
    arbitrary = StackOut <$> arbitrary





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
  ( Arbitrary (m (Pair w a)), Arbitrary w
  , Monoid w, CoArbitrary w, Monad m, Arbitrary a
  ) => Arbitrary (AppendOnlyT mark w m a)
  where
    arbitrary = do
      w <- arbitrary
      a <- arbitrary
      return $ AppendOnlyT $ \_ ->
        return $ Pair w a

instance
  ( Arbitrary (mark w), Arbitrary (Context m), MonadIdentity mark, Monoid w
  ) => Arbitrary (Context (AppendOnlyT mark w m))
  where
    arbitrary = do
      c <- arbitrary
      return $ AppendOnlyTCtx (pure mempty, c)

instance
  ( Arbitrary (mark ())
  ) => Arbitrary (InputT (AppendOnlyT mark w))
  where
    arbitrary = AppendOnlyTIn <$> arbitrary



{- WriteOnceT -}

instance
  ( Arbitrary (m (Pair (LeftZero w) a)), Arbitrary w, CoArbitrary w
  ) => Arbitrary (WriteOnceT mark w m a)
  where
    arbitrary = WriteOnceT <$> arbitrary

instance
  ( Arbitrary (mark (Maybe w)), Arbitrary (Context m), MonadIdentity mark
  ) => Arbitrary (Context (WriteOnceT mark w m))
  where
    arbitrary = do
      c <- arbitrary
      return $ WriteOnceTCtx (pure mempty, c)

instance
  ( Arbitrary (mark ())
  ) => Arbitrary (InputT (WriteOnceT mark w))
  where
    arbitrary = WriteOnceTIn <$> arbitrary



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



{- ComposeT -}

instance
  ( Arbitrary (t1 (t2 m) a), ComposableT t1
  ) => Arbitrary (ComposeT t1 t2 m a)
  where
    arbitrary = toComposeT <$> arbitrary

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



{- StackT -}

instance
  ( Arbitrary (m (Pair (f d) a)), CoArbitrary (f d)
  ) => Arbitrary (StackT mark f d m a)
  where
    arbitrary = StackT <$> arbitrary

instance
  ( Arbitrary (mark (f d)), Arbitrary (Context m)
  ) => Arbitrary (Context (StackT mark f d m))
  where
    arbitrary = StackTCtx <$> arbitrary

instance
  ( Arbitrary (mark (f d))
  ) => Arbitrary (InputT (StackT mark f d))
  where
    arbitrary = StackTIn <$> arbitrary





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
  ( Arbitrary (t m (Pair w a)), Arbitrary w, Arbitrary a
  , Monoid w, CoArbitrary w, Monad m, MonadTrans t
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



{- WriteOnceTT -}

instance
  ( Arbitrary (t m (Pair (LeftZero w) a)), Arbitrary w, CoArbitrary w
  ) => Arbitrary (WriteOnceTT mark w t m a)
  where
    arbitrary = WriteOnceTT <$> arbitrary

instance
  ( Arbitrary (mark ()), Arbitrary (Context (t m))
  ) => Arbitrary (Context (WriteOnceTT mark w t m))
  where
    arbitrary = WriteOnceTTCtx <$> arbitrary

instance
  ( Arbitrary (mark ())
  ) => Arbitrary (InputTT (WriteOnceTT mark w) m)
  where
    arbitrary = WriteOnceTTIn <$> arbitrary



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

instance
  ( Monad m
  ) => Arbitrary (Eval Identity m)
  where
    arbitrary = do
      return $ Eval (return . unIdentity)



{- OverTT -}

instance
  ( Monad m, MonadTrans t, MonadTrans w, OverableT w
  , MonadTransTrans u, Arbitrary (w (u t m) a)
  ) => Arbitrary (OverTT w u t m a)
  where
    arbitrary = toOverTT <$> arbitrary

instance
  ( Arbitrary (Context (t m)), Arbitrary (InputTT u m), Arbitrary (InputT v)
  ) => Arbitrary (Context (OverTT v u t m))
  where
    arbitrary = OverTTCtx <$> arbitrary

instance
  ( Arbitrary (InputTT u m), Arbitrary (InputT v)
  ) => Arbitrary (InputTT (OverTT v u) m)
  where
    arbitrary = OverTTIn <$> arbitrary





{------}
{- IO -}
{------}

{- TeletypeTT -}

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, Arbitrary a
  , Arbitrary (t m a)
  ) => Arbitrary (TeletypeTT mark t m a)
  where
    arbitrary = do
      p <- arbitrary
      if p
        then do
          a <- arbitrary
          return $ do
            (_ :: mark String) <- readLine
            return a
        else do
          x <- pure <$> arbitrary :: Gen (mark String)
          a <- arbitrary
          return $ do
            printLine x
            return a

instance
  ( Arbitrary (Context (t m)), Monad m, Applicative mark
  ) => Arbitrary (Context (TeletypeTT mark t m))
  where
    arbitrary = do
      (m :: String) <- arbitrary
      (p1 :: Bool) <- arbitrary
      (err1 :: mark IOException) <- pure <$> arbitrary
      (p2 :: Bool) <- arbitrary
      (err2 :: mark IOException) <- pure <$> arbitrary
      let
        eval :: TeletypeAction mark u -> m u
        eval x = case x of
          ReadLine ->
            return $ if p1 then Except err1 else Accept m
          PrintLine _ ->
            return $ if p2 then Except err2 else Accept ()
      h <- arbitrary
      return $ TeletypeTTCtx (Eval eval, h)

instance Arbitrary IOException where
  arbitrary = do
    t <- elements [doesNotExistErrorType]
    s <- arbitrary
    return $ mkIOError t s Nothing Nothing



{- SimpleHttpTT -}

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, Arbitrary a
  , Arbitrary (t m a)
  ) => Arbitrary (SimpleHttpTT mark t m a)
  where
    arbitrary = return <$> arbitrary

instance
  ( Arbitrary (Context (t m)), Monad m, Applicative mark
  ) => Arbitrary (Context (SimpleHttpTT mark t m))
  where
    arbitrary = do
      (err :: mark HttpException) <- pure <$> arbitrary
      let
        eval :: SimpleHttpAction mark u -> m u
        eval x = case x of
          SimpleHttpReq _ _ _ _ _ ->
            return $ Except err
      h <- arbitrary
      return $ SimpleHttpTTCtx (Eval eval, h)

instance Arbitrary HttpException where
  arbitrary = JsonHttpException <$> arbitrary



{- SystemClockTT -}

instance
  ( Monad m, MonadTrans t, MonadIdentity mark, Arbitrary a
  , Arbitrary (t m a)
  ) => Arbitrary (SystemClockTT mark t m a)
  where
    arbitrary = do
      a <- arbitrary
      return $ do
        (_ :: mark SystemTime) <- getSystemTime
        return a

instance
  ( Arbitrary (Context (t m)), Monad m, Applicative mark
  ) => Arbitrary (Context (SystemClockTT mark t m))
  where
    arbitrary = do
      (z :: SystemTime) <- arbitrary
      let
        eval :: SystemClockAction mark u -> m u
        eval x = case x of
          GetSystemTime -> return z
      h <- arbitrary
      return $ SystemClockTTCtx (Eval eval, h)

instance Arbitrary SystemTime where
  arbitrary = MkSystemTime <$> arbitrary <*> arbitrary





{--------}
{- Data -}
{--------}

{- StackTT -}

instance
  ( Arbitrary (t m (Pair (f d) a)), CoArbitrary (f d), IsStack f
  ) => Arbitrary (StackTT mark f d t m a)
  where
    arbitrary = StackTT <$> arbitrary

instance
  ( Arbitrary (mark (f d)), Arbitrary (Context (t m))
  ) => Arbitrary (Context (StackTT mark f d t m))
  where
    arbitrary = StackTTCtx <$> arbitrary

instance
  ( Arbitrary (mark (f d))
  ) => Arbitrary (InputTT (StackTT mark f d) m)
  where
    arbitrary = StackTTIn <$> arbitrary













instance
  Arbitrary (Context Maybe)
  where
    arbitrary = MaybeCtx <$> arbitrary

