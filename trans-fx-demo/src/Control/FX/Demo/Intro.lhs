---
title: Effects with monad transformer transformers
---

Hi! This is a technical introduction to `trans-fx`, an effect framework for Haskell. To say anything interesting I'll have to make a few assumptions about you, dear reader, and I think it's best to be up front about these. This document will make the most sense to you if:

1. You have some experience writing in a functional programming language, and a language with Hindley-Milner style type inference, and found these ideas to be to your liking.
2. You appreciate the use of monads to control side-effects and the use of monad transformers as a strategy for building complex monads from simpler ones.
3. You are invested in testing as a tool for building software that is responsive to changing requirements and resistant to decay.

> module Control.FX.Demo.Intro where




Monads
------

Strictly typed pure functional programming is great! It is _literally mathematics_, opening some interesting design possibilities around abstraction and program correctness. In this setting, monads are a powerful tool for writing and reasoning about code with side effects. Recall that a _monad_ is any type constructor equipped with two special functions, `return` and `>>=` (pronounced "bind"):

```haskell
class Monad m where
  return :: a -> m a

  (>>=) :: m a -> (a -> m b) -> m b
```

But there's more -- these functions also have to satisfy three properties, called the _monad laws_:

1. Left Identity: `(return a) >>= f  ===  (f a)`
2. Right Identity: `x >>= return  ===  x`
3. Associativity: `(x >>= f) >>= g  ===  x >>= (\a -> f a >>= g)`

(The names for the laws make more sense when interpreted using an equivalent formulation called a _kleisli category_, but that's not important.)

This way of thinking -- a kind of _thing_, with _operators_ on those things that satisfy _axioms_ -- is the bread and butter of algebra and the reason why monads are such an exciting and useful tool.





import Control.FX

data A a = A { unA :: a } deriving (Eq, Show)
data B a = B { unB :: a } deriving (Eq, Show)

instance Functor A where
  fmap f (A x) = A (f x)
instance Applicative A where
  pure = A
  (A f) <*> (A x) = A (f x)
instance Monad A where
  return = A
  (A x) >>= f = f x
instance MonadIdentity A where
  unwrap = unA

instance Functor B where
  fmap f (B x) = B (f x)
instance Applicative B where
  pure = B
  (B f) <*> (B x) = B (f x)
instance Monad B where
  return = B
  (B x) >>= f = f x
instance MonadIdentity B where
  unwrap = unB

type Test t m a = ComposeTT (TeletypeTT A) (TeletypeTT B) t m a

foo :: (Monad m) => Test IdentityT m ()
foo = do
  printLine $ A $ "What is your name?"
  B msg <- readLine
  printLine $ A $ "hello, " ++ msg
  return ()

bar :: TeletypeTT Identity IdentityT IO () -> IO ()
bar x = do
  z <- runIdentityT $ runTeletypeTT evalTeletypeIO x
  putStrLn $ show z
