-- | Module      : Control.FX.Monad.Class
--   Description : Basic monadic effect classes
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.FX.Monad.Class (
    Central(..)

  , RunMonad(..)
  , Output(..)

  , MonadTrans(..)
  , RunMonadTrans(..)
  , InputT(..)
  , OutputT(..)

  -- * Specialized Lifts
  , Catch
  , LiftCatch(..)
  , Draft
  , LiftDraft(..)
  , Local
  , LiftLocal(..)

  -- * Basic Effects
  , MonadIdentity(..)
  , MonadHalt(..)
  , MonadExcept(..)
  , MonadState(..)
  , MonadWriteOnly(..)
  , MonadReadOnly(..)
  , MonadAppendOnly(..)
  , MonadWriteOnce(..)
  , MonadPrompt(..)
) where



import Control.FX.EqIn
import Control.FX.Functor



-- | Class representing monads that 'commute' with every other monad.
-- Instances should satisfy the following laws:
--
-- > (1) commute . return === fmap return
-- >
-- > (2) commute . join === fmap join . commute . fmap commute
-- >
-- > (3) commute . fmap join === join . fmap commute . commute
class
  ( Commutant c, Monad c
  ) => Central c

instance Central Maybe
instance Central (Either e)

instance
  ( Monoid a
  ) => Central (Pair a)





-- | Class representing monads that can be "run" inside some context
-- @z@, producing a value in some result context @f@.
class
  ( Monad m
  ) => RunMonad m
  where
    data Input m
      :: *

    data Output m
      :: * -> *

    -- | Run a monadic computation in context
    run
      :: Input m
      -> m a
      -> Output m a



instance RunMonad Maybe where
  data Input Maybe
    = MaybeIn
        { unMaybeIn :: ()
        } deriving (Eq, Show)

  data Output Maybe a
    = MaybeOut
        { unMaybeOut :: Maybe a
        } deriving (Eq, Show)

  run _ = MaybeOut



{- MonadTrans -}

-- | Class representing monad transformers
class
  ( forall m. (Monad m) => Monad (t m)
  ) => MonadTrans
    (t :: (* -> *) -> * -> *)
  where
    -- | Lift a computation from the inner monad to the transformed monad
    lift
      :: ( Monad m )
      => m a
      -> t m a



-- | Class representing monad transformers which can be run in an input context, producting a monadic value in an output context.
class
  ( MonadTrans t
  ) => RunMonadTrans t
  where
    data InputT t
      :: *

    data OutputT t
      :: * -> *

    runT
      :: ( Monad m )
      => InputT t
      -> t m a
      -> m (OutputT t a)



{- Specialized Lifts -}

-- | The signature of @catch@ from @MonadExcept@
type Catch e m a = m a -> (e -> m a) -> m a

-- | Class representing monad transformers through which
-- @catch@ from @MonadExcept@ can be lifted. Instances
-- should satisfy the following law:
--
-- > (1) lift (catch x h) === liftCatch catch (lift x) (lift . h)
class
  ( MonadTrans t, RunMonadTrans t
  ) => LiftCatch t
  where
    liftCatch
      :: ( Monad m )
      => Catch e m (OutputT t a)
      -> Catch e (t m) a

-- | The signature of @draft@ from @MonadWriteOnly@
type Draft w m a = m a -> m (Pair w a)

-- | Class representing monad transformers through which
-- @draft@ from @MonadWriteOnly@ can be lifted. Instances
-- should satisfy the following law:
--
-- > (1) liftDraft draft (lift x) === lift (draft x)
class
  ( MonadTrans t, RunMonadTrans t
  ) => LiftDraft t
  where
    liftDraft
      :: ( Monad m, Monoid w )
      => Draft w m (OutputT t a)
      -> Draft w (t m) a

-- | The signature of @local@ from @MonadReadOnly@
type Local r m a = (r -> r) -> m a -> m a

-- | Class representing monad transformers through which @local@ from @MonadReadOnly@ can be lifted
class
  ( MonadTrans t, RunMonadTrans t
  ) => LiftLocal t
  where
    liftLocal
      :: ( Monad m )
      => Local r m (OutputT t a)
      -> Local r (t m) a





{- Effect Classes -}

-- | Class representing monads from which we can extract a pure value.
-- Instances should satisfy the following laws:
--
-- > (1) unwrap . return === id
-- >
-- > (2) return . unwrap === id
-- >
-- > (3) x >>= f === f (unwrap x)
class
  ( Monad m
  , forall x. (Eq x) => Eq (m x)
  , forall x. (Semigroup x) => Semigroup (m x)
  , forall x. (Monoid x) => Monoid (m x)
  ) => MonadIdentity m
  where
    -- | Extract a pure value
    unwrap :: m a -> a

instance
  ( Renaming f
  ) => MonadIdentity (Wrap f)
  where
    unwrap = namingInv . unWrap



-- | Class representing monads which can fail catastrophically, returning
-- nothing. Instances should satisfy the following laws:
--
-- > (1) halt a >> x === halt a
class
  ( Monad m, MonadIdentity mark
  ) => MonadHalt mark m
  where
    -- | Fail catastrophically, returning nothing.
    halt :: mark () -> m a

    default halt
      :: ( Monad m1, MonadTrans t1, m ~ t1 m1
         , MonadHalt mark m1 )
      => mark ()
      -> m a
    halt = lift . halt




-- | Class representing monads which can raise and handle marked exceptions
-- of type @mark e@. Instances should satisfy the following laws:
--
-- > (1) catch (return a) h === return a
-- >
-- > (2) catch (throw e) h === h e
-- >
-- > (3) throw e >>= f === throw e
class
  ( Monad m, MonadIdentity mark
  ) => MonadExcept mark e m
  where
    -- | Raise an exception
    throw :: mark e -> m a

    default throw
      :: ( Monad m1, MonadTrans t1, m ~ t1 m1
         , MonadExcept mark e m1 )
      => mark e
      -> m a
    throw = lift . throw

    -- | Run a computation, applying a handler to any raised exceptions
    catch :: m a -> (mark e -> m a) -> m a

    default catch
      :: ( Monad m1, MonadTrans t1, m ~ t1 m1
         , LiftCatch t1, MonadExcept mark e m1 )
      => m a
      -> (mark e -> m a)
      -> m a
    catch = liftCatch catch



-- | Class representing monads with access to a marked mutable state @mark s@.
-- Instances should satisfy the following laws:
--
-- > (1) put s1 >> put s2 === put s2
-- >
-- > (2) put s >> get === put s >> return s
-- >
-- > (3) get >>= put === return ()
-- >
-- > (4) get >>= \\s -> get >>= k s === get >>= \\s -> k s s
class
  ( Monad m, MonadIdentity mark
  ) => MonadState mark s m
  where
    -- | Retrieve the current state
    get :: m (mark s)

    default get
      :: ( Monad m1, MonadTrans t1, m ~ t1 m1
         , MonadState mark s m1 )
      => m (mark s)
    get = lift get

    -- | Replace the current state
    put :: (mark s) -> m ()

    default put
      :: ( Monad m1, MonadTrans t1, m ~ t1 m1
         , MonadState mark s m1 )
      => mark s
      -> m ()
    put = lift . put



-- | Class representing monads with access to a marked write-only state
-- @mark w@. Note that @w@ must be an instance of @Monoid@. Instances should
-- satisfy the following laws:
--
-- > (1) draft (tell w) === return ((),w)
-- >
-- > (2) tell mempty === return ()
-- >
-- > (3) tell w1 >> tell w2 === tell (mappend w1 w2)
-- >
-- > (4) draft (return a) === return (a, mempty)
-- >
-- > (5) draft (x >>= f) === draft x >>= (draft' f)
-- >       where draft' f (a,w) = mapsnd (mappend w) <$> draft (f a)
class
  ( Monad m, Monoid w, MonadIdentity mark
  ) => MonadWriteOnly mark w m
  where
    -- | Combine a value with the current write-only state
    tell :: mark w -> m ()

    default tell
      :: ( Monad m1, MonadTrans t1, m ~ t1 m1
         , MonadWriteOnly mark w m1)
      => mark w
      -> m ()
    tell = lift . tell

    -- | Run a computation, returning the write-only state
    -- with the result rather than writing it
    draft :: m a -> m (Pair (mark w) a)

    default draft
      :: ( Monad m1, MonadTrans t1, m ~ t1 m1
         , LiftDraft t1, MonadWriteOnly mark w m1 )
      => m a
      -> m (Pair (mark w) a)
    draft = liftDraft draft



-- | Class representing monads with access to a marked read-only state
-- @mark r@. Instances should satisfy the following laws:
--
-- > (1) local u ask === fmap u ask
-- >
-- > (2) local u (local v x) === local (v . u) x
-- >
-- > (3) local u x >> ask === ask >>= \r -> local u x >> return r
-- >
-- > (4) local u (return a) === return a
-- >
-- > (5) local u (x >>= f) === local u x >>= (local u . f)
class
  ( Monad m, MonadIdentity mark
  ) => MonadReadOnly mark r m
  where
    -- | Retrieve the read-only state
    ask :: m (mark r)

    default ask
      :: ( Monad m1, MonadTrans t1, m ~ t1 m1
         , MonadReadOnly mark r m1 )
      => m (mark r)
    ask = lift ask

    -- | Run a computation with a locally modified
    --   read-only state
    local :: (mark r -> mark r) -> m a -> m a

    default local
      :: ( Monad m1, MonadTrans t1, m ~ t1 m1
         , LiftLocal t1, MonadReadOnly mark r m1 )
      => (mark r -> mark r)
      -> m a
      -> m a
    local = liftLocal local



-- | Class representing monads with access to a marked append-only state
-- @mark w@. Instances should satisfy the following laws:
--
-- > (1) jot mempty  ===  return ()
-- >
-- > (2) jot (a <> b)  ===  jot a >> jot b
-- >
-- > (3) look  ===  return mempty
-- >
-- > (4) x >> look >> y  ===  x >> y
-- >
-- > (5) jot w >> look  ===  jot w >> return w
class
  ( Monad m, MonadIdentity mark
  ) => MonadAppendOnly mark w m
  where
    -- | Retrieve the append-only state
    look :: m (mark w)

    default look
      :: ( Monad m1, MonadTrans t1, m ~ t1 m1
         , MonadAppendOnly mark w m1 )
      => m (mark w)
    look = lift look

    -- | Append a value to the state
    jot :: mark w -> m ()

    default jot
      :: ( Monad m1, MonadTrans t1, m ~ t1 m1
         , MonadAppendOnly mark w m1 )
      => mark w
      -> m ()
    jot = lift . jot



-- | Class representing monads with access to a write-once, read-many state
-- @mark w@. Instances should satisfy the following laws.
--
-- > (1) etch a >> etch b  ===  etch a >> return False
-- >
-- > (2) etch a >> press  ===  return (Just $ pure a)
class
  ( Monad m, MonadIdentity mark
  ) => MonadWriteOnce mark w m
  where
    -- | Attempt to record the write-once state, returning @True@ if and
    -- only if the write succeeds.
    etch :: mark w -> m Bool

    default etch
      :: ( Monad m1, MonadTrans t1, m ~ t1 m1
         , MonadWriteOnce mark w m1 )
      => mark w
      -> m Bool
    etch = lift . etch

    -- | Attempt to read a copy of the write-once state.
    press :: m (Maybe (mark w))

    default press
      :: ( Monad m1, MonadTrans t1, m ~ t1 m1
         , MonadWriteOnce mark w m1 )
      => m (Maybe (mark w))
    press = lift press



-- | Class representing monads which can prompt an oracle for a monadic result.
class
  ( Monad m, MonadIdentity mark
  ) => MonadPrompt mark (p :: * -> *) m
  where
    -- | Prompt an oracle of type @mark (p a)@, receiving a monadic result
    prompt :: mark (p a) -> m (mark a)

    default prompt
      :: ( Monad m1, MonadTrans t1, m ~ t1 m1
         , MonadPrompt mark p m1 )
      => mark (p a)
      -> m (mark a)
    prompt = lift . prompt
