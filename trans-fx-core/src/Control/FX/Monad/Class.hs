-- | Module      : Control.FX.Monad.Class
--   Description : Basic monadic effect classes
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-#
  LANGUAGE
    InstanceSigs,
    KindSignatures,
    FlexibleInstances,
    MultiParamTypeClasses
#-}

module Control.FX.Monad.Class (
    Central(..)
  , RunMonad(..)

  -- * Basic Effects
  , MonadIdentity(..)
  , MonadMaybe(..)
  , MonadExcept(..)
  , MonadState(..)
  , MonadWriteOnly(..)
  , MonadReadOnly(..)
  , MonadPrompt(..)
) where



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
instance Central Tag



-- | Class representing monads that can be "run" inside some context
-- @z@, producing a value in some result context @f@.
class
  ( Monad m, Commutant f
  ) => RunMonad z m f
  where
    run :: z -> m a -> f a

instance RunMonad () Maybe Maybe where
  run () = id





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
  ) => MonadIdentity m
  where
    -- | Extract a pure value
    unwrap :: m a -> a



-- | Class representing monads which can fail catastrophically, returning
-- nothing. Instances should satisfy the following laws:
--
-- > (1) bail >> x === bail
class
  ( Monad m
  ) => MonadMaybe m
  where
    -- | Fail catastrophically, returning nothing.
    bail :: m a



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
    -- | Run a computation, applying a handler to any raised exceptions
    catch :: m a -> (mark e -> m a) -> m a



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
    -- | Replace the current state
    put :: (mark s) -> m ()



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
    -- | Run a computation, returning the write-only state
    -- with the result rather than writing it
    draft :: m a -> m (Pair (mark w) a)



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
    -- | Run a computation with a locally modified
    --   read-only state
    local :: (mark r -> mark r) -> m a -> m a



class
  ( Monad m, MonadIdentity mark
  ) => MonadPrompt mark (p :: * -> *) m
  where
    prompt :: p (mark a) -> m (mark a)
