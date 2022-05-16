-- | Module      : Control.FX.Monad.Trans.Trans.Class
--   Description : Monad transformer transformer classes
--   Copyright   : 2019, Automattic, Inc.
--   License     : BSD3
--   Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
--   Stability   : experimental
--   Portability : POSIX

{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.FX.Monad.Trans.Trans.Class (
    MonadTransTrans(..)

  , RunMonadTransTrans(..)
  , InputTT(..)
  , OutputTT(..)

  -- * Specialized Lifts
  , LiftCatchT(..)
  , LiftDraftT(..)
  , LiftLocalT(..)
) where



import Data.Typeable (Typeable)

import Control.FX.Functor
import Control.FX.Monad
import Control.FX.Monad.Trans



-- | Class representing monad transformer transformers
class
  ( forall t. (MonadTrans t) => MonadTrans (u t)
  , forall t m. (Monad m, MonadTrans t) => Monad (u t m)
  ) => MonadTransTrans
    (u :: ((* -> *) -> (* -> *)) -> (* -> *) -> * -> *)
  where
    liftT
      :: (Monad m, MonadTrans t)
      => t m a -> u t m a



-- | Class representing monad transformer transformers which can be "run" in a context @z m@, producing a value in context @t m (f a)@.
class
  ( MonadTransTrans u
  ) => RunMonadTransTrans u
  where
    data family InputTT u (m :: * -> *) :: *

    data family OutputTT u :: * -> *

    runTT
      :: (Monad m, MonadTrans t)
      => InputTT u m
      -> u t m a
      -> t m (OutputTT u a)





{- Specialized Lifts -}

-- | Class representing monad transformer transformers through
-- which @catch@ from @MonadExcept@ can be lifted.
class
  ( MonadTransTrans u, RunMonadTransTrans u
  ) => LiftCatchT u
  where
    liftCatchT
      :: ( Monad m, MonadTrans t )
      => (forall x. Catch e (t m) (OutputTT u x))
      -> (forall x. Catch e (u t m) x)



-- | Class representing monad transformer transformers through
-- which @draft@ from @MonadWriteOnly@ can be lifted.
class
  ( MonadTransTrans u, RunMonadTransTrans u
  ) => LiftDraftT u
  where
    liftDraftT
      :: ( Monad m, MonadTrans t, Monoid w )
      => (forall x. Draft w (t m) (OutputTT u x))
      -> (forall x. Draft w (u t m) x)



-- | Class representing monad transformer transformers through
-- which @local@ from @MonadReadOnly@ can be lifted.
class
  ( MonadTransTrans u, RunMonadTransTrans u
  ) => LiftLocalT u
  where
    liftLocalT
      :: ( Monad m, MonadTrans t )
      => (forall x. Local r (t m) (OutputTT u x))
      -> (forall x. Local r (u t m) x)
