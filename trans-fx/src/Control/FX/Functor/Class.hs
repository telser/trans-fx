{-#
  LANGUAGE
    InstanceSigs
#-}

module Control.FX.Functor.Class (
    Central(..)
) where

class
  ( Functor f
  ) => Central f
  where
    commute
      :: (Applicative g)
      => f (g a) -> g (f a)

instance Central Maybe where
  commute :: (Applicative g) => Maybe (g a) -> g (Maybe a)
  commute x = case x of
    Nothing -> pure Nothing
    Just m  -> fmap Just m

instance Central (Either e) where
  commute :: (Applicative g) => Either e (g a) -> g (Either e a)
  commute x = case x of
    Left e  -> pure (Left e)
    Right m -> fmap Right m
