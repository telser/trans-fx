{-#
  LANGUAGE
    InstanceSigs
#-}

module Control.FX.Functor.Class (
    Commutant(..)
) where

class
  ( Functor d
  ) => Commutant d
  where
    commute
      :: (Applicative f)
      => d (f a) -> f (d a)

instance Commutant Maybe where
  commute
    :: ( Applicative f )
    => Maybe (f a) -> f (Maybe a)
  commute x = case x of
    Nothing -> pure Nothing
    Just m  -> fmap Just m

instance Commutant (Either e) where
  commute
    :: ( Applicative f )
    => Either e (f a) -> f (Either e a)
  commute x = case x of
    Left e  -> pure (Left e)
    Right m -> fmap Right m
