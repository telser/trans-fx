module Control.FX.Structure.Stack (
    IsStack(..)
) where

class
  ( Functor t
  ) => IsStack t
  where
    stackInit :: t a

    stackPush :: a -> t a -> t a

    stackPop :: t a -> Maybe (a, t a)



instance IsStack [] where
  stackInit = []
  stackPush = (:)
  stackPop xs = case xs of
    []     -> Nothing
    (a:as) -> Just (a, as)
