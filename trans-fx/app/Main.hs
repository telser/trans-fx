{-#
  LANGUAGE
    FlexibleContexts
#-}

module Main where

import Control.FX

main :: IO ()
main = return ()

type Foo a = (ComposeTT IdentityTT (OverTT IdentityTT (ReadOnlyT Identity Char))) IdentityT Identity a

test :: Int -> Foo Int
test k = return k

type Bar a = IdentityTT IdentityT Identity a

test2 :: a -> Bar a
test2 = return



{-
need different tiers of lifts.
-}