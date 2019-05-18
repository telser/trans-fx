module Main where

import System.Environment
import Test.Tasty
import Test.Tasty.QuickCheck

import Test

main :: IO ()
main = do
  test_all_show
  setEnv "TASTY_NUM_THREADS" "3"
  setEnv "TASTY_QUICKCHECK_TESTS" "1000"
  defaultMain $ testGroup "Laws"
    [ testGroup "Functor"
      [ test_all_Functor_F
      , test_all_Functor_S
      , test_all_Functor_C
      , test_all_Functor_B
      ]
    ]
