module Main where

import System.Environment
import Test.Tasty
import Test.Tasty.QuickCheck

import Test

main :: IO ()
main = do
  test_all_show
  setEnv "TASTY_NUM_THREADS" "3"
  setEnv "TASTY_QUICKCHECK_TESTS" "100"
  defaultMain $ testGroup "Laws"
    [ testGroup "MonadTrans"
      [ test_all_MonadTrans_T
      ]
    ]
