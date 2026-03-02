module Main (main) where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@?=))

main :: IO ()
main =
  defaultMain
    [ testGroup
        "basic tests"
        [ testCase "sanity check" sanityCheck
        ]
    ]

sanityCheck :: Assertion
sanityCheck = 1 + 1 @?= (2 :: Int)
