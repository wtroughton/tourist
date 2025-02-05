{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit ((@?=))

import Test.Tasty qualified
import Test.Tasty.HUnit qualified

import Tour qualified

main :: IO ()
main = Test.Tasty.defaultMain tests

tests :: TestTree
tests =
    Test.Tasty.testGroup "Data.Text" [unitTests]

unitTests =
    Test.Tasty.testGroup
        "Unit tests"
        [ Test.Tasty.HUnit.testCase "Reverse string"
            $ Tour.reverse "abc" @?= "cba"
        ]
