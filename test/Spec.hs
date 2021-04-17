module Main where

-- * Doctests
-- * Test utils
-- * Implement tests from spec

import Test.DocTest (doctest)
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = do
  putStrLn "DocTests:"
  doctest ["app", "src"]
