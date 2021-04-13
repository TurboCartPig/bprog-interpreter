module Main where

import Lib

main :: IO ()
main = do
  content <- getContents
  print $ interpret content