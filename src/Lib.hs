module Lib (
  interpret
) where

import           Eval   (eval)
import           Parser (parse)
import           Types  (Value)
import           Data.Maybe (fromMaybe)

-- | Interpret a program from string to evaluated value.
interpret :: String -> Either String Value
interpret s = do
  -- Parse the input program
  parsed <- parse s
  -- Evaluate the parsed program
  eval parsed
