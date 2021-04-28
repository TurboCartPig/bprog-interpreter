module Lib (
  interpret
) where

import           Data.Maybe (fromMaybe)
import           Eval   (eval)
import           Parser (parse)
import           Types  (Value)

-- | Interpret a program from string to evaluated value.
interpret :: String -> Either String Value
interpret s = do
  -- Parse the input program
  parsed <- parse s
  -- Evaluate the parsed program
  eval [] parsed
