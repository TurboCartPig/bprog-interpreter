module Lib (
  interpret
) where

import           Eval
import           Parser
import           Types

-- | Interpret a program from string to evaluated value.
interpret :: String -> Maybe Value
interpret s = do
    -- Parse the input program
    parsed <- parse s
    -- Evaluate the parsed program
    eval parsed
