module Main where

import           Eval            (eval)
import           Lib             (interpret)
import           Parser          (parse)
import           Test.DocTest    (doctest)
import           Test.Hspec      (Spec, describe, hspec, it)
import           Test.QuickCheck
import           Types

-- | `it` specialized for parser.
pit :: String -> String -> Value -> Spec
pit text input output = do
  it text $ do
    (valueify . parse $ input) == Just output
  where
    -- | Turn a list of tokens into a single Value if possible
    valueify :: Maybe [Token] -> Maybe Value
    valueify (Just [Val x]) = return x
    valueify _              = Nothing

-- | Some basic unit tests for parsing primitive values.
parseSpec :: Spec
parseSpec = do
  describe "parser" $ do
    pit "parses an int" "1"
      (VInt 1)
    pit "parses a float" "1.2"
      (VFloat 1.2)
    pit "parses true as bool" "true"
      (VBool True)
    pit "parses false as bool" "false"
      (VBool False)
    pit "parses a string" "\"this is a string\""
      (VString "this is a string")
    pit "parses a list" "[ 1 2 3 ]"
      (VList [VInt 1, VInt 2, VInt 3])
    pit "parses a nested list" "[ [ 1 ] 2 [ 3 ] ]"
      (VList [VList [VInt 1], VInt 2, VList [VInt 3]])
    pit "parses a quotation" "{ 1 2 }"
      (VQuotation [Val (VInt 1), Val (VInt 2)])
    pit "parses a nested quotation" "{ 1 2 { 3 } }"
      (VQuotation [Val (VInt 1), Val (VInt 2), Val (VQuotation [Val (VInt 3)])])

main :: IO ()
main = do
  putStrLn "DocTests:"
  doctest ["app", "src"]

  putStrLn "Unit tests:"
  hspec parseSpec
