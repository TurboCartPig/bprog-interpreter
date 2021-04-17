-- | The tests implemented here are not identical to the official tests,
-- but they are equivilant in terms of what they test.
module Main where

import           Eval            (eval)
import           GHC.Base
import           Lib             (interpret)
import           Parser          (parse)
import           Test.DocTest    (doctest)
import           Test.Hspec      (Spec, describe, hspec, it)
import           Test.QuickCheck
import           Types

-- | `it` specialized for parser.
pit :: String -> String -> Value -> Spec
pit text input expected = do
  it text $ do
    (valueify . parse $ input) == Just expected
  where
    -- | Turn a list of tokens into a single Value if possible
    valueify :: Maybe [Token] -> Maybe Value
    valueify (Just [Val x]) = return x
    valueify _              = Nothing

-- | `it` specialized for interpret.
iit :: String -> Value -> Spec
iit input expected = do
  it input $ do
    interpret input == Just expected

-- | Some basic unit tests for parsing primitive values.
parseSpec :: Spec
parseSpec = do
  describe "parser" $ do
    pit "parses an int" "1"
      (VInt 1)
    pit "parses a negative int" "-1"
      (VInt (-1))
    pit "parses a float" "1.2"
      (VFloat 1.2)
    pit "parses a negative float" "-1.2"
      (VFloat (-1.2))
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

interpretSpec :: Spec
interpretSpec = do
  describe "interpreter" $ do
    describe "interprets literals" $ do
      iit "1"
        (VInt 1)
      iit "1.1"
        (VFloat 1.1)
    describe "interprets simple arithmetic" $ do
      iit "1 1 +"
        (VInt 2)
      iit "10 20 *"
        (VInt 200)
      iit "10 2 div"
        (VInt 5)
      iit "20 2 /"
        (VFloat 10.0)
    describe "interprets arithmetic with type coercion" $ do
      iit "1 1.0 +"
        (VFloat 2.0)
      iit "10 20.0 *"
        (VFloat 200.0)
      iit "20 2.0 div"
        (VInt 10)
      iit "20.0 2.0 div"
        (VInt 10)
    describe "bool operations" $ do
      iit "false false &&"
        (VBool False)
      iit "false true ||"
        (VBool True)
      iit "false not"
        (VBool True)
      iit "true not"
        (VBool False)
    describe "comparisons" $ do
      iit "20 10 <"
        (VBool False)
      iit "20 10 >"
        (VBool True)
      iit "20 10.0 >"
        (VBool True)
      iit "20 20.0 >"
        (VBool False)
      iit "10 10 =="
        (VBool True)
      iit "10 10.0 =="
        (VBool True)
      iit "true true =="
        (VBool True)
      iit "true 40 40 == =="
        (VBool True)
      iit "\"abba\" \"abba\" =="
        (VBool True)
      iit "[ ] [ ] =="
        (VBool True)
      iit "[ 1 2 ] [ 1 2 ] =="
        (VBool True)
      iit "[ [ ] ] [ [ ] ] =="
        (VBool True)
    describe "stack operations" $ do
      iit "10 20 swp pop"
        (VInt 20)
      iit "10 dup dup + swp pop"
        (VInt 20)
      iit "10 20 swp dup + div"
        (VInt 1)
    describe "length" $ do
      iit "\"hello\" length"
        (VInt 5)
      iit "\"hello world\" length"
        (VInt 11)
      iit "[ 1 2 3 [ ] ] length"
        (VInt 4)
      iit "{ 10 20 + } length"
        (VInt 3)
    describe "string parsing" $ do
      iit "\"12\" parseInteger"
        (VInt 12)
      iit "\"12.34\" parseFloat"
        (VFloat 12.34)
      iit "\"adam bob charlie\" words"
        (VList [VString "adam", VString "bob", VString "charlie"])
    describe "lists" $ do
      iit "[ 1 2 3 ]"
        (VList [VInt 1, VInt 2, VInt 3])
      iit "[ 1 \"bob\" ]"
        (VList [VInt 1, VString "bob"])
      iit "[ 1 2 ] empty"
        (VBool False)
      iit "[ 1 2 3 ] head"
        (VInt 1)
      iit "[ 1 2 3 ] length"
        (VInt 3)
      iit "[ 1 2 3 ] tail"
        (VList [VInt 2, VInt 3])
      iit "1 [ ] cons"
        (VList [VInt 1])
      iit "1 [ 2 3 ] cons"
        (VList [VInt 1, VInt 2, VInt 3])
      iit "[ 1 ] [ 2 3 ] append"
        (VList [VInt 1, VInt 2, VInt 3])
      iit "[ 1 2 ] [ ] append"
        (VList [VInt 1, VInt 2])
      iit "[ ] [ 1 2 ] append"
        (VList [VInt 1, VInt 2])
      iit "[ 1 ] [ 2 3 ] cons"
        (VList [VList [VInt 1], VInt 2, VInt 3])
    describe "list quotations" $ do
      it "" False
    describe "assignments" $ do
      it "" False
    describe "quotations" $ do
      iit "{ 20 10 + } exec"
        (VInt 30)
      iit "20 { 10 + } exec"
        (VInt 30)
      iit "20 10 { + } exec"
        (VInt 30)
      iit "{ { 20 10 + } exec } exec"
        (VInt 30)
      iit "{ { 20 10 + } exec 20 + } exec"
        (VInt 50)
    describe "ifs" $ do
      it "" False
    describe "if without quotations" $ do
      it "" False
    describe "times" $ do
      it "" False
    describe "loop" $ do
      it "" False
    describe "some programs" $ do
      it "" False

main :: IO ()
main = do
  putStrLn "DocTests:"
  doctest ["app", "src"]

  putStrLn "Unit tests:"
  hspec parseSpec
  hspec interpretSpec
