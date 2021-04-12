{-# LANGUAGE TupleSections #-}

-- | The Parser module defines a series of parser combinators that is composes
-- into high level parsers like the parse function.
module Parser where

import           Control.Applicative hiding (many, some)
import           Control.Monad
import           Data.Bifunctor (Bifunctor (first))
import qualified Data.Char      (isAlphaNum)
import           Data.Maybe     (fromMaybe)
import           Text.Read      (readMaybe)
import           Types

-- | Construct a Maybe from a char if it is equal to another char.
-- This enables do notation further down.
char :: Char -> Char -> Maybe Char
char a b = if a == b then Just a else Nothing

-- | isAlphaNum for strings.
isAlphaNum :: String -> Bool
isAlphaNum = all Data.Char.isAlphaNum

-- | Check if a string contains only alphanumeric characters.
alphanumeric :: String -> Maybe String
alphanumeric s = if isAlphaNum s then Just s else Nothing

-- | Top level parse function, takes string from file or repl and parses it into tokens.
parse :: String -> Maybe [Token]
parse s = let
    ws = words s
  in
    parse' ws

-- | Parse a list of words into tokens
parse' :: [String] -> Maybe [Token]
parse' [] = Just []
parse' ws = case parseOperator ws <|> parseValue ws of
              Nothing      -> Nothing
              Just (x, xs) -> (x :) <$> parse' xs

-- | Parses a list of words into a Value.
parseValue :: [String] -> Maybe (Token, [String])
parseValue ws =
  first Val         <$> (
    parseVInt ws    <|>
    parseVFloat ws  <|>
    parseVString ws <|>
    parseVList ws
  )

-- | Parse an Int into a Value.
--
-- >>> parseVInt ["12"]
-- Just (VInt 12,[])
--
-- >>> parseVInt ["12.2"]
-- Nothing
parseVInt :: [String] -> Maybe (Value, [String])
parseVInt []     = Nothing
parseVInt (w:ws) = (, ws) . VInt <$> readMaybe w

-- | Parse a Float into a Value.
--
-- NOTE: How parseVFloat parses integers as well, and how the ordering in parseValue is important.
-- >>> parseVFloat ["12"]
-- Just (VFloat 12.0,[])
--
-- >>> parseVFloat ["12.2"]
-- Just (VFloat 12.2,[])
parseVFloat :: [String] -> Maybe (Value, [String])
parseVFloat []     = Nothing
parseVFloat (w:ws) = (, ws) . VFloat <$> readMaybe w

-- | Parse a String into a Value VString.
-- Where a string looks like "a string". Note the lack of spaces around the quotes.
--
-- >>> parseVString ["\"Hei", "hoo\"", "1.2"]
-- Just (VString "Hei hoo",["1.2"])
--
-- >>> parseVString ["\"Hello\""]
-- Just (VString "Hello",[])
--
-- >>> parseVString ["123", "\"hoo\""]
-- Nothing
parseVString :: [String] -> Maybe (Value, [String])
parseVString [] = Nothing
parseVString ws = if (head . head $ ws) == '"'
  then Just (VString xs'', ys)
  else Nothing
  where
    -- xs and y are part of the string, ys is the rest of input not part of string
    (xs, y:ys) = break (\x -> last x == '"') ws
    -- Concat all the parts of the string back into a continues string
    xs' = unwords $ xs ++ [y]
    -- Remove surrounding double quotes
    xs'' = tail . init $ xs'

-- | Parse a List into a Value VList.
-- Where a list looks like [ "a string", 12, 1.2 ]. Note the spaces around the brackets.
--
-- >>> parseVList ["[", "12", "12.2", "\"Hei", "Hoo\"", "]"]
-- Just (VList [VInt 12,VFloat 12.2,VString "Hei Hoo"],[])
parseVList :: [String] -> Maybe (Value, [String])
parseVList [] = Nothing
parseVList (w:ws) = if w == "["
  then (, ys) . VList <$> xs'
  else Nothing
  where
    (xs, _:ys) = break (== "]") ws -- FIXME: Breaks nested lists.
    xs' = parseVListParts xs

-- | Parse the parts of a VList.
parseVListParts :: [String] -> Maybe [Value]
parseVListParts [] = Nothing
parseVListParts ws = do
  (Val xs, ys) <- parseValue ws
  Just $ xs : fromMaybe [] (parseVListParts ys)

-- | Parse a symbol into an Operator.
parseOperator :: [String] -> Maybe (Token, [String])
parseOperator [] = Nothing
parseOperator (w:ws) =
  first Op       <$> (
    parseAssign  <|>
    parseAdd     <|>
    parseSub     <|>
    parseMul     <|>
    parseDiv     <|>
    parseDivI    <|>
    parseGreater <|>
    parseLess    <|>
    parseEqual   <|>
    parseAnd     <|>
    parseOr      <|>
    parseNot
  )
  where
    lookup sym op = if w == sym then Just (op, ws) else Nothing
    parseAssign = lookup ":=" OAssign
    parseAdd = lookup "+" OAdd
    parseSub = lookup "-" OSub
    parseMul = lookup "*" OMul
    parseDiv = lookup "/" ODiv
    parseDivI = lookup "div" ODivI
    parseGreater = lookup ">" OGreater
    parseLess = lookup "<" OLess
    parseEqual = lookup "==" OEqual
    parseAnd = lookup "&&" OAnd
    parseOr = lookup "||" OOr
    parseNot = lookup "not" ONot
