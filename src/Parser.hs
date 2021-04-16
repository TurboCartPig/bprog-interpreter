{-# LANGUAGE TupleSections #-}

-- | The Parser module defines a series of parser combinators that is composes
-- into high level parsers like the parse function.
module Parser (
  parse
) where

import           Control.Applicative hiding (many, some)
import           Control.Monad
import           Control.Monad.Extra
import           Data.Bifunctor      (Bifunctor (first))
import qualified Data.Char           (isAlphaNum)
import           Data.Maybe          (fromMaybe)
import           Text.Read           (readMaybe)
import           Types

-- | Define the type of all the parser combinators in this module.
-- Takes in a list of words the parse function has split the program input into.
-- Then parse the first token out of that list of words,
-- and return both the some token and the rest of the unparsed words if we succeed,
-- or nothing if we fail.
type Parser a = [String] -> Maybe (a, [String])

-- | Parse a char into another char, and return Nothing if that failed.
-- This is simply a building block for other parsers.
char :: Char -> Char -> Maybe Char
char a b = if a == b then Just a else Nothing

-- | Parse a string into another string, and return Nothing if that failed.
-- This is simply a building block for other parsers.
string :: String -> String -> Maybe String
string a b = if a == b then Just a else Nothing

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
-- This is basically a weird fold.
parse' :: [String] -> Maybe [Token]
parse' [] = Just []
parse' ws = do
  (token, rest) <- parseToken ws
  (token:) <$> parse' rest

-- | Parse a single token from the input string.
parseToken :: Parser Token
parseToken ws =
  parseOperator ws <|>
  parseBuiltin  ws <|>
  parseValue    ws

-- | Parses a list of words into a Value.
parseValue :: Parser Token
parseValue ws =
  first Val <$> (
    parseVInt       ws <|>
    parseVFloat     ws <|>
    parseVBool      ws <|>
    parseVString    ws <|>
    parseVList      ws <|>
    parseVQuotation ws
  )

-- | Parse an Int into a Value.
--
-- >>> parseVInt ["12"]
-- Just (VInt 12,[])
--
-- >>> parseVInt ["12.2"]
-- Nothing
parseVInt :: Parser Value
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
parseVFloat :: Parser Value
parseVFloat []     = Nothing
parseVFloat (w:ws) = (, ws) . VFloat <$> readMaybe w

-- | Parse a boolean into a Value.
--
-- >>> parseVBool ["true"]
--
-- >>> parseVBool ["false"]
parseVBool :: Parser Value
parseVBool []     = Nothing
parseVBool (w:ws) =
  (, ws) . VBool <$> (
    True         <$ string "true"  w <|>
    False        <$ string "false" w
  )

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
parseVString :: Parser Value
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
parseVList :: Parser Value
parseVList [] = Nothing
parseVList (w:ws) = do
  _ <- string "[" w

  (xs, w':ws') <- parseVListParts ws

  _ <- string "]" w'

  return (VList xs, ws')

-- | Parse all the elements we can find into a list of values.
-- This is done by looping over the words we take as input, parsing something out of those words,
-- and then feeding the rest back into the same machinery.
parseVListParts :: Parser [Value]
parseVListParts [] = Nothing
parseVListParts ws =
  -- Here tokens is the parsed tokens,
  -- rest and rest' are the remaining unparsed words,
  -- and x is a parsed value.
  return $ loop (\(tokens, rest) -> case parseValue rest of
                                      Nothing             -> Right (tokens, rest)
                                      Just (Val x, rest') -> Left (tokens ++ [x], rest')) ([], ws)

-- | Parse a quotation (or codeblock) into a Value.
parseVQuotation :: Parser Value
parseVQuotation []     = Nothing
parseVQuotation (w:ws) = do
  _ <- string "{" w

  (xs, w':ws') <- parseVQuotationParts ws

  _ <- string "}" w'

  return (VQuotation xs, ws')

-- | Parse the parts of a VQuotation into a list of tokens.
-- See parseVListParts to see how it works
parseVQuotationParts :: Parser [Token]
parseVQuotationParts [] = Nothing
parseVQuotationParts ws =
  return $ loop (\(tokens, rest) -> case parseToken rest of
                                      Nothing         -> Right (tokens, rest)
                                      Just (x, rest') -> Left (tokens ++ [x], rest')) ([], ws)

-- | Parse a symbol into an Operator.
parseOperator :: Parser Token
parseOperator [] = Nothing
parseOperator (w:ws) =
  (, ws) . Op <$> (
    (OAdd     <$ string "+" w)   <|>
    (OSub     <$ string "-" w)   <|>
    (OMul     <$ string "*" w)   <|>
    (ODiv     <$ string "/" w)   <|>
    (ODivI    <$ string "div" w) <|>
    (OGreater <$ string ">" w)   <|>
    (OLess    <$ string "<" w)   <|>
    (OEqual   <$ string "==" w)  <|>
    (OAnd     <$ string "&&" w)  <|>
    (OOr      <$ string "||" w)  <|>
    (ONot     <$ string "not" w)
  )

parseBuiltin :: Parser Token
parseBuiltin []     = Nothing
parseBuiltin (w:ws) =
  (, ws) . Bi <$> (
    (BDup          <$ string "dup"          w) <|>
    (BSwp          <$ string "swp"          w) <|>
    (BPop          <$ string "pop"          w) <|>
    (BParseInteger <$ string "parseInteger" w) <|>
    (BParseFloat   <$ string "parseFloat"   w) <|>
    (BWords        <$ string "words"        w) <|>
    (BHead         <$ string "head"         w) <|>
    (BTail         <$ string "tail"         w) <|>
    (BEmpty        <$ string "empty"        w) <|>
    (BLength       <$ string "length"       w) <|>
    (BCons         <$ string "cons"         w) <|>
    (BAppend       <$ string "append"       w) <|>
    (BExec         <$ string "exec"         w) <|>
    (BTimes        <$ string "times"        w) <|>
    (BMap          <$ string "map"          w) <|>
    (BFoldl        <$ string "foldl"        w) <|>
    (BEach         <$ string "each"         w) <|>
    (BIf           <$ string "if"           w)
  )
