{-# LANGUAGE TupleSections #-}

-- | The Parser module defines a series of parser combinators that is composes
-- into high level parsers like the parse function.
module Parser (
  parse
) where

import           Control.Applicative hiding (many, some)
import           Control.Monad
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

-- | Construct a Maybe from a char if it is equal to another char.
-- This enables do notation further down.
char :: Char -> Char -> Maybe Char
char a b = if a == b then Just a else Nothing

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
parse' :: [String] -> Maybe [Token]
parse' [] = Just []
parse' ws = case parseOperator ws <|> parseBuiltin ws <|> parseValue ws of
              Nothing      -> Nothing
              Just (x, xs) -> (x :) <$> parse' xs

-- | Parses a list of words into a Value.
parseValue :: Parser Token
parseValue ws =
  first Val         <$> (
    parseVInt ws    <|>
    parseVFloat ws  <|>
    parseVBool ws   <|>
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
parseBuiltin (w:ws) =
  (, ws) . Bi <$> (
    (BDup     <$ string "dup"    w) <|>
    (BSwp     <$ string "swp"    w) <|>
    (BPop     <$ string "pop"    w) <|>
    (BHead    <$ string "head"   w) <|>
    (BTail    <$ string "tail"   w) <|>
    (BEmpty   <$ string "empty"  w) <|>
    (BCons    <$ string "length" w) <|>
    (BAppend  <$ string "cons"   w)
  )
