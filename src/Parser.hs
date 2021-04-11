{-# LANGUAGE TupleSections #-}
module Parser where

import           Control.Applicative hiding (many, some)
import           Control.Monad
import qualified Data.Char (isAlphaNum)
import           Text.Read (readMaybe)
import           Types

-- | Construct a Maybe from a char if it is equal to another char.
-- | This enables do notation further down.
char :: Char -> Char -> Maybe Char
char a b = if a == b then Just a else Nothing

-- | isAlphaNum for strings.
isAlphaNum :: String -> Bool
isAlphaNum = all Data.Char.isAlphaNum

-- | Check if a string contains only alphanumeric characters.
alphanumeric :: String -> Maybe String
alphanumeric s = if isAlphaNum s then Just s else Nothing

parse :: String -> [Value]
parse s = let
    ws = words s
  in
    parse' ws

parse' :: [String] -> [Value]
parse' [] = []
parse' ws = case parseValue ws of
              Nothing      -> error "Fuck"
              Just (x, xs) -> x:parse' xs

parseValue :: [String] -> Maybe (Value, [String])
parseValue ws = parseVInt ws <|> parseVFloat ws <|> parseVString ws <|> parseVList ws

-- | Parse an Int into a Value.
parseVInt :: [String] -> Maybe (Value, [String])
parseVInt []     = Nothing
parseVInt (w:ws) = (, ws) . VInt <$> readMaybe w

-- | Parse a Float into a Value.
parseVFloat :: [String] -> Maybe (Value, [String])
parseVFloat []     = Nothing
parseVFloat (w:ws) = (, ws) . VFloat <$> readMaybe w

-- | Parse a String into a Value.
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

parseVList :: [String] -> Maybe (Value, [String])
parseVList = undefined
