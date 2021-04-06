{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parser where

import           Control.Applicative        hiding (many, some) -- Prefer functions from Megaparsec
import           Control.Monad
import           Data.Text                  (Text)
import           Data.Void
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex
import           Types

type Parser = Parsec Void Text

spaceC :: Parser ()
spaceC = Lex.space space1 empty empty

symbol :: Text -> Parser Text
symbol = Lex.symbol spaceC

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme spaceC

-- | Parse input into Operator.
pOperator :: Parser Operator
pOperator = choice
    [ OAdd     <$ string "+",
      OSub     <$ string "-",
      OMul     <$ string "*",
      ODiv     <$ string "/",
      ODivI    <$ string "div",
      OGreater <$ string ">",
      OLess    <$ string "<",
      OEqual   <$ string "==",
      OAnd     <$ string "&&",
      OOr      <$ string "||",
      ONot     <$ string "not"
    ]

-- | Parse a quoted string from input.
pString :: Parser String
pString = do
  _ <- char '"'
  manyTill Lex.charLiteral (char '"')

-- | Parse a list of format [x, y, z] where x, y, and z are elements, from input.
pList :: Parser [Value]
pList = undefined

pValue :: Parser Value
pValue = undefined

-- pSequence :: Parser Sequence
-- pSequence = undefined
