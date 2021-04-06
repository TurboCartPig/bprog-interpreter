module Types
  (Stack, Token, Operator (OAdd, OSub, OMul, ODiv, ODivI, OGreater, OLess, OEqual, OAnd, OOr, ONot), Value)
    where

type Stack = [Value]

type Sequence = [Token]

data Token
  = Op Operator
  | Val Value
    deriving(Show, Eq)

-- | Binary or unary operator, acting on the top elements of the stack.
data Operator
  = OAdd
  | OSub
  | OMul
  | ODiv
  | ODivI
  | OGreater
  | OLess
  | OEqual
  | OAnd
  | OOr
  | ONot
    deriving(Show, Eq)

data Value
  -- | A integer number.
  = VInt Int
  -- | A floating point number.
  | VFloat Float
  -- | A string.
  | VString String
  -- | A list of tokens.
  | VList [Token]
    deriving(Show, Eq)
