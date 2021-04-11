module Types
  (Stack, Token, Operator (OAdd, OSub, OMul, ODiv, ODivI, OGreater, OLess, OEqual, OAnd, OOr, ONot), Value (VInt, VFloat, VString))
    where

type Stack = [Value]

type Sequence = [Token]

-- | A top level token that the user can write as part of a bprog program.
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

-- | A Value that can be placed on the stack.
data Value
  -- | A integer number.
  = VInt Int
  -- | A floating point number.
  | VFloat Float
  -- | A string.
  | VString String
  -- | A list of tokens.
  | VList [Value]
    deriving(Show, Eq)
