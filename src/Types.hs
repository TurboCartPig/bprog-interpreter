-- | The Types module defines the types of all the various types of tokens the program can parse.
module Types (
  Stack, Sequence,
  Token (Val, Bi, Op),
  Operator (OAssign, OFun, OAdd, OSub, OMul, ODiv, ODivI, OGreater, OLess, OEqual, OAnd, OOr, ONot),
  Builtin (BDup, BSwp, BPop, BRead, BPrint, BParseInteger, BParseFloat, BWords, BHead, BTail, BEmpty, BLength, BCons, BAppend, BExec, BTimes, BMap, BFoldl, BEach, BIf),
  Value (VInt, VFloat, VBool, VString, VList, VQuotation, VSymbol)
) where

-- | A stack of values that represents the internal state of the program.
type Stack = [Value]

-- | A Sequence of tokens. This can either be an entire program or a quotation.
type Sequence = [Token]

-- | A top level token that the user can write as part of a bprog program.
data Token
  = Op Operator
  | Bi Builtin
  | Val Value
    deriving(Show, Eq)

-- | Binary or unary operator, acting on the top elements of the stack.
data Operator
  = OAssign
  | OFun
  | OAdd
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

-- | Builtin operation that can be applied to some value.
data Builtin
  -- Stack operations
  = BDup
  | BSwp
  | BPop
  -- IO operations
  | BRead
  | BPrint
  -- Sting parsing operations
  | BParseInteger
  | BParseFloat
  | BWords
  -- List operations
  | BHead
  | BTail
  | BEmpty
  | BLength
  | BCons
  | BAppend
  -- Quotation operations
  | BExec
  | BTimes
  | BMap
  | BFoldl
  | BEach
  | BIf
    deriving(Show, Eq)

-- | A Value that can be placed on the stack.
data Value
  -- | A integer number.
  = VInt Int
  -- | A floating point number.
  | VFloat Float
  -- | A boolean.
  | VBool Bool
  -- | A string.
  | VString String
  -- | A list of tokens.
  | VList [Value]
  -- | A quotation (or codeblock), containing a sequence of program tokens ready for evaluation.
  | VQuotation Sequence
  -- | A symbol name. This will evaluate to whatever is bound to the symbol in the global symbol table.
  | VSymbol String
    deriving(Show, Eq)
