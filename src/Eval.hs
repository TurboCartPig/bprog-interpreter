-- | The Eval module defines how a parsed program is evaluated to produce one single value.
module Eval (
  eval
) where

import           Parser
import           Types

-- | Top level evaluation function for any parsed program.
eval :: Sequence -> Maybe Value
eval tokens =
  let
    -- Initial stack before evaluation
    initialSt = [] :: Stack
    -- Final stack after evaluation
    finalSt = eval'' tokens initialSt
  in
    -- If there is exactly one element on the stack,
    -- then we are successful, and return the element, otherwise we have failed
    if length finalSt == 1 then return . head  $ finalSt else Nothing

-- | Evaluate one token from the top of the stack.
eval' :: Stack -> Token -> Stack
eval' st token =
  case token of
    -- Push the new value on top of the stack
    Val v -> v:st
    -- Evaluate the result of applying a builtin to the stack
    Bi bi -> evalBuiltin bi st
    -- Evaluate new value, and push it onto the stack
    Op op -> evalOperator op st

-- | Evaluate all the tokens in a sequence in the context of a stack.
eval'' :: Sequence -> Stack -> Stack
eval'' seq st = foldl eval' st seq

-- | Evaluate a binary operation on two values, with specific types.
evalBinaryOp' :: Value -> Value -> Operator -> Value
evalBinaryOp' (VInt a)   (VInt b)   OAdd     = VInt   $ a + b
evalBinaryOp' (VInt a)   (VInt b)   OSub     = VInt   $ a - b
evalBinaryOp' (VInt a)   (VInt b)   OMul     = VInt   $ a * b
evalBinaryOp' (VInt a)   (VInt b)   ODiv     = VFloat $ fromIntegral a / fromIntegral b
evalBinaryOp' (VInt a)   (VInt b)   ODivI    = VInt   $ a `div` b
evalBinaryOp' (VInt a)   (VInt b)   OGreater = VBool  $ a > b
evalBinaryOp' (VInt a)   (VInt b)   OLess    = VBool  $ a < b
evalBinaryOp' (VFloat a) (VFloat b) OAdd     = VFloat $ a + b
evalBinaryOp' (VFloat a) (VFloat b) OSub     = VFloat $ a - b
evalBinaryOp' (VFloat a) (VFloat b) OMul     = VFloat $ a * b
evalBinaryOp' (VFloat a) (VFloat b) ODiv     = VFloat $ a / b
evalBinaryOp' (VFloat a) (VFloat b) ODivI    = VInt   $ floor a `div` floor b
evalBinaryOp' (VFloat a) (VFloat b) OGreater = VBool  $ a > b
evalBinaryOp' (VFloat a) (VFloat b) OLess    = VBool  $ a < b
evalBinaryOp' (VBool a)  (VBool b)  OAnd     = VBool  $ a && b
evalBinaryOp' (VBool a)  (VBool b)  OOr      = VBool  $ a || b
evalBinaryOp' a                 b   OEqual   = VBool  $ a == b
evalBinaryOp' _ _ _ = error "Tried to apply binary operator to incompatible operands"

-- | Evaluate a binary operation on two values.
-- Coerces ints into floats, but disallows all other coercions.
evalBinaryOp :: Value -> Value -> Operator -> Value
-- Normal integer operation.
evalBinaryOp (VInt a) (VInt b) op =
  evalBinaryOp' (VInt a) (VInt b) op
-- Normal floating point operation.
evalBinaryOp (VFloat a) (VFloat b) op =
  evalBinaryOp' (VFloat a) (VFloat b) op
-- Mixed integer and floating point operation results
-- in converting integer into floating point and doing floating point operation.
evalBinaryOp (VInt a) (VFloat b) op =
  evalBinaryOp' (VFloat . fromIntegral $ a) (VFloat b) op
-- Same as previous branch.
evalBinaryOp (VFloat a) (VInt b) op =
  evalBinaryOp' (VFloat a) (VFloat . fromIntegral $ b) op
-- If no coercion rules are defined, simply try to perform the operation.
evalBinaryOp a b op =
  evalBinaryOp' a b op

-- | Evaluate a unary operation on a value.
-- Currently not is the only unary operator.
evalUnaryOp :: Value -> Operator -> Value
evalUnaryOp (VInt v)   ONot = VInt (-v)
evalUnaryOp (VFloat v) ONot = VFloat (-v)
evalUnaryOp (VBool v)  ONot = VBool (not v)
evalUnaryOp _ _ = error "Tried to evaluate an unary operator on unsupported data type or with no-unary operator"

-- | Evaluate an operator by deciding if it is binary or unary and calling the right function.
evalOperator :: Operator -> Stack -> Stack
evalOperator op st
  | op `elem` [OAdd, OSub, OMul, ODiv, ODivI, OGreater, OLess, OEqual, OAnd, OOr]
          = let (a:b:st') = st in evalBinaryOp b a op : st'
  | op == ONot
          = let (a:st') = st in evalUnaryOp a op : st'
  | otherwise = error "Operator not implemented"

-- | Evaluate a builtin operation onto the stack.
evalBuiltin :: Builtin -> Stack -> Stack
evalBuiltin BDup          = evalBDup
evalBuiltin BSwp          = evalBSwp
evalBuiltin BPop          = evalBPop
evalBuiltin BParseInteger = evalBParseInteger
evalBuiltin BParseFloat   = evalBParseFloat
evalBuiltin BWords        = evalBWords
evalBuiltin BHead         = evalBHead
evalBuiltin BTail         = evalBTail
evalBuiltin BEmpty        = evalBEmpty
evalBuiltin BLength       = evalBLength
evalBuiltin BCons         = evalBCons
evalBuiltin BAppend       = evalBAppend
evalBuiltin BExec         = evalBExec
evalBuiltin BTimes        = evalBTimes
evalBuiltin BMap          = evalBMap
evalBuiltin BFoldl        = evalBFoldl
evalBuiltin BEach         = evalBEach
evalBuiltin BIf           = evalBIf
evalBuiltin _             = error "Tried to evaluate an unsupported builtin"

-- The following builtins will simply terminate the execution of the interpreter if they are called on an invalid stack

-- Stack operations ---------------------------------------------------------------------------

-- | Duplicate the top element of stack.
--
-- >>> evalBDup [VInt 1]
-- [VInt 1,VInt 1]
evalBDup :: Stack -> Stack
evalBDup (top:tail) = top : top : tail

-- | Swap the top elements of stack.
--
-- >>> evalBSwp [VInt 1, VInt 2]
-- [VInt 2,VInt 1]
evalBSwp :: Stack -> Stack
evalBSwp (a:b:st) = b:a:st

-- | Pop the top element off stack.
--
-- >>> evalBPop [VInt 1, VInt 2]
-- [VInt 2]
evalBPop :: Stack -> Stack
evalBPop (_:st) = st

-- Stack operations ---------------------------------------------------------------------------

-- | Parse an integer from a string on top of the stack.
--
-- >>> evalBParseInteger [VString "12"]
-- [VInt 12]
evalBParseInteger :: Stack -> Stack
evalBParseInteger ((VString s):st) = VInt (read s) : st

-- | Parse a floating point from a string on top of the stack.
--
-- >>> evalBParseFloat [VString "2.4"]
-- [VFloat 2.4]
evalBParseFloat :: Stack -> Stack
evalBParseFloat ((VString s):st) = VFloat (read s) : st

-- | Split a string into a list of words contained in that string, from the top of the stack.
--
-- >>> evalBWords [VString "one two three"]
-- [VList [VString "one",VString "two",VString "three"]]
evalBWords :: Stack -> Stack
evalBWords ((VString s):st) = VList (map VString (words s)) : st

-- List operations ----------------------------------------------------------------------------
-- Some list operations are implemented on strings as well,
-- but not everything makes sens, since we do not have a Char type.

-- | Get the head of the list on top of the stack
--
-- >>> evalBHead [VList [VInt 1, VInt 2 ,VInt 3]]
-- [VInt 1]
evalBHead :: Stack -> Stack
evalBHead ((VList xs):st) = head xs:st

-- | Get the tail of the list on top of the stack
--
-- >>> evalBTail [VList [VInt 1, VInt 2, VInt 3]]
-- [VList [VInt 2,VInt 3]]
evalBTail :: Stack -> Stack
evalBTail ((VList xs):st)   = VList (tail xs):st
evalBTail ((VString xs):st) = VString (tail xs):st

-- | Check if the list on top of the stack is empty.
--
-- >>> evalBEmpty [VList [VInt 1]]
-- [VBool False]
--
-- >>> evalBEmpty [VList []]
-- [VBool True]
evalBEmpty :: Stack -> Stack
evalBEmpty ((VList xs):st)   = VBool (null xs):st
evalBEmpty ((VString xs):st) = VBool (null xs):st

-- | Get the length of the list on top of the stack.
--
-- >>> evalBLength [VList [VInt 1, VInt 2]]
-- [VInt 2]
evalBLength :: Stack -> Stack
evalBLength ((VList xs):st)      = VInt (length xs):st
evalBLength ((VString xs):st)    = VInt (length xs):st
evalBLength ((VQuotation xs):st) = VInt (length xs):st -- How this makes any sense is beyond me

-- | Cons the top element of stack onto the the list that is the second element of the stack.
--
-- >>> evalBCons [VList  [VInt 2, VInt 3], VInt 1]
-- [VList [VInt 1,VInt 2,VInt 3]]
evalBCons :: Stack -> Stack
evalBCons ((VList xs):x:st) = VList (x:xs):st

-- | Append the list on top of the stack onto the list second stack
--
-- >>> evalBAppend [VList [VInt 3, VInt 4], (VList [VInt 1, VInt 2])]
-- [VList [VInt 1,VInt 2,VInt 3,VInt 4]]
evalBAppend :: Stack -> Stack
evalBAppend ((VList xs):(VList ys):st)     = VList (ys ++ xs):st
evalBAppend ((VString xs):(VString ys):st) = VString (ys ++ xs):st

-- Quotation operations -----------------------------------------------------------------------

evalQuotation :: Value -> Stack -> Stack
evalQuotation (VQuotation inst) = eval'' inst
evalQuotation _ = error "Tried to evaluate something that is not a quotation as a quotation"

evalBExec :: Stack -> Stack
evalBExec (q:st) = evalQuotation q st

evalBTimes :: Stack -> Stack
evalBTimes (_:(VInt 0):st) = st
evalBTimes (q:(VInt n):st) = q : VInt (n-1) : evalQuotation q st

evalBMap :: Stack -> Stack
evalBMap = undefined

evalBFoldl :: Stack -> Stack
evalBFoldl = undefined

evalBEach :: Stack -> Stack
evalBEach = undefined

evalBIf :: Stack -> Stack
evalBIf = undefined
