-- | The Eval module defines how a parsed program is evaluated to produce one single value.
module Eval (
  eval
) where

import           Parser
import           Types

-- | Top level evaluation function for any parsed program.
eval :: [Token] -> Maybe Value
eval tokens =
  let
    -- Initial stack before evaluation
    initialS = [] :: Stack
    -- Final stack after evaluation
    finalS = foldl eval' initialS tokens
  in
    -- If there is exactly one element on the stack,
    -- then we are successful, and return the element, otherwise we have failed
    if length finalS == 1 then return . head  $ finalS else Nothing

-- | Evaluate one token from the top of the stack.
eval' :: Stack -> Token -> Stack
eval' st token = case token of
                   Val v -> v:st               -- Push the new value on top of the stack
                   Op op -> evalOperator st op -- Evaluate new value, and push it onto the stack

-- | Evaluate a binary operation on two values, with specific types.
evalBinaryOp' :: Value -> Value -> Operator -> Value
evalBinaryOp' (VInt a)   (VInt b)   OAdd     = VInt   $ a + b
evalBinaryOp' (VInt a)   (VInt b)   OSub     = VInt   $ a - b
evalBinaryOp' (VInt a)   (VInt b)   OMul     = VInt   $ a * b
evalBinaryOp' (VInt a)   (VInt b)   ODivI    = VInt   $ a `div` b
evalBinaryOp' (VInt a)   (VInt b)   OGreater = VBool  $ a > b
evalBinaryOp' (VInt a)   (VInt b)   OLess    = VBool  $ a < b
evalBinaryOp' (VInt a)   (VInt b)   OEqual   = VBool  $ a == b
evalBinaryOp' (VFloat a) (VFloat b) OAdd     = VFloat $ a + b
evalBinaryOp' (VFloat a) (VFloat b) OSub     = VFloat $ a - b
evalBinaryOp' (VFloat a) (VFloat b) OMul     = VFloat $ a * b
evalBinaryOp' (VFloat a) (VFloat b) ODiv     = VFloat $ a / b
evalBinaryOp' (VFloat a) (VFloat b) OGreater = VBool  $ a > b
evalBinaryOp' (VFloat a) (VFloat b) OLess    = VBool  $ a < b
evalBinaryOp' (VFloat a) (VFloat b) OEqual   = VBool  $ a == b
evalBinaryOp' (VBool a)  (VBool b)  OAnd     = VBool  $ a && b
evalBinaryOp' (VBool a)  (VBool b)  OOr      = VBool  $ a || b
evalBinaryOp' _ _ _ = error "Tried to apply binary operator to incompatible operands"

-- | Evaluate a binary operation on two values.
-- Coerces ints into floats, but disallows all other coercions
evalBinaryOp :: Value -> Value -> Operator -> Value
-- Implement special rule for floating point division on two integers
evalBinaryOp (VInt a)   (VInt b)   ODiv = evalBinaryOp' (VFloat . fromIntegral $ a) (VFloat . fromIntegral $ b) ODiv
-- Normal integer operation
evalBinaryOp (VInt a)   (VInt b)   op   = evalBinaryOp' (VInt a)                    (VInt b) op
-- Normal floating point operation
evalBinaryOp (VFloat a) (VFloat b) op   = evalBinaryOp' (VFloat a)                  (VFloat b) op
-- Mixed integer and floating point operation results
-- in converting integer into floating point and doing floating point operation
evalBinaryOp (VInt a)   (VFloat b) op   = evalBinaryOp' (VFloat . fromIntegral $ a) (VFloat b) op
-- Same as previous branch
evalBinaryOp (VFloat a) (VInt b)   op   = evalBinaryOp' (VFloat a)                  (VFloat . fromIntegral $ b) op
-- Normal boolean operation
evalBinaryOp (VBool a)  (VBool b)  op   = evalBinaryOp' (VBool a)                   (VBool b) op
-- Tried to performed illegal operation
evalBinaryOp _ _ _ = error "Fail to coerce the operands to any valid combination"

-- | Evaluate a unary operation on a value.
-- Currently not is the only unary operator.
evalUnaryOp :: Value -> Operator -> Value
evalUnaryOp (VInt v)   ONot = VInt (-v)
evalUnaryOp (VFloat v) ONot = VFloat (-v)
evalUnaryOp (VBool v)  ONot = VBool (not v)
evalUnaryOp _ _ = error "Tried to evaluate an unary operator on unsupported data type or with no-unary operator"

-- | Evaluate an operator by deciding if it is binary or unary and calling the right function.
evalOperator :: Stack -> Operator -> Stack
evalOperator st op
  | op `elem` [OAdd, OSub, OMul, ODiv, ODivI, OGreater, OLess, OEqual, OAnd, OOr]
          = let (a:b:st') = st in evalBinaryOp a b op : st'
  | op == ONot
          = let (a:st') = st in evalUnaryOp a op : st'
  | otherwise = error "Operator not implemented"
