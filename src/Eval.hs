-- | The Eval module defines how a parsed program is evaluated to produce one single value.
module Eval (
  eval
) where

import           Parser
import           Types

-- | A type given to the IO aware part of the program,
-- so that it can perform certain operations, and then continue,
-- or throw an error, and terminate.
data IOResult
  -- | The program encountered a fatal error, and will terminate.
  = Err String
  -- | The program should read user input from stdin, and then resume with the input placed on top of the stack.
  | Read Stack Sequence
  -- | The program should print a string to stdout, and then resume the evaluation of the program with what is left of the stack and sequence.
  | Print Stack Sequence String

-- | Top level evaluation function for any parsed program.
-- TODO: Drag the IO monad in here and implement handling of Read and Print.
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
    case finalSt of
      Right [x]      -> return x
      Right xs       -> error $ "Multiple return values: " ++ show xs
      Left (Err err) -> error err
      _              -> error "Failed to evaluate somehow"

-- | Evaluate one token from the top of the stack.
eval' :: Stack -> Token -> Either IOResult Stack
eval' st token =
  case token of
    -- Push the new value on top of the stack
    Val v -> return (v:st)
    -- Evaluate the result of applying a builtin to the stack
    Bi bi -> evalBuiltin bi st
    -- Evaluate new value, and push it onto the stack
    Op op -> evalOperator op st

-- | Evaluate all the tokens in a sequence in the context of a stack.
eval'' :: Sequence -> Stack -> Either IOResult Stack
eval'' seq st = foldl (\b a -> b >>= (`eval'` a)) (return st) seq

-- | Evaluate a binary operation on two values, with specific types.
evalBinaryOp' :: Value -> Value -> Operator -> Either IOResult Value
evalBinaryOp' (VInt a)   (VInt b)   OAdd     = return . VInt   $ a + b
evalBinaryOp' (VInt a)   (VInt b)   OSub     = return . VInt   $ a - b
evalBinaryOp' (VInt a)   (VInt b)   OMul     = return . VInt   $ a * b
evalBinaryOp' (VInt a)   (VInt b)   ODiv     = return . VFloat $ fromIntegral a / fromIntegral b
evalBinaryOp' (VInt a)   (VInt b)   ODivI    = return . VInt   $ a `div` b
evalBinaryOp' (VInt a)   (VInt b)   OGreater = return . VBool  $ a > b
evalBinaryOp' (VInt a)   (VInt b)   OLess    = return . VBool  $ a < b
evalBinaryOp' (VFloat a) (VFloat b) OAdd     = return . VFloat $ a + b
evalBinaryOp' (VFloat a) (VFloat b) OSub     = return . VFloat $ a - b
evalBinaryOp' (VFloat a) (VFloat b) OMul     = return . VFloat $ a * b
evalBinaryOp' (VFloat a) (VFloat b) ODiv     = return . VFloat $ a / b
evalBinaryOp' (VFloat a) (VFloat b) ODivI    = return . VInt   $ floor a `div` floor b
evalBinaryOp' (VFloat a) (VFloat b) OGreater = return . VBool  $ a > b
evalBinaryOp' (VFloat a) (VFloat b) OLess    = return . VBool  $ a < b
evalBinaryOp' (VBool a)  (VBool b)  OAnd     = return . VBool  $ a && b
evalBinaryOp' (VBool a)  (VBool b)  OOr      = return . VBool  $ a || b
evalBinaryOp' a                 b   OEqual   = return . VBool  $ a == b
evalBinaryOp' _ _ _ = Left $ Err "Tried to apply binary operator to incompatible operands"

-- | Evaluate a binary operation on two values.
-- Coerces ints into floats, but disallows all other coercions.
evalBinaryOp :: Value -> Value -> Operator -> Either IOResult Value
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
evalUnaryOp :: Value -> Operator -> Either IOResult Value
evalUnaryOp (VInt v)   ONot = return . VInt $ (-v)
evalUnaryOp (VFloat v) ONot = return . VFloat $ (-v)
evalUnaryOp (VBool v)  ONot = return . VBool $ not v
evalUnaryOp _ _ = Left $ Err "Tried to evaluate an unary operator on unsupported data type or with no-unary operator"

-- | Evaluate an operator by deciding if it is binary or unary and calling the right function.
evalOperator :: Operator -> Stack -> Either IOResult Stack
evalOperator op st
  | op `elem` [OAdd, OSub, OMul, ODiv, ODivI, OGreater, OLess, OEqual, OAnd, OOr]
    = do
      let (a:b:st') = st
      res <- evalBinaryOp b a op
      return (res:st')
  | op == ONot
    = do
      let (a:st') = st
      res <- evalUnaryOp a op
      return (res:st')
  | otherwise = Left $ Err "Operator not implemented"

-- | Evaluate a builtin operation onto the stack.
evalBuiltin :: Builtin -> Stack -> Either IOResult Stack
evalBuiltin BDup          st = return . evalBDup $ st
evalBuiltin BSwp          st = return . evalBSwp $ st
evalBuiltin BPop          st = return . evalBPop $ st
evalBuiltin BParseInteger st = return . evalBParseInteger $ st
evalBuiltin BParseFloat   st = return . evalBParseFloat $ st
evalBuiltin BWords        st = return . evalBWords $ st
evalBuiltin BHead         st = return . evalBHead $ st
evalBuiltin BTail         st = return . evalBTail $ st
evalBuiltin BEmpty        st = return . evalBEmpty $ st
evalBuiltin BLength       st = return . evalBLength $ st
evalBuiltin BCons         st = return . evalBCons $ st
evalBuiltin BAppend       st = return . evalBAppend $ st
evalBuiltin BExec         st = evalBExec st
evalBuiltin BTimes        st = evalBTimes st
evalBuiltin BMap          st = evalBMap st
evalBuiltin BFoldl        st = evalBFoldl st
evalBuiltin BEach         st = evalBEach st
evalBuiltin BIf           st = evalBIf st
evalBuiltin _ _ = Left $ Err "Tried to evaluate an unsupported builtin"

-- The following builtins will simply terminate the execution of the interpreter if they are called on an invalid stack
-- TODO: Make them return appropriate error messages instead of throwing.

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

-- | Evaluate a quotation in the context of a stack.
-- Evaluating anything that is not a quotation as a quotation just results in that value being put back onto the stack.
evalQuotation :: Value -> Stack -> Either IOResult Stack
evalQuotation (VQuotation inst) st = eval'' inst st
evalQuotation v st = return $ v:st

evalBExec :: Stack -> Either IOResult Stack
evalBExec (q:st) = evalQuotation q st
evalBExec _ = Left $ Err "Tried to evaluate `exec` but did not find a quotation on the stack"

evalBTimes :: Stack -> Either IOResult Stack
evalBTimes ((VInt 0):_:st) = return st
evalBTimes ((VInt n):q:st) = do
  res <- evalQuotation q st
  evalBTimes (VInt (n-1) : q : res)
evalBTimes _ = Left $ Err "Tried to evaluate `times` but did not find the right values on the stack"

evalBMap :: Stack -> Either IOResult Stack
evalBMap = undefined

evalBFoldl :: Stack -> Either IOResult Stack
evalBFoldl = undefined

evalBEach :: Stack -> Either IOResult Stack
evalBEach = undefined

evalBIf :: Stack -> Either IOResult Stack
evalBIf (elseq:thenq:(VBool cond):st)
  = evalQuotation (if cond then thenq else elseq) st
evalBIf _ = Left $ Err "Tried to evaluate `if`, but could not find all three of `condition`, `if branch`, and `else branch`, or they were not in the right order"
