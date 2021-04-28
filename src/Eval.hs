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
    deriving (Show, Eq)

-- | Top level evaluation function for any parsed program.
-- TODO: Drag the IO monad in here and implement handling of Read and Print.
eval :: Sequence -> Either String Value
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
      Right xs       -> Left $ "Multiple return values: " ++ show xs
      Left (Err err) -> Left err
      _              -> Left "Failed to evaluate somehow"

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
evalBuiltin BDup          st = evalBDup st
evalBuiltin BSwp          st = evalBSwp st
evalBuiltin BPop          st = evalBPop st
evalBuiltin BParseInteger st = evalBParseInteger st
evalBuiltin BParseFloat   st = evalBParseFloat st
evalBuiltin BWords        st = evalBWords st
evalBuiltin BHead         st = evalBHead st
evalBuiltin BTail         st = evalBTail st
evalBuiltin BEmpty        st = evalBEmpty st
evalBuiltin BLength       st = evalBLength st
evalBuiltin BCons         st = evalBCons st
evalBuiltin BAppend       st = evalBAppend st
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
-- Right [VInt 1,VInt 1]
evalBDup :: Stack -> Either IOResult Stack
evalBDup (top:tail) = return $ top : top : tail
evalBDup _ = Left $ Err "Tried to evaluate `dup` with missing or incorrect parameters"

-- | Swap the top elements of stack.
--
-- >>> evalBSwp [VInt 1, VInt 2]
-- Right [VInt 2,VInt 1]
evalBSwp :: Stack -> Either IOResult Stack
evalBSwp (a:b:st) = return $ b:a:st
evalBSwp _ = Left $ Err "Tried to evaluate `swp` with missing or incorrect parameters"

-- | Pop the top element off stack.
--
-- >>> evalBPop [VInt 1, VInt 2]
-- Right [VInt 2]
evalBPop :: Stack -> Either IOResult Stack
evalBPop (_:st) = return st
evalBPop _ = Left $ Err "Tried to evaluate `pop` with missing or incorrect parameters"

-- Stack operations ---------------------------------------------------------------------------

-- | Parse an integer from a string on top of the stack.
--
-- >>> evalBParseInteger [VString "12"]
-- Right [VInt 12]
evalBParseInteger :: Stack -> Either IOResult Stack
evalBParseInteger ((VString s):st) = return $ VInt (read s) : st
evalBParseInteger _ = Left $ Err "Tried to evaluate `parseInteger` with missing or incorrect parameters"

-- | Parse a floating point from a string on top of the stack.
--
-- >>> evalBParseFloat [VString "2.4"]
-- Right [VFloat 2.4]
evalBParseFloat :: Stack -> Either IOResult Stack
evalBParseFloat ((VString s):st) = return $ VFloat (read s) : st
evalBParseFloat _ = Left $ Err "Tried to evaluate `parseFloat` with missing or incorrect parameters"

-- | Split a string into a list of words contained in that string, from the top of the stack.
--
-- >>> evalBWords [VString "one two three"]
-- Right [VList [VString "one",VString "two",VString "three"]]
evalBWords :: Stack -> Either IOResult Stack
evalBWords ((VString s):st) = return $ VList (map VString (words s)) : st
evalBWords _ = Left $ Err "Tried to evaluate `words` with missing or incorrect parameters"

-- List operations ----------------------------------------------------------------------------
-- Some list operations are implemented on strings as well,
-- but not everything makes sens, since we do not have a Char type.

-- | Get the head of the list on top of the stack
--
-- >>> evalBHead [VList [VInt 1, VInt 2 ,VInt 3]]
-- Right [VInt 1]
evalBHead :: Stack -> Either IOResult Stack
evalBHead ((VList xs):st) = return $ head xs:st
evalBHead _ = Left $ Err "Tried to evaluate `head` with missing or incorrect parameters"

-- | Get the tail of the list on top of the stack
--
-- >>> evalBTail [VList [VInt 1, VInt 2, VInt 3]]
-- Right [VList [VInt 2,VInt 3]]
evalBTail :: Stack -> Either IOResult Stack
evalBTail ((VList xs):st)   = return $ VList (tail xs):st
evalBTail ((VString xs):st) = return $ VString (tail xs):st
evalBTail _ = Left $ Err "Tried to evaluate `tail` with missing or incorrect parameters"

-- | Check if the list on top of the stack is empty.
--
-- >>> evalBEmpty [VList [VInt 1]]
-- Right [VBool False]
--
-- >>> evalBEmpty [VList []]
-- Right [VBool True]
evalBEmpty :: Stack -> Either IOResult Stack
evalBEmpty ((VList xs):st)   = return $ VBool (null xs):st
evalBEmpty ((VString xs):st) = return $ VBool (null xs):st
evalBEmpty _ = Left $ Err "Tried to evaluate `empty` with missing or incorrect parameters"

-- | Get the length of the list on top of the stack.
--
-- >>> evalBLength [VList [VInt 1, VInt 2]]
-- Right [VInt 2]
evalBLength :: Stack -> Either IOResult Stack
evalBLength ((VList xs):st)      = return $ VInt (length xs):st
evalBLength ((VString xs):st)    = return $ VInt (length xs):st
evalBLength ((VQuotation xs):st) = return $ VInt (length xs):st -- How this makes any sense is beyond me
evalBLength _ = Left $ Err "Tried to evaluate `length` with missing or incorrect parameters"

-- | Cons the top element of stack onto the the list that is the second element of the stack.
--
-- >>> evalBCons [VList  [VInt 2, VInt 3], VInt 1]
-- Right [VList [VInt 1,VInt 2,VInt 3]]
evalBCons :: Stack -> Either IOResult Stack
evalBCons ((VList xs):x:st) = return $ VList (x:xs):st
evalBCons _ = Left $ Err "Tried to evaluate `cons` with missing or incorrect parameters"

-- | Append the list on top of the stack onto the list second stack
--
-- >>> evalBAppend [VList [VInt 3, VInt 4], (VList [VInt 1, VInt 2])]
-- Right [VList [VInt 1,VInt 2,VInt 3,VInt 4]]
evalBAppend :: Stack -> Either IOResult Stack
evalBAppend ((VList xs):(VList ys):st)     = return $ VList (ys ++ xs):st
evalBAppend ((VString xs):(VString ys):st) = return $ VString (ys ++ xs):st
evalBAppend _ = Left $ Err "Tried to evaluate `append` with missing or incorrect parameters"

-- Quotation operations -----------------------------------------------------------------------

-- | Evaluate a quotation in the context of a stack.
-- Evaluating anything that is not a quotation as a quotation just results in that value being put back onto the stack.
evalQuotation :: Value -> Stack -> Either IOResult Stack
evalQuotation (VQuotation inst) st = eval'' inst st
evalQuotation v st                 = return $ v:st

evalBExec :: Stack -> Either IOResult Stack
evalBExec (q:st) = evalQuotation q st
evalBExec _ = Left $ Err "Tried to evaluate `exec` but did not find a quotation on the stack"

evalBTimes :: Stack -> Either IOResult Stack
evalBTimes ((VInt 0):_:st) = return st
evalBTimes ((VInt n):q:st) = do
  res <- evalQuotation q st
  evalBTimes (VInt (n-1) : q : res)

evalBTimes _ = Left $ Err "Tried to evaluate `times` but did not find the right values on the stack"

-- FIXME: This is just terrible.
evalBMap :: Stack -> Either IOResult Stack
evalBMap (q:VList xs:st) =
  return $ VList xs' : st
  where
    xs' = map f xs

    f :: Value -> Value
    f v = case evalQuotation q (v:st) of
            Right (v':_)   -> v'
            Left (Err err) -> error err
            _              -> error "Failed to evaluate map"

evalBMap _ = Left $ Err "Tried to evaluate `map` with missing or incorrect parameters"

evalBFoldl :: Stack -> Either IOResult Stack
evalBFoldl (q:acc:VList xs:st) = do
  res <- foldl f (return acc) xs
  return (res:st)
  where
    f :: Either IOResult Value -> Value -> Either IOResult Value
    f b a = do
      b' <- b
      x <- evalQuotation q (a:b':st)
      return (head x)

evalBFoldl _ = Left $ Err "Tried to evaluate `foldl` with missing or incorrect parameters"

evalBEach :: Stack -> Either IOResult Stack
evalBEach (_:VList []:st) = return st
evalBEach (q:VList (x:xs):st) = do
  res <- evalQuotation q (x:st)
  evalBEach (q:VList xs:res)

evalBEach _ = Left $ Err "Tried to evaluate `each` with missing or incorrect parameters"

evalBIf :: Stack -> Either IOResult Stack
evalBIf (elseq:thenq:(VBool cond):st)
  = evalQuotation (if cond then thenq else elseq) st

evalBIf _ = Left $ Err "Tried to evaluate `if`, but could not find all three of `condition`, `if branch`, and `else branch`, or they were not in the right order"
