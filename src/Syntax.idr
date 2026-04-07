module Syntax

import Types

%default covering

mutual
  levels : List (Op -> Bool)
  levels = [isAddOp, isMulOp]

  export
  parseExprTokens : List Token -> Result (Expr, List Token)
  parseExprTokens = parseLevel levels

  parseLevel : List (Op -> Bool) -> List Token -> Result (Expr, List Token)
  parseLevel [] = parseAtom
  parseLevel (isOp :: rest) = \tokens => do
    (left, remaining) <- parseLevel rest tokens
    parseBinOpTail isOp (parseLevel rest) left remaining

  parseBinOpTail : (Op -> Bool)
                -> (List Token -> Result (Expr, List Token))
                -> Expr -> List Token -> Result (Expr, List Token)
  parseBinOpTail isOp parseOperand left (TOp op :: rest) =
    if isOp op then do
      (right, rest') <- parseOperand rest
      parseBinOpTail isOp parseOperand (BinOp op left right) rest'
    else
      Ok (left, TOp op :: rest)
  parseBinOpTail _ _ left rest = Ok (left, rest)

  parseAtom : List Token -> Result (Expr, List Token)
  parseAtom (TNumber value :: rest) = Ok (Number value, rest)
  parseAtom (TLParen :: rest) = do
    (expr, rest') <- parseExprTokens rest
    case rest' of
      TRParen :: rest'' => Ok (expr, rest'')
      _ => Err "Expected ')'"
  parseAtom _ = Err "Expected number or '('"
