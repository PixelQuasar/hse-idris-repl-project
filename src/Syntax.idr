module Syntax

import Control.WellFounded
import Data.Nat
import Types

%default total

ParseLT : List Token -> Type
ParseLT xs = Result (Expr, (rest : List Token ** length rest `LT` length xs))

ParseLTE : List Token -> Type
ParseLTE xs = Result (Expr, (rest : List Token ** length rest `LTE` length xs))

levels : List (Op -> Bool)
levels = [isAddOp, isMulOp]

parseAtom : (xs : List Token) -> SizeAccessible xs -> ParseLT xs

parseLevel : List (Op -> Bool) -> (xs : List Token) -> SizeAccessible xs -> ParseLT xs

parseBinOpTail : (Op -> Bool) -> List (Op -> Bool) -> Expr
              -> (xs : List Token) -> SizeAccessible xs -> ParseLTE xs

export
parseTokens : List Token -> Result (Expr, List Token)
parseTokens xs = do
  (expr, (rest ** _)) <- parseLevel levels xs (sizeAccessible xs)
  Ok (expr, rest)

parseAtom xs (Access rec) = case xs of
  TNumber value :: rest => Ok (Number value, (rest ** reflexive))
  TLParen :: rest => do
    (expr, (rest' ** prfRest')) <- parseLevel levels rest (rec rest reflexive)
    case rest' of
      TRParen :: rest'' =>
        Ok (expr, (rest'' ** lteSuccRight (lteSuccLeft prfRest')))
      _ => Err "Expected ')'"
  _ => Err "Expected number or '('"

parseLevel []              xs acc          = parseAtom xs acc
parseLevel (isOp :: inner) xs (Access rec) = do
  (left, (after ** afterLT)) <- parseLevel inner xs (Access rec)
  (expr, (final ** finalLTE)) <- parseBinOpTail isOp inner left after (rec after afterLT)
  Ok (expr, (final ** transitive (LTESucc finalLTE) afterLT))

parseBinOpTail isOp inner left xs (Access rec) = case xs of
  TOp op :: tokens =>
    if isOp op then do
      (right, (after ** afterLT)) <- parseLevel inner tokens (rec tokens reflexive)
      (result, (final ** finalLTE)) <- parseBinOpTail isOp inner (BinOp op left right) after
                                                       (rec after (lteSuccRight afterLT))
      Ok (result, (final ** lteSuccRight (transitive finalLTE (lteSuccLeft afterLT))))
    else Ok (left, (TOp op :: tokens ** reflexive))
  _ => Ok (left, (xs ** reflexive))
