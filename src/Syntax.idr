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
parseTokens xs = case parseLevel levels xs (sizeAccessible xs) of
  Err e => Err e
  Ok (expr, (rest ** _)) => Ok (expr, rest)

parseAtom xs (Access rec) = case xs of
  TNumber value :: rest => Ok (Number value, (rest ** reflexive))
  TLParen :: rest =>
    case parseLevel levels rest (rec rest reflexive) of
      Err e => Err e
      Ok (expr, (rest' ** prfRest')) => case rest' of
        TRParen :: rest'' =>
          Ok (expr, (rest'' ** lteSuccRight (lteSuccLeft prfRest')))
        _ => Err "Expected ')'"
  _ => Err "Expected number or '('"

parseLevel []              xs acc          = parseAtom xs acc
parseLevel (isOp :: inner) xs (Access rec) =
  case parseLevel inner xs (Access rec) of
    Err e => Err e
    Ok (left, (after ** afterLT)) =>
      case parseBinOpTail isOp inner left after (rec after afterLT) of
        Err e => Err e
        Ok (expr, (final ** finalLTE)) =>
          Ok (expr, (final ** transitive (LTESucc finalLTE) afterLT))

parseBinOpTail isOp inner left xs (Access rec) = case xs of
  TOp op :: tokens =>
    if isOp op then
      case parseLevel inner tokens (rec tokens reflexive) of
        Err e => Err e
        Ok (right, (after ** afterLT)) =>
          case parseBinOpTail isOp inner (BinOp op left right) after
                              (rec after (lteSuccRight afterLT)) of
            Err e => Err e
            Ok (result, (final ** finalLTE)) =>
              Ok (result, (final ** lteSuccRight (transitive finalLTE (lteSuccLeft afterLT))))
    else Ok (left, (TOp op :: tokens ** reflexive))
  _ => Ok (left, (xs ** reflexive))
