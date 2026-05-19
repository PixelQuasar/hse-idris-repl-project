module Parser

import Data.String
import Types
import Tokenize
import Syntax

%default covering

public export
parseExpr : Maybe Double -> String -> Result Expr
parseExpr ans input = do
  tokens <- tokenizeChars (unpack input)
  tokens' <- case tokens of
    (TOp _ :: _) => case ans of
      Just v  => Ok (TNumber v :: tokens)
      Nothing => Err "No previous result"
    _ => Ok tokens
  (expr, rest) <- parseTokens tokens'
  case rest of
    [] => Ok expr
    _  => Err "Unexpected trailing input"

public export
parseCommand : Maybe Double -> String -> Result Command
parseCommand ans input =
  case trim input of
    ":q"  => Ok Quit
    value => map Evaluate (parseExpr ans value)
