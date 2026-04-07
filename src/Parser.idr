module Parser

import Data.String
import Types
import Tokenize
import Syntax

%default covering

public export
parseExpr : String -> Result Expr
parseExpr input = do
  tokens <- tokenizeChars (unpack input)
  (expr, rest) <- parseExprTokens (tokens)
  case rest of
    [] => Ok expr
    _ => Err "Unexpected trailing input"

public export
parseCommand : String -> Result Command
parseCommand input =
  case trim input of
    ":q" => Ok Quit
    value => map Evaluate (parseExpr value)
