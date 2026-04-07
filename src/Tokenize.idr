module Tokenize

import Data.String
import Data.Maybe
import Types

%default covering

isOperatorChar : Char -> Bool
isOperatorChar ch = isJust (lookup ch operatorTable)

export
tokenizeChars : List Char -> Result (List Token)
tokenizeChars [] = Ok []
tokenizeChars (c :: cs) =
  if isSpace c then
    tokenizeChars cs
  else
    case lookup c operatorTable of
      Just tok => map (tok ::) (tokenizeChars cs)
      Nothing =>
        let (numChars, rest) = span (\ch => not (isSpace ch || isOperatorChar ch)) (c :: cs)
        in case parseDouble (pack numChars) of
             Just value => map (TNumber value ::) (tokenizeChars rest)
             Nothing => Err ("Unexpected character: " ++ singleton c)
