module Repl

import Parser
import Eval
import Types

%default covering

export
repl : Maybe Double -> IO ()
repl ans = do
  putStr "> "
  line <- getLine
  case parseCommand ans line of
    Ok Quit => putStrLn "Bye!"
    Ok (Evaluate expr) =>
      case eval expr of
        Ok value => do
          printLn value
          repl (Just value)
        Err err => do
          putStrLn err
          repl ans
    Err err => do
      putStrLn err
      repl ans
