module Repl

import Parser
import Eval
import Types

%default covering

export
repl : IO ()
repl = do
  putStr "> "
  line <- getLine
  case parseCommand line of
    Ok Quit => putStrLn "Bye!"
    Ok (Evaluate expr) =>
      case eval expr of
        Ok value => do
          putStrLn (show value)
          repl
        Err err => do
          putStrLn err
          repl
    Err err => do
      putStrLn err
      repl
