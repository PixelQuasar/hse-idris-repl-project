module Main

import Repl

%default covering

main : IO ()
main = do
  putStrLn "Idris REPL calculator"
  putStrLn "Type :q to exit"  
  repl
