module Eval

import Types

%default total

applyOp : Op -> Double -> Double -> Result Double
applyOp OpDiv _ 0.0 = Err "Division by zero"
applyOp OpDiv x y = Ok (x / y)
applyOp OpAdd x y = Ok (x + y)
applyOp OpSub x y = Ok (x - y)
applyOp OpMul x y = Ok (x * y)

export
eval : Expr -> Result Double
eval (Number value) = Ok value
eval (BinOp op left right) = do
  l <- eval left
  r <- eval right
  applyOp op l r
