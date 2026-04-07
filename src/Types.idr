module Types

%default total

public export
data Op = OpAdd | OpSub | OpMul | OpDiv

public export
data Expr
  = Number Double
  | BinOp Op Expr Expr

public export
data Command
  = Evaluate Expr
  | Quit

public export
data Token
  = TNumber Double
  | TOp Op
  | TLParen
  | TRParen

public export
operatorTable : List (Char, Token)
operatorTable =
  [ ('+', TOp OpAdd)
  , ('-', TOp OpSub)
  , ('*', TOp OpMul)
  , ('/', TOp OpDiv)
  , ('(', TLParen)
  , (')', TRParen)
  ]

public export
isAddOp : Op -> Bool
isAddOp OpAdd = True
isAddOp OpSub = True
isAddOp _     = False

public export
isMulOp : Op -> Bool
isMulOp OpMul = True
isMulOp OpDiv = True
isMulOp _     = False

public export
data Result a
  = Ok a
  | Err String


public export
Functor Result where
  map f (Ok x)  = Ok (f x)
  map f (Err e) = Err e

public export
Applicative Result where
  pure = Ok
  (Ok f)  <*> (Ok x)  = Ok (f x)
  (Err e) <*> _       = Err e
  _       <*> (Err e) = Err e

public export
Monad Result where
  (Ok x)  >>= f = f x
  (Err e) >>= _ = Err e
