module Antlr where

data Expr = Var {unVar :: String} | BinOp {left :: Expr, op :: String, right :: Expr} | ListOp {op :: String, args :: [Expr]}
