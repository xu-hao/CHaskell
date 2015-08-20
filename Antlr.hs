module Antlr where

data Expr = Var {unVar :: String} | BinOp {left :: Expr, op :: String, right :: Expr} | ListOp {op :: String, args :: [Expr]}

data ExprCon = VarCon String | BinOpCon Expr String Expr | ListOpCon String [Expr]
