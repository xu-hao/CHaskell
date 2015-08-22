module Antlr where

import Data.List

data Expr = Var {unVar :: String} | BinOp {left :: Expr, op :: String, right :: Expr} | ListOp {op :: String, args :: [Expr]}

data ExprCon = VarCon String | BinOpCon ExprCon String ExprCon | ListOpCon String [ExprCon]

s :: Expr -> String
s (Var v) = v
s (BinOp a b c) = s a ++  b ++ s c
s (ListOp a b) = a ++ "(" ++ intercalate "," (map s b) ++ ")"

sz :: Expr -> Int
sz (Var v) = 1
sz (BinOp a b c) = sz a +  1 + sz c
sz (ListOp a b) = 1 + sum (map sz b)
