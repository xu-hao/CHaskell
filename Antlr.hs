module Antlr where

import Prelude hiding (sum)
import Data.List hiding (intercalate, sum)

data Expr = Var {unVar :: String} | BinOp {left :: Expr, op :: String, right :: Expr} | ListOp {op :: String, args :: [Expr]}

data ExprCon = VarCon String | BinOpCon ExprCon String ExprCon | ListOpCon String [ExprCon]


intercalate :: [a] -> [[a]] -> [a]
intercalate sep = foldl1 (\a b -> a ++ sep ++ b)

sum :: [Int] -> Int
sum = foldl (+) 0

s :: Expr -> String
s (Var v) = v
s (BinOp a b c) = s a ++  b ++ s c
s (ListOp a b) = a ++ "(" ++ intercalate "," (map s b) ++ ")"

sz :: Expr -> Int
sz (Var v) = 1
sz (BinOp a b c) = sz a +  1 + sz c
sz (ListOp a b) = 1 + sum (map sz b)

add :: Int -> Int -> Int
add a b = a + b
