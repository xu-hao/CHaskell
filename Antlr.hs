module Antlr where

import Prelude hiding (sum)

data Expr = Var {unVar :: String} | BinOp {left :: Expr, op :: String, right :: Expr} | ListOp {op :: String, args :: [Expr]}

data ExprCon = VarCon String | BinOpCon ExprCon String ExprCon | ListOpCon String [ExprCon]


intercalate :: [a] -> [[a]] -> [a]
intercalate sep = foldl1 ((\a b -> a ++ sep ++ b) :: [a] -> [a] -> [a])

interint :: [Int] -> [[Int]] -> [Int]
interint sep l = intercalate sep l

sum :: [Int] -> Int
sum = foldl (+) 0

concat :: [[a]] -> [a]
concat = foldl ((++) :: [a] -> [a] -> [a]) ([] :: [a])

s :: Expr -> String
s (Var v) = v
s (BinOp a b c) = s a ++  b ++ s c
s (ListOp a b) = a ++ "(" ++ intercalate "," (map s b) ++ ")"

s2 :: Expr -> String
s2 v0 = case v0 of
    (Var v) -> v
    (BinOp a b c) -> s a ++  b ++ s c
    (ListOp a b) -> a ++ "(" ++ intercalate "," (map s b) ++ ")"

ss :: String -> Expr -> String
ss n (Var v) = v
ss n (BinOp a b c) = ss n a ++  b ++ ss n c
ss n (ListOp a b) = a ++ "(" ++ intercalate "," (map (ss n) b) ++ ")"

sz :: Expr -> Int
sz (Var v) = 1
sz (BinOp a b c) = sz a +  1 + sz c
sz (ListOp a b) = 1 + sum (map sz b)

opop :: Expr -> Int
opop (Var v) = 0
opop (BinOp a b c) = case a of
      (Var _) -> 0
      (BinOp _ _ _) -> 1
      (ListOp _ _) -> 0
opop (ListOp a b) = 0

letcase :: Expr -> Int
letcase (Var v) = 0
letcase (BinOp a b c) =
  let x :: Int
      x = case a of
          (Var v) -> 0
          (BinOp a b c) -> 1
          (ListOp a b) -> 0 in
      x + 1
letcase (ListOp a b) = 0

add :: Int -> Int -> Int
add a b = a + b

g :: Int -> Int
g x =
    let y = x * x
        z = y * y in
        z * z

letfunc :: Int -> Int
letfunc x =
    let f :: Int -> Int
        f x = x in
        f x

append2 :: [Int] -> [Int]
append2 x = x ++ [1,2,3]
