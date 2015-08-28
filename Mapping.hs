module Mapping where

import CPlusPlus

import Prelude hiding (lookup, map)
import Language.Haskell.Parser
import Language.Haskell.Syntax
import System.Environment
import Control.Monad.State
import Data.List (intercalate, sortBy)
import qualified Data.List as L
import Data.Map hiding (foldl)
import Control.Applicative ((<$>),(<*>))
import Data.Maybe

wrapFunc :: Type -> Expr -> Expr
wrapFunc ty expr =
    let fcons = showII ty in
        App (Con fcons) [expr]

curried2 :: Type -> Expr -> Expr
curried2 (TFun (TFun t2 rt) t1) e =
    Lam [Param t1 "_p1"] (Block [Return $ Lam [Param t2 "_p2"] (Block [Return $ App e [Var "_p1" t1, Var "_p2" t2]])])
curried2 ty _ = error $ showII ty

curried1 :: Type -> Expr -> Expr
curried1 (TFun rt t1) e = Lam [Param t1 "_p1"] (Block [Return $ App e [Var "_p1" t1]])

symtable :: Map String (Type -> Expr)
symtable = fromList [
    (":", Var "chaskell::cons"),
    ("++", Var "chaskell::append"),
    ("+", Op2 "+") ,
    ("-", Op2 "-"),
    ("*", Op2 "*"),
    ("/", Op2 "/"),
    ("==", Op2 "=="),
    ("/=", Op2 "!="),
    ("&&", Op2 "&&"),
    ("||", Op2 "||"),
    ("not", Op "!"),
    ("True" ,\ty -> BoolL True),
    ("False", \ty -> BoolL False)]

primtypetable = fromList [
    ("String", TApp "std::vector" [TCon  "char"]),
    ("Int", TCon "int"),
    ("Bool", TCon "bool")
    ]

typetable = mapWithKey (\n ty -> TypeScheme (vars ty) ty) typetable0

typetable0 :: Map String Type
typetable0 = fromList $ [
    (":", TVar "A" --> list (TVar "A") --> list (TVar "A")),
    ("&&",  TCon "bool" --> TCon "bool" --> TCon "bool"),
    ("||",  TCon "bool" --> TCon "bool" --> TCon "bool"),
    ("==",  TVar "A" --> TVar "A" --> TCon "bool"),
    ("/=",  TVar "A" --> TVar "A" --> TCon "bool"),
    ("+",  TCon "int" --> TCon "int" --> TCon "int"),
    ("-",  TCon "int" --> TCon "int" --> TCon "int"),
    ("/",  TCon "int" --> TCon "int" --> TCon "int"),
    ("*",  TCon "int" --> TCon "int" --> TCon "int"),
    ("++", list (TVar "A") --> list (TVar "A") --> list (TVar "A")),
    ("not",  TCon "bool" --> TCon "bool"),
    ("map", (TVar "B" --> TVar "A") --> list (TVar "B") --> list (TVar "A")),
    ("foldl", (TVar "B" --> TVar "A" --> TVar "B") --> TVar "B" --> list (TVar "A") --> TVar "B"),
    ("foldl1", (TVar "A" --> TVar "A" --> TVar "A") --> list (TVar "A") --> TVar "A")]
currytable :: Map String Int
currytable = fromList $ [
    ("chaskell::cons", 2),
    ("chaskell::append", 2),
    (":", 2),
    ("&&",  2),
    ("||",  2),
    ("==",  2),
    ("/=",  2),
    ("+",  2),
    ("-",  2),
    ("/",  2),
    ("*",  2),
    ("++", 2),
    ("map", 2),
    ("foldl", 3),
    ("foldl1", 2)]
