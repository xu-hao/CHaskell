module Mapping where

import CPlusPlus

import Prelude hiding (lookup)
import Language.Haskell.Parser
import Language.Haskell.Syntax
import System.Environment
import Control.Monad.State
import Data.List (intercalate, sortBy)
import qualified Data.List as L
import Data.Map hiding (map, foldl, (!))
import Control.Applicative ((<$>),(<*>))
import Data.Maybe

symtable = fromList [
    ("++", Con "chaskell::append"),
    ("+", Op2 "+"),
    ("-", Op2 "-"),
    ("*", Op2 "*"),
    ("/", Op2 "/"),
    ("==", Op2 "=="),
    ("/=", Op2 "!="),
    ("&&", Op2 "&&"),
    ("||", Op2 "||"),
    ("not", Op "!"),
    ("True" ,BoolL True),
    ("False", BoolL False)]

primtypetable = fromList [
    ("String", TCon "std::string"),
    ("Int", TCon "int"),
    ("Bool", TCon "bool")
    ]

typetable = fromList [
    ("chaskell::cons", TVar "A" --> list (TVar "A") --> list (TVar "A")),
    ("&&",  TCon "bool" --> TCon "bool" --> TCon "bool"),
    ("||",  TCon "bool" --> TCon "bool" --> TCon "bool"),
    ("==",  TVar "A" --> TVar "A" --> TCon "bool"),
    ("!=",  TVar "A" --> TVar "A" --> TCon "bool"),
    ("+",  TCon "int" --> TCon "int" --> TCon "int"),
    ("-",  TCon "int" --> TCon "int" --> TCon "int"),
    ("/",  TCon "int" --> TCon "int" --> TCon "int"),
    ("*",  TCon "int" --> TCon "int" --> TCon "int"),
    ("chaskell::append", list (TVar "A") --> list (TVar "A") --> list (TVar "A")),
    ("not",  TCon "bool" --> TCon "bool"),
    ("map", (TVar "B" --> TVar "A") --> list (TVar "A") --> list (TVar "B")),
    ("foldl", (TVar "B" --> TVar "A" --> TVar "B") --> TVar "B" --> list (TVar "A") --> TVar "B"),
    ("foldl1", (TVar "A" --> TVar "A" --> TVar "A") --> list (TVar "A") --> TVar "A")]
