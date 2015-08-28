module CPlusPlus where

import Control.Monad.State
import Data.List (intercalate)
import qualified Data.List as L
import Control.Applicative
import Data.Map (Map, fromList, (!))

data Type = TAuto
          | TCon String
          | TVar String
          | TApp String [Type]
          | TFun Type Type deriving Eq

data TypeScheme = TypeScheme [String] Type

instance Show TypeScheme where
    show (TypeScheme vs ty) = "forall " ++ show vs ++ " " ++ showII ty

data Param = Param Type String
data FuncSig = FuncSig Type String [Param] [String]
data Expr = This
          | Dot Expr String
          | Deref Expr
          | Var String Type -- type is used for optimization
          | Con String
          | IntL Int
          | StrL String
          | BoolL Bool
          | Lam [Param] Block
          | If Expr Expr Expr
          | InitApp Expr [Expr]
          | App Expr [Expr]
          | TypeApp Expr [Type]
          | Op2 String Type
          | Op String Type
          | AppT String [Type]
data Stmt = Return Expr
          | Def Template
data Block = Block [Stmt]
data Init = Init String Expr
data Member = Field Type String
            | Ctor String String [Param] [Init] Block
            | TemplateMember Template
data Inherit = Public Type
             | Private Type
data Def = StructDef String [Inherit] [Member]
         | StructFwdDecl String
         | Typedef Type String
         | FuncDef FuncSig Block
         | FuncProto FuncSig
         | VarProto Type String
         | VarDef Type String Expr
data Template = Template [String] Def
              | NoTemplate Def
              | Comment String

tToE :: Type -> Expr
tToE (TApp n ts) = AppT n ts
tToE t = error $ "tToE:: cannot create constructor expression for type " ++ showII t

wrapTFun :: Type -> Type
wrapTFun (TFun rt pt) = "std::function" @@ [wrapTFun pt --> wrapTFun rt]
wrapTFun (TApp n pts) = n @@ map wrapTFun pts
wrapTFun t = t

unwrapTFun :: Type -> Type
unwrapTFun (TFun rt pt) = unwrapTFun pt --> unwrapTFun rt
unwrapTFun (TApp "std::function" [pt]) = unwrapTFun pt
unwrapTFun (TApp n pts) = n @@ map unwrapTFun pts
unwrapTFun t = t

data Prog = Prog [String] [Template]
class ShowI a where
    showI :: a -> State String String

instance ShowI Type where
    showI (TAuto) = return "auto"
    showI (TVar t) = return $ "T" ++ t
    showI (TCon t) = return t
    showI (TApp t targs) = do
        targss <- mapM showI targs
        return $ t ++ "<" ++ intercalate "," targss ++ " >"
    showI (TFun rt0 pt0) = do
        -- let (rt, pts) = gatherPts rt0 [pt0]
        -- targss <- map (\t-> "const " ++ t ++ "&") <$> mapM showI pts
        -- rts <- showI rt
        targ0 <- showI pt0
        let targss = [targ0]
        rts <- showI rt0
        return $ rts ++ "("++ intercalate "," (map (\ts -> "const " ++ ts ++ "&") targss) ++ ")"
instance ShowI Param where
    showI (Param t n) = do
        ts <- showI t
        return $ "const " ++ ts ++ "& " ++ n
instance ShowI Inherit where
    showI (Public t) = ("public " ++) <$> showI t
    showI (Private t) = ("private " ++) <$> showI t

instance ShowI Def where
    showI (FuncDef sig block) = do
        i <- get
        sigs <- showI sig
        blocks <- showI block
        return $ i ++ sigs ++ blocks ++ "\n"
    showI (StructDef n inherits members) = do
        i <- get
        put ("\t" ++ i)
        memberss <- concat <$> mapM showI members
        inheritss <- mapM showI inherits
        put i
        return $ i ++ "struct " ++ n ++ (if L.null inherits then "" else ":" ++ intercalate "," inheritss) ++ "{\n" ++ memberss ++ i ++ "};\n"
    showI (StructFwdDecl n) = do
        i <- get
        return $ i ++ "struct " ++ n ++ ";\n"
    showI (Typedef t n) = do
        i <- get
        ts <- showI t
        return $ i ++ "typedef " ++ ts ++ " " ++ n ++ ";\n"
    showI (FuncProto sig) = do
        i<-get
        sigs <- showI sig
        return $ i++ sigs++";\n"
    showI (VarDef ty n expr) = do
        i<-get
        tys <- showI ty
        exprs <- showI expr
        return $ i++ tys ++ " " ++ n ++ "=" ++ exprs ++ ";\n"
    showI (VarProto ty n) = do
        i<- get
        tys <- showI ty
        return $ i++ tys ++ " " ++ n ++ ";\n"
instance ShowI FuncSig where
    showI (FuncSig t n ps postmods) = do
        ts<-showI t
        pss <- mapM showI ps
        return $ ts ++ " " ++ n ++ "(" ++ intercalate "," pss ++ ")"++ unwords postmods
instance ShowI Init where
    showI (Init n v) = do
        vs <- showI v
        return $ n ++ "(" ++ vs ++ ")"
instance ShowI Block where
    showI (Block stmts) = do
        i <- get
        put ("\t"++i)
        stmtss <- concat <$> mapM showI stmts
        put i
        return $ "{\n" ++ stmtss ++ i++"}"
instance ShowI Member where
    showI (Field t n) = do
        i <-get
        ts <- showI t
        return $ i ++ ts ++ " " ++ n ++ ";\n"
    showI (Ctor mod n ps inits block) = do
        i <- get
        pss <- mapM showI ps
        initss <- mapM showI inits
        blocks <-  showI block
        return $ i ++ mod ++ " " ++ n ++ " (" ++ intercalate "," pss ++ ")" ++ (if L.null initss then "" else ":\n" ++ i ++ "\t" ++ intercalate "," initss) ++ blocks ++ "\n"
    showI (TemplateMember template) = showI template
instance ShowI Expr where
    showI = showI' 100 False

precedencetable :: Map String Int
precedencetable = fromList [
    (".", 2),
    ("deref", 3),
    ("!", 3),
    ("*", 5),
    ("/", 5),
    ("+", 6),
    ("-", 6),
    ("==", 9),
    ("/=", 9),
    ("&&", 13),
    ("||", 14)
    ]

class ShowI' a where
    showI' :: Int -> Bool -> a -> State String String
instance ShowI' Expr where
    showI' _ _ (Op n _) = return n
    showI' _ _ (Op2 n _) = return n
    showI' _ _ (This) = return "this"
    showI' _ _ (Var v _) = return v
    showI' _ _ (Con n) = return n
    showI' n assoc (Dot a b) = do
        let precedence = precedencetable ! "."
        as <- showI' precedence False a
        return $ paren n assoc precedence (as ++ "." ++ b)
    showI' n assoc (Deref a) = do
        let precedence = precedencetable ! "deref"
        as <- showI' precedence False a
        return $ paren n assoc precedence ("*"++ as)
    showI' n assoc  (IntL i) = return $ show i
    showI' n assoc  (StrL s) = return $ show s
    showI' n assoc  (BoolL b) = return $ if b then "true" else "false"
    showI' n assoc  (Lam vs e) = do
        es <- showI e
        vss <- mapM showI vs
        return $ "[=](" ++ intercalate "," vss ++ ")" ++ es
    showI' n assoc  (If e1 e2 e3) = do
        e1s <- showI e1
        e2s <- showI e2
        e3s <- showI e3
        return $ e1s ++ "?" ++ e2s ++ ":" ++ e3s
    showI' n assoc  (InitApp fn args) = do
        fns <- showI fn
        argss<-mapM showI args
        return $ fns ++ "{"++ intercalate "," argss ++ "}"
    showI' n assoc  (App (Op2 string _) [arg1, arg2]) = do
        let p = precedencetable ! string
        arg1s  <- showI' p False arg1
        arg2s <- showI' p True arg2
        return $ paren n assoc p (arg1s ++ string ++ arg2s)
    showI' n assoc  (App (Op string _) [arg]) = do
        let precedence = precedencetable ! string
        arg1s <- showI' precedence False arg
        return $ paren n assoc precedence (string ++ arg1s)
    showI' n assoc   (AppT exp tys) = do
        tyss <- mapM showI tys
        return $ exp ++ "<" ++ intercalate "," tyss ++ ">"
    showI' n assoc   (App fn args) = do
        fns <- showI fn
        argss <- mapM showI args
        return $ fns ++ "("++ intercalate "," argss ++ ")"
    showI' n assoc   (TypeApp expr targs) = do
        fns <- showI expr
        argss <- mapM showI targs
        return $ fns ++ "<"++ intercalate "," argss ++ ">"
paren n assoc p s =
    if n < p || (n == p && assoc)
        then "(" ++ s ++ ")"
        else s
instance ShowI Stmt where
    showI (Return v) = do
        i<-get
        vs <- showI v
        return $ i++"return " ++ vs ++ ";\n"
    showI (Def def) = showI def
instance ShowI Prog where
    showI (Prog preamble defs) = do
        defss<-concat<$> mapM showI defs
        return $ unlines (map ("#include " ++) preamble) ++ defss
instance ShowI Template where
    showI (Template tvs def) = do
        i<-get
        let tvss = tvs
        defs <- showI def
        return $ i ++ "template <" ++ intercalate "," (map ("typename T" ++) tvss) ++ " >\n" ++ defs
    showI (NoTemplate def) = showI def
    showI (Comment string) = do
        i <- get
        return $ i ++ "//" ++ string ++ "\n"

showII a = evalState (showI a) ""

instance Show Type where
    show = showII
pname = ("_" ++)

mname = (++ "_")

vars v@ (TVar n) = [n]
vars (TFun rt pts) = vars rt `L.union` vars pts
vars (TApp t1 t2) = foldl L.union [] (map vars t2)
vars (TCon _) = []

infixl 4 @@
infixr 3 -->
(-->) pt rt =
    TFun rt pt

list e = "std::vector" @@ [e]

(@@) = TApp

variant :: [Type] -> Type
variant types = "boost::variant" @@ map (("boost::recursive_wrapper" @@) . return) types

tfun :: Type -> Type
tfun ft = TApp "std::function" [ft]
