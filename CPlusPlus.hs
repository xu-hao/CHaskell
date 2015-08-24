module CPlusPlus where

import Control.Monad.State
import Data.List (intercalate)
import qualified Data.List as L
import Control.Applicative

data Type = TAuto
          | TCon String
          | TVar String
          | TApp String [Type]
          | TFun [Type] Type [Type] deriving Eq
data Param = Param Type String
data FuncSig = FuncSig Type String [Param] [String]
data Expr = This
          | Dot Expr String
          | Deref Expr
          | Var String
          | Con String
          | IntL Int
          | StrL String
          | BoolL Bool
          | Lam [Param] Block
          | If Expr Expr Expr
          | InitApp String [Expr]
          | App Expr [Expr]
          | TypeApp Expr [Type]
          | Op2 String
          | Op String
data Stmt = Return Expr
          | Def Template
          | DefAssign Type Expr Expr
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
         | VarDef Type String Expr
data Template = Template [Type] Def
              | NoTemplate Def

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
    showI (TFun vns rt pts) = do
        targss <- map (\t-> "const " ++ t ++ "&") <$> mapM showI pts
        rts <- showI rt
        return $ "std::function<"++rts ++ "("++ intercalate "," targss ++ ")>"
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
    showI (This) = return "this"
    showI (Var v) = return v
    showI (Con n) = return n
    showI (Dot a b) = do
        as <- showIExpr a
        return $ as ++ "." ++ b
    showI (Deref a) = do
        as <- showIExpr a
        return $ "*"++ as
    showI (IntL i) = return $ show i
    showI (StrL s) = return $ show s
    showI (BoolL b) = return $ if b then "true" else "false"
    showI (Lam vs e) = do
        es <- showI e
        vss <- mapM showI vs
        return $ "[=](" ++ intercalate "," vss ++ ")" ++ es
    showI (If e1 e2 e3) = do
        e1s <- showI e1
        e2s <- showI e2
        e3s <- showI e3
        return $ e1s ++ "?" ++ e2s ++ ":" ++ e3s
    showI (InitApp fn args) = do
        argss<-mapM showI args
        return $ fn ++ "{"++ intercalate "," argss ++ "}"
    showI (App (Op2 string) args) = do
        [arg1s, arg2s] <- mapM showI args
        return $ "(" ++ arg1s ++ string ++ arg2s ++ ")"
    showI (App (Op string) args) = do
        [arg1s] <- mapM showI args
        return $ "(" ++ string ++ arg1s ++ ")"
    showI (App fn args) = do
        fns <- showIExpr fn
        argss <- mapM showI args
        return $ fns ++ "("++ intercalate "," argss ++ ")"
    showI (TypeApp expr targs) = do
        fns <- showI expr
        argss <- mapM showI targs
        return $ fns ++ "<"++ intercalate "," argss ++ ">"
showIExpr fn = case fn of
    Con n -> return n
    Var n -> return n
    _ -> do
        s <- showI fn
        return $ "(" ++ s ++ ")"
instance ShowI Stmt where
    showI (Return v) = do
        i<-get
        vs <- showI v
        return $ i++"return " ++ vs ++ ";\n"
    showI (Def def) = showI def
    showI (DefAssign t e1 e2) =do
        i <- get
        ts <- showI t
        e1s <- showI e1
        e2s <- showI e2
        return $ i ++ ts ++ " " ++ e1s ++ "=" ++ e2s ++ ";\n"
instance ShowI Prog where
    showI (Prog preamble defs) = do
        defss<-concat<$> mapM showI defs
        return $ unlines (map ("#include " ++) preamble) ++ defss
instance ShowI Template where
    showI (Template tvs def) = do
        i<-get
        tvss <- mapM showI tvs
        defs <- showI def
        return $ i ++ "template <" ++ intercalate "," (map ("typename " ++) tvss) ++ " >\n" ++ defs
    showI (NoTemplate def) = showI def

showII a = evalState (showI a) ""

instance Show Type where
    show = showII
pname = ("_" ++)

mname = (++ "_")

vars v@ (TVar _) = [v]
vars (TFun vs _ _) = vs
vars (TApp t1 t2) = foldl L.union [] (map vars t2)
vars (TCon _) = []

infixl 4 @@
infixr 3 -->
(-->) t (TFun vs rt pts) =
    TFun (vars t `L.union` vs) rt (t:pts)

(-->) t s = TFun (vars t `L.union` vars s) s [t]
list e = "std::vector" @@ [e]

(@@) = TApp

variant :: [Type] -> Type
variant types = "boost::variant" @@ map (("boost::recursive_wrapper" @@) . return) types
