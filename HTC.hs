module HTC where

import Language.Haskell.Parser
import Language.Haskell.Syntax

import System.Environment
import Data.List (intercalate)

data Type = TCon String
          | TVar String
          | TApp String [Type]
data Param = Param Type String
data FuncSig = FuncSig Type String [Param]
data Expr = This
          | Dot Expr Expr
          | Deref Expr
          | Var String
          | IntL Int
          | StrL String
          | App String [Expr]
data Stmt = Return Expr
data Block = Block [Stmt]
data FuncDef = FuncDef FuncSig Block
data Init = Init String Expr
data Member = Field Type String
            | Ctor String String [Param] [Init] Block
            | Method FuncDef
data StructDef = StructDef String [Member]
data Def = Template [Type] (Either StructDef FuncDef)
         | NoTemplate (Either StructDef FuncDef)
         | Typedef Type String
data Prog = Prog [Def]

instance Show Type where
    show (TVar t) = t
    show (TCon t) = t
    show (TApp t targs) = t ++ "<" ++ intercalate "," (map show targs) ++ " >"
instance Show Param where
    show (Param t n) = show t ++ " " ++ n
instance Show FuncDef where
    show (FuncDef sig block) = show sig ++ show block
instance Show FuncSig where
    show (FuncSig t n ps) = show t ++ " " ++ n ++ intercalate "," (map show ps)
instance Show Init where
    show (Init n v) = n ++ "(" ++ show v ++ ")"
instance Show Block where
    show (Block stmts) = "{\n" ++ concatMap (("\t\t" ++) . show) stmts ++ "\t}\n"
instance Show Member where
    show (Field t n) = "\t" ++ show t ++ " " ++ n ++ ";\n"
    show (Ctor mod n ps inits block) = "\t" ++ mod ++ " " ++ n ++ " (" ++ intercalate "," (map show ps) ++ "):\n\t\t" ++ intercalate "," (map show inits) ++ show block
    show (Method fdef) = show fdef
instance Show Expr where
    show (This) = "this"
    show (Var v) = v
    show (Dot a b) = "(" ++ show a ++ ").(" ++ show b ++ ")"
    show (Deref a) = "*(" ++ show a ++ ")"
    show (IntL i) = show i
    show (StrL s) = show s
    show (App fn args) = fn ++ "("++ intercalate "," (map show args) ++ ")"
instance Show Stmt where
    show (Return v) = "return " ++ show v ++ ";\n"
instance Show StructDef where
    show (StructDef n members) = "struct " ++ n ++ "{\n" ++ concatMap show members ++ "};\n"
instance Show Prog where
    show (Prog defs) = concatMap show defs
instance Show Def where
    show (Template tvs def) = "template <" ++ intercalate "," (map show tvs) ++ " >\n" ++ either show show def
    show (NoTemplate def) = either show show def
    show (Typedef t n) = show t ++ " " ++ n ++ ";\n"

(@@) = TApp

variant :: [Type] -> Type
variant types = "boost::variant" @@ map (("boost::recursive_wrapper" @@) . return) types

tranlateHaskellToCPlusPlus :: HsModule -> Prog
tranlateHaskellToCPlusPlus (HsModule _ _ _ _ decls) =
    Prog (concatMap tranlateHaskellDeclsToCPlusPlus decls) where
        tranlateHaskellDeclsToCPlusPlus (HsDataDecl _ _ (HsIdent name) tvars constructors _) =
            let constructorNames = map extractConstructorName constructors
                tdef = Typedef (variant (map TVar constructorNames)) name in
                tdef : map tranlateHaskellConsDeclsToCPlusPlus constructors
        extractConstructorName (HsRecDecl _ (HsIdent name) _) = name
        tranlateHaskellConsDeclsToCPlusPlus (HsRecDecl _ (HsIdent name) fields) =
            let params = map extractConstructorParam fields
                inits = map extractConstructorInit fields
                ctor = Ctor "explicit" name params inits (Block []) in
                NoTemplate (Left (StructDef name (ctor : map tranlateHaskellConsFieldDeclsToCPlusPlus fields)))
        extractConstructorParam (HsIdent name : _, HsUnBangedTy ty) = Param (tranlateHaskellTypeToCPlusPlus ty) (pname name)
        extractConstructorInit (HsIdent name : _, HsUnBangedTy ty) = Init (mname name) (Var (pname name))
        tranlateHaskellConsFieldDeclsToCPlusPlus (HsIdent name : _, HsUnBangedTy ty) =
            Field (tranlateHaskellTypeToCPlusPlus ty) (mname name)
        tranlateHaskellTypeToCPlusPlus (HsTyVar (HsIdent name)) = TVar name
        tranlateHaskellTypeToCPlusPlus (HsTyCon (UnQual (HsIdent name))) | name == "String" = TCon "std::string"
                                                                         | name == "Int" = TCon "int"
                                                                         | otherwise = TCon name
        tranlateHaskellTypeToCPlusPlus (HsTyApp (HsTyCon (Special HsListCon)) ty) = TApp "std::vector" [tranlateHaskellTypeToCPlusPlus ty]
        tranlateHaskellTypeToCPlusPlus ty = error (show ty)
        pname = ("_" ++)
        mname = (++ "_")


main :: IO ()
main = do
    (input : _) <- getArgs
    str <- readFile input
    case parseModule str of
        ParseOk a -> do
            print (tranlateHaskellToCPlusPlus a)
        ParseFailed loc str -> do
            print loc
            print str
