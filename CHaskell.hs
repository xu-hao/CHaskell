module CHaskell where

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
          | App Expr [Expr]
data Stmt = Return Expr
data Block = Block [Stmt]
data Init = Init String Expr
data Member = Field Type String
            | Ctor String String [Param] [Init] Block
            | TemplateMember Template
data Def = StructDef String [Member]
         | StructFwdDecl String
         | Typedef Type String
         | FuncDef FuncSig Block
data Template = Template [Type] Def
              | NoTemplate Def

data Prog = Prog [String] [Template]

instance Show Type where
    show (TVar t) = t
    show (TCon t) = t
    show (TApp t targs) = t ++ "<" ++ intercalate "," (map show targs) ++ " >"
instance Show Param where
    show (Param t n) = show t ++ " " ++ n
instance Show Def where
    show (FuncDef sig block) = show sig ++ show block
    show (StructDef n members) = "struct " ++ n ++ "{\n" ++ concatMap show members ++ "};\n"
    show (StructFwdDecl n) = "struct " ++ n ++ ";\n"
    show (Typedef t n) = "typedef " ++ show t ++ " " ++ n ++ ";\n"
instance Show FuncSig where
    show (FuncSig t n ps) = show t ++ " " ++ n ++ intercalate "," (map show ps)
instance Show Init where
    show (Init n v) = n ++ "(" ++ show v ++ ")"
instance Show Block where
    show (Block stmts) = "{\n" ++ concatMap (("\t\t" ++) . show) stmts ++ "\t}\n"
instance Show Member where
    show (Field t n) = "\t" ++ show t ++ " " ++ n ++ ";\n"
    show (Ctor mod n ps inits block) = "\t" ++ mod ++ " " ++ n ++ " (" ++ intercalate "," (map show ps) ++ "):\n\t\t" ++ intercalate "," (map show inits) ++ show block
    show (TemplateMember template) = show template
instance Show Expr where
    show (This) = "this"
    show (Var v) = v
    show (Dot a b) = "(" ++ show a ++ ").(" ++ show b ++ ")"
    show (Deref a) = "*(" ++ show a ++ ")"
    show (IntL i) = show i
    show (StrL s) = show s
    show (App fn args) = "(" ++ show fn ++ ")("++ intercalate "," (map show args) ++ ")"
instance Show Stmt where
    show (Return v) = "return " ++ show v ++ ";\n"
instance Show Prog where
    show (Prog preamble defs) = unlines (map ("#include " ++) preamble) ++ concatMap show defs
instance Show Template where
    show (Template tvs def) = "template <" ++ intercalate "," (map show tvs) ++ " >\n" ++ show def
    show (NoTemplate def) = show def

(@@) = TApp

variant :: [Type] -> Type
variant types = "boost::variant" @@ map (("boost::recursive_wrapper" @@) . return) types

tranlateHaskellToCPlusPlus :: HsModule -> Prog
tranlateHaskellToCPlusPlus (HsModule _ _ _ _ decls) =
    Prog ["<string>", "<vector>", "<utility>", "<boost/variant.hpp>"] (concatMap tranlateHaskellDeclsToCPlusPlus decls) where
        extractConstructorName (HsRecDecl _ (HsIdent name) _) = name
        extractConstructorName (HsConDecl _ (HsIdent name) _) = name

        extractConstructorParam (name, HsUnBangedTy ty) = Param (tranlateHaskellTypeToCPlusPlus ty) (pname name)

        extractFieldNameAndType (HsIdent name : _, ty) = (name, ty)

        extractConstructorInit (name, _) = Init (mname name) (App (Var "std::move") [Var (pname name)])

        tranlateHaskellDeclsToCPlusPlus (HsDataDecl _ _ (HsIdent name) tvars constructors _) =
            let constructorNames = map extractConstructorName constructors
                fwddecls = map tranlateHaskellConsDeclsToFwdDeclsCPlusPlus constructors
                tdef = NoTemplate (Typedef (variant (map TCon constructorNames)) name) in
                fwddecls ++ tdef : map tranlateHaskellConsDeclsToCPlusPlus constructors

        tranlateHaskellConsDeclsToFwdDeclsCPlusPlus (HsRecDecl _ (HsIdent name) _) =
            NoTemplate (StructFwdDecl name)
        tranlateHaskellConsDeclsToFwdDeclsCPlusPlus (HsConDecl _ (HsIdent name) _) =
            NoTemplate (StructFwdDecl name)

        tranlateHaskellConsDeclsToCPlusPlus (HsRecDecl _ (HsIdent name) fields) =
            let fields2 = map extractFieldNameAndType fields in
                tranlateHaskellConsDeclsToCPlusPlus2 name fields2
        tranlateHaskellConsDeclsToCPlusPlus (HsConDecl _ (HsIdent name) fields) =
            let fields2 = zip (map (("arg" ++). show) [1..length fields]) fields in
                tranlateHaskellConsDeclsToCPlusPlus2 name fields2

        tranlateHaskellConsDeclsToCPlusPlus2 name fields2 =
            let params = map extractConstructorParam fields2
                inits = map extractConstructorInit fields2
                ctor = Ctor "explicit" name params inits (Block []) in
                NoTemplate (StructDef name (ctor : map tranlateHaskellRecFieldDeclsToCPlusPlus fields2))

        tranlateHaskellRecFieldDeclsToCPlusPlus (i, HsUnBangedTy ty) =
            Field (tranlateHaskellTypeToCPlusPlus ty) (mname i)

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
