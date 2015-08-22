module CHaskell where

import Language.Haskell.Parser
import Language.Haskell.Syntax
import System.Environment
import Control.Monad.State
import Data.List (intercalate, sortBy)
import Data.Map hiding (map, foldl)
import Control.Applicative ((<$>),(<*>))

data Type = TAuto
          | TCon String
          | TVar String
          | TApp String [Type]
data Param = Param Type String
data FuncSig = FuncSig Type String [Param]
data Expr = This
          | Dot Expr String
          | Deref Expr
          | Var String
          | Con String
          | IntL Int
          | StrL String
          | Lam [String] Block
          | If Expr Expr Expr
          | InitApp String [Expr]
          | App Expr [Expr]
data Stmt = Return Expr
          | Def Template
          | DefAssign Type Expr Expr
data Block = Block [Stmt]
data Init = Init String Expr
data Member = Field Type String
            | Ctor String String [Param] [Init] Block
            | TemplateMember Template
data Def = StructDef String [Member]
         | StructFwdDecl String
         | Typedef Type String
         | FuncDef FuncSig Block
         | FuncProto FuncSig
data Template = Template [Type] Def
              | NoTemplate Def

data Prog = Prog [String] [Template]
class ShowI a where
    showI :: a -> State String String

instance ShowI Type where
    showI (TAuto) = return "auto"
    showI (TVar t) = return t
    showI (TCon t) = return t
    showI (TApp t targs) = do
        targss <- mapM showI targs
        return $ t ++ "<" ++ intercalate "," targss ++ " >"
instance ShowI Param where
    showI (Param t n) = do
        ts <- showI t
        return $ ts ++ " " ++ n
instance ShowI Def where
    showI (FuncDef sig block) = do
        i <- get
        sigs <- showI sig
        blocks <- showI block
        return $ i ++ sigs ++ blocks ++ "\n"
    showI (StructDef n members) = do
        i <- get
        put ("\t" ++ i)
        memberss <- concat <$> mapM showI members
        put i
        return $ i ++ "struct " ++ n ++ "{\n" ++ memberss ++ "};\n"
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
instance ShowI FuncSig where
    showI (FuncSig t n ps) = do
        ts<-showI t
        pss <- mapM showI ps
        return $ ts ++ " " ++ n ++ "(" ++ intercalate "," pss ++ ")"
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
        return $ i ++ mod ++ " " ++ n ++ " (" ++ intercalate "," pss ++ "):\n" ++ i ++ "\t" ++ intercalate "," initss ++ blocks ++ "\n"
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
    showI (Lam vs e) = do
        es <- showI e
        return $ "[=](" ++ intercalate "," (map ("auto "++) vs) ++ ")" ++ es
    showI (If e1 e2 e3) = do
        e1s <- showI e1
        e2s <- showI e2
        e3s <- showI e3
        return $ e1s ++ "?" ++ e2s ++ ":" ++ e3s
    showI (InitApp fn args) = do
        argss<-mapM showI args
        return $ fn ++ "{"++ intercalate "," argss ++ "}"
    showI (App fn args) = do
        fns <- showIExpr fn
        argss <- mapM showI args
        return $ fns ++ "("++ intercalate "," argss ++ ")"
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
        return $ i ++ "template <" ++ intercalate "," tvss ++ " >\n" ++ defs
    showI (NoTemplate def) = showI def

pname = ("_" ++)

mname = (++ "_")

tranlateHaskellTypeToCPlusPlus (HsTyVar (HsIdent name)) = TVar name
tranlateHaskellTypeToCPlusPlus (HsTyCon (UnQual (HsIdent name))) | name == "String" = TCon "std::string"
                                                                 | name == "Int" = TCon "int"
                                                                 | otherwise = TCon name
tranlateHaskellTypeToCPlusPlus (HsTyApp (HsTyCon (Special HsListCon)) ty) = TApp "std::vector" [tranlateHaskellTypeToCPlusPlus ty]
tranlateHaskellTypeToCPlusPlus ty = error (show ty)

translateHaskellExprToCPlusPlus :: HsExp -> TranEnv Expr
translateHaskellExprToCPlusPlus (HsCon n) = return $ Con (extractQName n)
translateHaskellExprToCPlusPlus (HsVar n) = return $ Var (extractQName n)
translateHaskellExprToCPlusPlus (HsLit (HsString s)) = return $ StrL s
translateHaskellExprToCPlusPlus (HsLit (HsInt i)) = return $ IntL (fromIntegral i)
translateHaskellExprToCPlusPlus (HsInfixApp exp1 op exp2) = App (Con (extractQOp op)) <$> mapM translateHaskellExprToCPlusPlus [exp1, exp2]
translateHaskellExprToCPlusPlus (HsApp exp1 exp2) = do
  expr2 <- translateHaskellExprToCPlusPlus exp2
  transApp exp1 [expr2] where
      transApp (HsApp exp1 exp2) args = do
          expr2 <- translateHaskellExprToCPlusPlus exp2
          transApp exp1 (expr2 : args)
      transApp exp1 args =
          App <$> translateHaskellExprToCPlusPlus exp1 <*> return args
translateHaskellExprToCPlusPlus (HsLambda _ [HsPVar n] exp) = Lam [extractName n] <$> Block <$> sequence [Return <$> translateHaskellExprToCPlusPlus exp]
translateHaskellExprToCPlusPlus (HsLet decls exp) = error "let is not supported"
translateHaskellExprToCPlusPlus (HsIf exp1 exp2 exp3) = If <$> translateHaskellExprToCPlusPlus exp1 <*> translateHaskellExprToCPlusPlus exp2 <*> translateHaskellExprToCPlusPlus exp3
translateHaskellExprToCPlusPlus (HsCase exp alts) = do
    stmts <- translateHaskellAltsToLambdaCPlusPlus alts
    stmt <- translateHaskellExprToCPlusPlus exp
    return $ App (Con "boost::apply_visitor") (stmts ++ [stmt])
translateHaskellExprToCPlusPlus (HsDo stmts) = error "not supported"
translateHaskellExprToCPlusPlus (HsTuple exps) = App (Con "std::make_tuple") <$> mapM translateHaskellExprToCPlusPlus exps
translateHaskellExprToCPlusPlus (HsList exps) = InitApp "std::vector" <$> mapM translateHaskellExprToCPlusPlus exps
translateHaskellExprToCPlusPlus (HsParen exp) = translateHaskellExprToCPlusPlus exp
translateHaskellExprToCPlusPlus (_) = error "not supported"
type DataDic = Map String (Int, [String])
type TypeDic = Map String (Type, [Type])
type TranEnv = StateT DataDic (State TypeDic)

translateHaskellAltsToLambdaCPlusPlus :: Pattern p => [p] -> TranEnv [Expr]
translateHaskellAltsToLambdaCPlusPlus alts = do
    alts' <- sortAlts alts
    mapM translateHaskellAltToLambdaCPlusPlus alts

sortAlts :: Pattern p => [p] -> TranEnv [p]
sortAlts alts = do
    datadic <- get
    return (sortBy (\alt1 alt2 ->
        let cons1 = extractCons alt1
            cons2 = extractCons alt2
            (i1, _) = datadic ! cons1
            (i2, _) = datadic ! cons2 in
            compare i1 i2) alts)

translateHaskellAltToLambdaCPlusPlus :: Pattern p => p -> TranEnv Expr
translateHaskellAltToLambdaCPlusPlus p =
    Lam ["_var"] <$> Block <$> ((++) <$> translateHaskellPatternToStmtsCpp (Var "_var") (extractPat p)<*> sequence [Return <$> translateHaskellExprToCPlusPlus (extractExp p)])

translateHaskellPatternToStmtsCpp :: Expr -> HsPat -> TranEnv [Stmt]
translateHaskellPatternToStmtsCpp e (HsPVar n) = return [DefAssign TAuto (Var (extractName n)) e]
translateHaskellPatternToStmtsCpp e (HsPApp n pats) = do
    datadic <- get
    let (_, fields) = datadic ! extractQName n
    concat <$> mapM (\(field, pat) -> translateHaskellPatternToStmtsCpp (Dot e (mname field)) pat) (zip fields pats)
translateHaskellPatternToStmtsCpp e (HsPParen pat) = translateHaskellPatternToStmtsCpp e pat

-- translateHaskellExprToStmtCPlusPlus (HsLet decls exp) = Block (translateHaskellDeclListToCPlusPlus decls) [Return (translateHaskellExprToCPlusPlus exp)]

class Pattern a where
    extractCons :: a -> String
    extractPat :: a -> HsPat
    extractExp :: a -> HsExp
instance Pattern HsMatch where
    extractCons (HsMatch _ _ [HsPApp n _] _ _) = extractQName n
    extractPat (HsMatch _ _ [pat] _ _) = pat
    extractExp (HsMatch _ _ _ (HsUnGuardedRhs exp) _) = exp
instance Pattern HsAlt where
    extractCons (HsAlt _ (HsPApp n _) _ _) = extractQName n
    extractPat (HsAlt _ pat _ _) = pat
    extractExp (HsAlt _ _ (HsUnGuardedAlt exp) _) = exp


extractQOp (HsQVarOp name) = extractQName name
extractQOp (HsQConOp name) = extractQName name
extractQName (UnQual name) = extractName name
extractQName (Qual (Module m) n) = m ++ "::" ++ extractName n
extractQName (Special (HsTupleCon _) ) = "std::make_tuple"
extractQName (Special HsCons) = "chaskell::cons"
extractName (HsIdent n) = n
extractName (HsSymbol n) | n == "++" = "chaskell::append"
                         | n == "+" = "chaskell::add"

extractConstructorName (HsRecDecl _ (HsIdent name) _) = name
extractConstructorName (HsConDecl _ (HsIdent name) _) = name

extractConstructorNameAndFields (HsRecDecl _ n fields) = (extractName n, map (fst . extractFieldNameAndType) fields)
extractConstructorNameAndFields (HsConDecl _ n fields) = (extractName n, map (("var" ++) . show) [1..length fields])

extractConstructorParam (name, HsUnBangedTy ty) = Param (tranlateHaskellTypeToCPlusPlus ty) (pname name)

extractFieldNameAndType (HsIdent name : _, ty) = (name, ty)

extractConstructorInit (name, _) = Init (mname name) (App (Var "std::move") [Var (pname name)])

(@@) = TApp

variant :: [Type] -> Type
variant types = "boost::variant" @@ map (("boost::recursive_wrapper" @@) . return) types

tranlateHaskellToHPlusPlus :: HsModule -> TranEnv Prog
tranlateHaskellToHPlusPlus (HsModule _ _ _ _ decls) =
    Prog ["\"chaskell.hpp\"", "<string>", "<vector>", "<utility>", "<boost/variant.hpp>"] <$>
        translateHaskellDeclListToHPlusPlus decls

tranlateHaskellToCPlusPlus :: HsModule -> TranEnv Prog
tranlateHaskellToCPlusPlus (HsModule _ _ _ _ decls) =
    Prog ["\"chaskell.hpp\"", "<string>", "<vector>", "<utility>", "<boost/variant.hpp>"] <$>
        translateHaskellDeclListToCPlusPlus decls

translateHaskellDeclListToCPlusPlus:: [HsDecl] -> TranEnv [Template]
translateHaskellDeclListToCPlusPlus decls = concat <$> (sequence $ map tranlateHaskellDeclsToCPlusPlus decls)

translateHaskellDeclListToHPlusPlus:: [HsDecl] -> TranEnv [Template]
translateHaskellDeclListToHPlusPlus decls = concat <$> (sequence $ map tranlateHaskellDeclsToHPlusPlus decls)

tranlateHaskellDeclsToHPlusPlus:: HsDecl -> TranEnv [Template]
tranlateHaskellDeclsToHPlusPlus (HsDataDecl _ _ (HsIdent name) tvars constructors _) = do
    let constructorNameAndFields = map extractConstructorNameAndFields constructors
        constructorNames = map fst constructorNameAndFields
        fwddecls = map tranlateHaskellConsDeclsToFwdDeclsCPlusPlus constructors
        tdef = NoTemplate (Typedef (variant (map TCon constructorNames)) name)
    datadic <- get
    put $ datadic `union` fromList (map (\(i, (ctor, fields)) -> (ctor, (i, fields))) (zip [1..length constructorNameAndFields] constructorNameAndFields))
    return (fwddecls ++ tdef : map tranlateHaskellConsDeclsToCPlusPlus constructors)

tranlateHaskellDeclsToHPlusPlus (HsTypeSig _ [n] (HsQualType _ ty)) = do
    typedic <- lift get
    let fn = extractName n
    let tyt@(rt, pt) = tranlateHaskellFuncTypeToCPlusPlus ty
    lift $ put $ insert fn tyt typedic
    return [NoTemplate $ FuncProto $ FuncSig rt fn $ map (\ty -> Param ty "") pt ]
tranlateHaskellDeclsToHPlusPlus (HsFunBind matches) = return []

tranlateHaskellDeclsToCPlusPlus (HsFunBind matches) = do
    let curr0@(HsMatch _ currn0 _ _ _ ) = head matches
    let (curr, currn, groups) = foldl (\(curr,currn, prev) match@(HsMatch _ n _ _ _ )->
            if n == currn
                then (curr++[match],currn, prev)
                else ([match], n, prev++[(currn, curr)])) ([curr0], currn0, []) $ tail matches
    concat <$> mapM tranlateHaskellGroupsToCPlusPlus (groups++[(currn, curr) ])
tranlateHaskellDeclsToCPlusPlus _ = return []

tranlateHaskellFuncTypeToCPlusPlus :: HsType ->  (Type, [Type])
tranlateHaskellFuncTypeToCPlusPlus (HsTyFun ty1 ty2) =
    let ty1t = tranlateHaskellTypeToCPlusPlus ty1
        (rtt, ptt) = tranlateHaskellFuncTypeToCPlusPlus ty2 in
    (rtt, ty1t : ptt)
tranlateHaskellFuncTypeToCPlusPlus a = (tranlateHaskellTypeToCPlusPlus a, [])

tranlateHaskellGroupsToCPlusPlus :: (HsName, [HsMatch]) -> TranEnv [Template]
tranlateHaskellGroupsToCPlusPlus (n, matches) = do
    typedic <- lift get
    stmts <- translateHaskellAltsToLambdaCPlusPlus matches
    let exp = Var "_param"
    let fn = extractName n
    let (rt , [pt]) = typedic ! fn
    let expr = App (Con "boost::apply_visitor") (stmts ++ [exp])
    return [NoTemplate $ FuncDef (FuncSig rt fn [Param pt "_param"]) $ Block [Return expr]]


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

evalTran x = evalState (evalStateT x empty ) empty

main :: IO ()
main = do
    (input : _) <- getArgs
    str <- readFile input
    case parseModule str of
        ParseOk a -> do
            let (x,y) = evalTran (do
                (,) <$> tranlateHaskellToHPlusPlus a
                  <*> tranlateHaskellToCPlusPlus a)
            putStrLn $ evalState (showI x) ""
            putStrLn "//~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
            putStrLn $ evalState (showI y) ""
        ParseFailed loc str -> do
            print loc
            print str
