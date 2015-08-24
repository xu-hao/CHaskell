module CHaskell where

-- all polymorphic values must be given a type when used except when being applied
-- empty list must be given a type unless its type can be derived from the context
-- case must be given a type unless its type can be derived from the context
-- in let function binding must have type signature (with scoped type vars)
-- only monomorphic data type definition
-- only pattern match on one argument, all patterns must be of the form Cons Tvar ... Tvar
-- only unqualified types
-- every polymorphic list functions implemented here should be implemented for std::string in c++
-- supported operators : (+), (++), (,), (==), (/=)
-- supported functions : map, foldl, foldl1
-- supported types : Bool, Int, String, [a], a -> a, (a,...,a)
-- supported expressions : lambda, application, [ a,...,a ] (a,...,a), int, bool, string, case, let

import CPlusPlus
import Mapping
import HaskellSrc

import Prelude hiding (lookup)
import Language.Haskell.Parser
import Language.Haskell.Syntax
import System.Environment
import Control.Monad.State
import Data.List (sortBy)
import qualified Data.List as L
import Data.Map hiding (map, foldl, (!))
import Control.Applicative ((<$>),(<*>))
import Data.Maybe

(!) m k = fromMaybe (error $ "cannot find key " ++ show k) $ lookup k m

type DataDic = Map String (Int, [(Type, String)])
type TypeDic = Map String Type
type TranEnv = StateT DataDic (StateT TypeDic (StateT [[Stmt]] (StateT Type (StateT [String] IO))))

pushStmt :: Stmt -> TranEnv ()
pushStmt stmt = do
    stmts : stmtst <- lift . lift $ get
    lift . lift $ put ((stmts ++ [stmt]) : stmtst)

popStmts :: TranEnv [Stmt]
popStmts = do
    stmts : stmtst <- lift . lift $ get
    lift . lift $ put stmtst
    return stmts

block :: TranEnv Block
block = Block <$> popStmts

withBlock :: TranEnv a -> TranEnv Block
withBlock action = do
    stmts <- lift . lift $ get
    lift . lift $ put ([] : stmts)
    action
    block

withTypes :: [(String, Type)] -> TranEnv a -> TranEnv a
withTypes ts a = do
    typedic <- lift get
    lift $ put (fromList ts `union` typedic)
    r <- a
    lift $ put typedic
    return r

getCurrentType :: TranEnv Type
getCurrentType = lift . lift . lift $ get

withCurrentType :: Type -> TranEnv a -> TranEnv a
withCurrentType ty action = do
    ty0 <- getCurrentType
    lift . lift . lift $ put ty
    r <- action
    lift . lift . lift $ put ty0
    return r

getLocalVars :: TranEnv [String]
getLocalVars = lift . lift . lift . lift $ get

withLocalVars :: [String] -> TranEnv a -> TranEnv a
withLocalVars vns a = do
    vns0 <- getLocalVars
    lift . lift . lift . lift $ put $ vns0 `L.union` vns
    r <- a
    lift . lift . lift . lift $ put vns0
    return r

-- types
tranlateHaskellTypeToCPlusPlus (HsTyVar (HsIdent name)) = TVar name
tranlateHaskellTypeToCPlusPlus (HsTyCon (UnQual (HsIdent name))) = fromMaybe (TCon name) (lookup name primtypetable)
tranlateHaskellTypeToCPlusPlus (HsTyApp (HsTyCon (Special HsListCon)) ty) = TApp "std::vector" [tranlateHaskellTypeToCPlusPlus ty]
tranlateHaskellTypeToCPlusPlus (HsTyApp (HsTyCon (Special (HsTupleCon _))) ty) = TApp "std::tuple" [tranlateHaskellTypeToCPlusPlus ty]
tranlateHaskellTypeToCPlusPlus ty@(HsTyFun ty1 ty2) =
        let expr1 = tranlateHaskellTypeToCPlusPlus ty1
            expr2 = tranlateHaskellTypeToCPlusPlus ty2 in
            expr1 --> expr2
tranlateHaskellTypeToCPlusPlus ty = error (show ty)

-- exprs
wrapArg :: Expr -> TranEnv Expr
wrapArg arg@(Con v) = do
    typedic <- lift get
    return $ case lookup v typedic of
        Just t@(TFun _ _ _) -> App (Con "chaskell::function") [arg]
        _ -> arg
wrapArg arg@(Var v) = do
    typedic <- lift get
    return $ case lookup v typedic of
        Just t@(TFun _ _ _) -> App (Con "chaskell::function") [arg]
        _ -> arg
wrapArg arg@(Op2 v) = do
    typedic <- lift get
    return $ case lookup v typedic of
        Just t@(TFun _ _ pts) ->
            App (Con $ showII t)
                [Lam (zipWith Param pts ["_param1", "_param2"]) $ Block [Return $ App arg [Var "_param1", Var "_param2"]]]
wrapArg arg@(Op v) = do
    typedic <- lift get
    return $ case lookup v typedic of
        Just t@(TFun _ _ pts) ->
            App (Con $ showII t)
                [Lam (zipWith Param pts ["_param1"]) $ Block [Return $ App arg [Var "_param1"]]]
wrapArg arg = return arg

wrapLam rt pts lam = App (Con (showII (TFun [] rt pts))) [lam]

wrapExpr arg = do
    currty <- getCurrentType
    return $ case currty of
        TFun _ _ pts -> TypeApp arg pts
        _ -> arg

determineType :: Type -> Type
determineType pt1 = if L.null (vars pt1) then pt1 else TAuto
extractFromexpr :: Expr -> String
extractFromexpr expr = case expr of
        Var n -> n
        Con n -> n
        Op n -> n
        Op2 n -> n
translateHaskellExprToCPlusPlus :: HsExp -> TranEnv Expr
translateHaskellExprToCPlusPlus (HsCon n) = do
    let nn = extractQName n
    wrapExpr $ fromMaybe (Con nn) (lookup nn symtable)
translateHaskellExprToCPlusPlus (HsVar n) = do
    let nn = extractQName n
    wrapExpr $ fromMaybe (Var nn) (lookup nn symtable)
translateHaskellExprToCPlusPlus (HsLit (HsString s)) = return $ App (Con "std::string") [StrL s]
translateHaskellExprToCPlusPlus (HsLit (HsInt i)) = return $ IntL (fromIntegral i)
translateHaskellExprToCPlusPlus (HsInfixApp exp1 op exp2) = do
    let nn = extractQOp op
    let opc = fromMaybe (error $ "unsupported " ++ nn) $ lookup nn symtable
    typedic <- lift get
    let fn = extractFromexpr opc
    let (TFun _ rt [pt1, pt2]) = fromMaybe (error $ "translateHaskellExprToCPlusPlus (HsInfixApp):: cannot find type for " ++ fn) (lookup fn typedic)
        pt1' = determineType pt1
        pt2' = determineType pt2
    expr1 <- withCurrentType pt1' $ translateHaskellExprToCPlusPlus exp1
    expr2 <- withCurrentType pt2' $ translateHaskellExprToCPlusPlus exp2
    return $ App opc [expr1, expr2]
translateHaskellExprToCPlusPlus app@(HsApp exp1 exp2) = do
  transApp exp1 [exp2] where
      transApp (HsApp exp1 exp2) args = do
          transApp exp1 (exp2 : args)
      transApp exp1 args = do
          typedic <- lift get
          expr <- withCurrentType TAuto $ translateHaskellExprToCPlusPlus exp1
          let n = extractFromexpr expr
          let (TFun _ rt pts) = fromMaybe (error $ "translateHaskellExprToCPlusPlus(HsApp):: cannot find type for " ++ show n ++ " in " ++ show app) (lookup n typedic)
          let lpts = length pts
          let largs = length args
          let extraargs = map (\i -> "_param" ++ show i) [largs + 1 .. lpts]
          let argpts = take largs pts
          let extrapts = drop largs pts
          argsargs <- zipWithM transArg args (map determineType argpts)
          let allargs = argsargs ++ map Var extraargs
          let app = App expr allargs
          return $ if lpts /= largs
              then wrapLam rt extrapts (Lam (zipWith Param extrapts extraargs) (Block [Return app]))
              else app where
              transArg arg ty = withCurrentType ty $ do
                  arg <- translateHaskellExprToCPlusPlus arg
                  wrapArg arg
translateHaskellExprToCPlusPlus exp0@(HsLambda _ pats exp) = do
    currty <- getType exp0
    let (rt, pts) = case currty of
            TFun _ rt pts -> (rt, pts)
            _ -> (TAuto, replicate (length pats) TAuto)
    let vars = map (extractName . (\(HsPVar n) -> n)) pats
    block <- withBlock $ do
        expr <- withLocalVars (freeVars pats) $ withCurrentType rt $ translateHaskellExprToCPlusPlus exp
        pushStmt $ Return expr
    let lam = wrapLam rt pts (Lam (zipWith Param pts vars) block)
    return lam
translateHaskellExprToCPlusPlus (HsLet decls exp) = do
    withTypes [] $ do
        translateHaskellDeclListToHPlusPlus LetBinding decls
        defs <- translateHaskellDeclListToCPlusPlus LetBinding decls
        mapM_ (pushStmt . Def) defs
        typedic <- lift get
        withLocalVars (boundVars decls) $ translateHaskellExprToCPlusPlus exp
translateHaskellExprToCPlusPlus (HsIf exp1 exp2 exp3) = do
    b<- withCurrentType (TCon "bool") $ translateHaskellExprToCPlusPlus exp1
    e1<-translateHaskellExprToCPlusPlus exp2
    e2<-translateHaskellExprToCPlusPlus exp3
    return $ If b e1 e2
translateHaskellExprToCPlusPlus (HsCase exp alts) = do
    translateHaskellAltsToCPlusPlus exp alts

translateHaskellExprToCPlusPlus (HsDo stmts) = error "not supported"
translateHaskellExprToCPlusPlus exp0@(HsTuple exps) = do
    currty <- getType exp0
    let pts = case currty of
            TApp "std::tuple" pts -> pts
            _ -> replicate (length exps) TAuto
    App (Con "std::make_tuple") <$> zipWithM transComp exps pts where
        transComp exp pt = withCurrentType pt $ translateHaskellExprToCPlusPlus exp
translateHaskellExprToCPlusPlus exp0@(HsList exps) = do
    tyc0 <- getType exp0
    let tyc = case tyc0 of
                TApp "std::vector" [tyec] -> tyec
                _ -> TAuto
    exprs <- withCurrentType tyc $ mapM translateHaskellExprToCPlusPlus exps
    return $ InitApp (showII tyc0) exprs
translateHaskellExprToCPlusPlus (HsExpTypeSig _ exp (HsQualType _ ty)) =
    withCurrentType (tranlateHaskellTypeToCPlusPlus ty) $ translateHaskellExprToCPlusPlus exp
translateHaskellExprToCPlusPlus (HsParen exp) = translateHaskellExprToCPlusPlus exp
translateHaskellExprToCPlusPlus c = error $ "not supported " ++ show c

-- alts
translateHaskellAltsToCPlusPlus :: (FreeVars p, Pattern p) => HsExp -> [p] -> TranEnv Expr
translateHaskellAltsToCPlusPlus exp alts = do
    expr <- withCurrentType TAuto $ translateHaskellExprToCPlusPlus exp
    rt <- getCurrentType
    typedic <- lift get
    alts' <- sortAlts alts
    fdefs <- mapM (translateHaskellAltToCPlusPlus rt) alts'
    localvars <- getLocalVars
    let freevars = freeVars alts'
    let localvars' = L.filter (`L.elem` freevars) localvars
    let ty vn = fromMaybe (error $ "translateHaskellAltsToCPlusPlus:: cannot find type for local variable " ++ show vn) (lookup vn typedic)
    let ctorparam = map (\vn -> Param (ty vn) (pname vn)) localvars'
    let fields = map (\vn -> Field (ty vn) vn) localvars'
    let ctorinits = map (\vn -> Init vn (Var (pname vn))) localvars'
    let ctor = Ctor "" "visitor" ctorparam ctorinits (Block [])
    let stmt = StructDef "visitor"  [Public (TApp "boost::static_visitor" [rt])] (ctor : fields ++ map TemplateMember fdefs)
    pushStmt $ Def (NoTemplate stmt)
    return $ App (Con "boost::apply_visitor") [App (Con "visitor") (map Var localvars'), expr]


sortAlts :: Pattern p => [p] -> TranEnv [p]
sortAlts alts = do
    datadic <- get
    return (sortBy (\alt1 alt2 ->
        let cons1 = extractCons alt1
            cons2 = extractCons alt2
            (i1, _) = datadic ! cons1
            (i2, _) = datadic ! cons2 in
            compare i1 i2) alts)

removePParens (HsPParen pat) = pat
removePParens a = a

translateHaskellAltToCPlusPlus :: Pattern p => Type -> p -> TranEnv Template
translateHaskellAltToCPlusPlus rt p = do
    let cons = extractCons p
    block <- withBlock $ do
        let pat@(HsPApp n vs) = removePParens $ extractPat p
        stmts <- translateHaskellPatternToStmtsCpp (Var "_var") pat
        mapM_ pushStmt stmts
        datadic <- get
        let (_, fields) = datadic ! cons
        expr <- withTypes (concat $ zipWith (\(ty, n) pat ->case pat of
            HsPVar n -> [(extractName n, ty)]
            _ -> []) fields vs) $ withLocalVars (freeVars pat) $ translateHaskellExprToCPlusPlus (extractExp p)
        pushStmt (Return expr)
    return $ NoTemplate (FuncDef (FuncSig rt "operator()" [Param (TCon cons) "_var"] ["const"]) block)

translateHaskellPatternToStmtsCpp :: Expr -> HsPat -> TranEnv [Stmt]
translateHaskellPatternToStmtsCpp e (HsPVar n) = return [DefAssign TAuto (Var (extractName n)) e]
translateHaskellPatternToStmtsCpp e (HsPApp n pats) = do
    datadic <- get
    let (_, fields) = datadic ! extractQName n
    concat <$> mapM (\((_, field), pat) -> translateHaskellPatternToStmtsCpp (Dot e (mname field)) pat) (zip fields pats)
translateHaskellPatternToStmtsCpp e (HsPParen pat) = translateHaskellPatternToStmtsCpp e pat
translateHaskellPatternToStmtsCpp e (HsPWildCard) = return []

-- translateHaskellExprToStmtCPlusPlus (HsLet decls exp) = Block (translateHaskellDeclListToCPlusPlus decls) [Return (translateHaskellExprToCPlusPlus exp)]

-- extract
extractConstructorParam (name, ty) = Param (tranlateHaskellTypeToCPlusPlus ty) (pname name)


-- hpp
tranlateHaskellToHPlusPlus :: HsModule -> TranEnv Prog
tranlateHaskellToHPlusPlus (HsModule _ _ _ _ decls) =
    Prog ["\"chaskell.hpp\"", "<string>", "<vector>", "<utility>", "<boost/variant.hpp>"] <$>
        translateHaskellDeclListToHPlusPlus TopLevel decls
translateHaskellDeclListToHPlusPlus:: Mode -> [HsDecl] -> TranEnv [Template]
translateHaskellDeclListToHPlusPlus mode decls = concat <$> mapM (tranlateHaskellDeclsToHPlusPlus mode) decls

data Mode = TopLevel | LetBinding
tranlateHaskellDeclsToHPlusPlus:: Mode -> HsDecl -> TranEnv [Template]
tranlateHaskellDeclsToHPlusPlus mode (HsDataDecl _ _ (HsIdent name) tvars constructors _) = do
    let constructorNameAndFields = map extractConstructorNameAndFields constructors
        constructorNames = map fst constructorNameAndFields
        fwddecls = map (tranlateHaskellConsDeclsToFwdDeclsHPlusPlus mode) constructors
        tdef = NoTemplate (Typedef (variant (map TCon constructorNames)) name)
    datadic <- get
    put $ datadic `union` fromList (map (\(i, (ctor, fields)) -> (ctor, (i, map (\(n, ty) -> (tranlateHaskellTypeToCPlusPlus ty, n)) fields))) (zip [1..length constructorNameAndFields] constructorNameAndFields))
    return (fwddecls ++ (tdef : map (tranlateHaskellConsDeclsToHPlusPlus mode) constructors))

tranlateHaskellDeclsToHPlusPlus mode (HsTypeSig _ [n] (HsQualType _ ty)) = do
    typedic <- lift get
    let fn = extractName n
        ft = tranlateHaskellTypeToCPlusPlus ty
    lift $ put $ insert fn ft typedic
    return $ case ft of
        TFun vns rt pt ->
            let fundef = FuncProto $ FuncSig rt fn (map (\ty -> Param ty "") pt) [] in
                [if L.null vns
                    then NoTemplate fundef
                    else Template vns fundef
                    ]
        _ -> []
tranlateHaskellDeclsToHPlusPlus mode (HsForeignImport _ _ _ _ n ty) = do
    typedic <- lift get
    let fn = extractName n
    let ft@(TFun vns rt pt) = tranlateHaskellTypeToCPlusPlus ty
    lift $ put $ insert fn ft typedic
    let fundef = FuncProto $ FuncSig rt fn (map (\ty -> Param ty "") pt) []
    return [if L.null vns
        then NoTemplate fundef
        else Template vns fundef
        ]
tranlateHaskellDeclsToHPlusPlus _ (HsFunBind _) = return []
tranlateHaskellDeclsToHPlusPlus _ (HsPatBind _ _ _ _) = return []

tranlateHaskellConsDeclsToFwdDeclsHPlusPlus mode (HsRecDecl _ (HsIdent name) _) =
    NoTemplate (StructFwdDecl name)
tranlateHaskellConsDeclsToFwdDeclsHPlusPlus mode (HsConDecl _ (HsIdent name) _) =
    NoTemplate (StructFwdDecl name)

tranlateHaskellConsDeclsToHPlusPlus :: Mode -> HsConDecl -> Template
tranlateHaskellConsDeclsToHPlusPlus mode cons =
    let (name, fields2) = extractConstructorNameAndFields cons in
        tranlateHaskellConsDeclsToHPlusPlus2 name fields2

tranlateHaskellConsDeclsToHPlusPlus2 name fields2 =
    let params = map extractConstructorParam fields2
        inits = map extractConstructorInit fields2
        ctor = Ctor "explicit" name params inits (Block []) in
        NoTemplate (StructDef name [] (ctor : map tranlateHaskellRecFieldDeclsToHPlusPlus fields2))

tranlateHaskellRecFieldDeclsToHPlusPlus (i, ty) =
    Field (tranlateHaskellTypeToCPlusPlus ty) (mname i)

-- cpp
tranlateHaskellToCPlusPlus :: HsModule -> TranEnv Prog
tranlateHaskellToCPlusPlus (HsModule _ _ _ _ decls) =
    Prog ["\"chaskell.hpp\"", "<string>", "<vector>", "<utility>", "<boost/variant.hpp>"] <$>
        translateHaskellDeclListToCPlusPlus TopLevel decls

translateHaskellDeclListToCPlusPlus:: Mode -> [HsDecl] -> TranEnv [Template]
translateHaskellDeclListToCPlusPlus mode decls = concat <$> (sequence $ map (tranlateHaskellDeclsToCPlusPlus mode) decls)


tranlateHaskellDeclsToCPlusPlus mode bind@(HsPatBind _ _ _ _) = do
    typedic <- lift get
    let ps = []
    let n = case extractPat bind of HsPVar n -> n
    let fn = extractName n
    case lookup fn typedic of
        Just (TFun vns rt pts) -> do
            let match = transformPatBindToAlts bind
            tranlateHaskellMatchToCPlusPlus mode (n, match)
        Just ty -> do
            expr <- withCurrentType ty $ translateHaskellExprToCPlusPlus (extractExp bind)
            let vardef = VarDef ty fn expr
            return [NoTemplate vardef]
        Nothing -> do
            let exp = extractExp bind
            expr <- withCurrentType TAuto $ translateHaskellExprToCPlusPlus exp
            ty <- getType exp
            let vardef = VarDef ty fn expr
            return [NoTemplate vardef]
tranlateHaskellDeclsToCPlusPlus mode (HsFunBind matches) = do
    let curr0@(HsMatch _ currn0 _ _ _ ) = head matches
    let (curr, currn, groups) = foldl (\(curr,currn, prev) match@(HsMatch _ n _ _ _ )->
            if n == currn
                then (curr++[match],currn, prev)
                else ([match], n, prev++[(currn, curr)])) ([curr0], currn0, []) $ tail matches
    concat <$> mapM (tranlateHaskellGroupsToCPlusPlus mode) (groups++[(currn, curr) ])
tranlateHaskellDeclsToCPlusPlus _ _ = return []

tranlateHaskellGroupsToCPlusPlus :: Mode -> (HsName, [HsMatch]) -> TranEnv [Template]
tranlateHaskellGroupsToCPlusPlus mode (n, matches@[_]) = do
    let match = transformMatchesToAlts matches
    tranlateHaskellMatchToCPlusPlus mode (n, match)

tranlateHaskellGroupsToCPlusPlus mode (n, matches) = do
    let alts = transformMatchesToAlts matches
    tranlateHaskellMatchToCPlusPlus mode (n, alts)

tranlateHaskellMatchToCPlusPlus :: Mode -> (HsName, HsMatch) -> TranEnv [Template]
tranlateHaskellMatchToCPlusPlus mode (n, match) = do
    typedic <- lift get
    let ps = extractParams match
    let fn = extractName n
    let ft@(TFun vns rt pts) = fromMaybe (error $ "tranlateHaskellMatchToCPlusPlus:: cannot find type for " ++ show fn) (lookup fn typedic)
    let psl = length ps
    let ptsl = length pts
    let extra = map (HsVar . UnQual . HsIdent . ("_param" ++) . show) [psl + 1 .. ptsl]
    let extraps = map ( ("_param" ++) . show) [psl + 1 .. ptsl]
    let exp' = foldl HsApp (extractExp match) extra
    let ps' = ps++extraps
    block <- withBlock $ do
        expr <- withLocalVars ps $ withTypes (zip ps' pts) $ withCurrentType rt $ translateHaskellExprToCPlusPlus exp'
        pushStmt $ Return expr
    return $ case mode of
        TopLevel ->
            let fundef = FuncDef (FuncSig rt fn (zipWith Param pts ps') []) block in
                [if L.null vns
                    then NoTemplate fundef
                    else Template vns fundef ]
        LetBinding ->
            let lam = Lam (zipWith Param pts ps') block in
                [NoTemplate (VarDef ft fn lam)]

-- type
class TypeIt a where
    typeIt :: a -> TranEnv Type

mostSpecific (TFun v1 rt1 pts1) (TFun v2 rt2 pts2) = TFun (v1 `L.union` v2) (mostSpecific rt1 rt2) (zipWith mostSpecific pts1 pts2)
mostSpecific (TApp rt1 pts1) (TApp rt2 pts2) = TApp rt1 (zipWith mostSpecific pts1 pts2)
mostSpecific TAuto b = b
mostSpecific a _ = a

getType :: HsExp -> TranEnv Type
getType exp = do
    t1 <- getCurrentType
    t2 <- typeIt exp
    return $ mostSpecific t1 t2

instance TypeIt HsExp where
    typeIt (HsLit (HsString _)) = return $ TCon "std::string"
    typeIt (HsLit (HsInt _)) = return $ TCon "int"
    typeIt (HsCon (UnQual (HsIdent "True"))) = return $ TCon "bool"
    typeIt (HsCon (UnQual (HsIdent "False"))) = return $ TCon "bool"
    typeIt (HsCon n) = do
        typedic <- lift get
        let nn = extractQName n
        let nnn = extractFromexpr $ fromMaybe (Con nn) (lookup nn symtable)
        let ty = typedic ! nnn
        return $ if L.null (vars ty)
            then ty
            else TAuto
    typeIt (HsVar n) = do
        typedic <- lift get
        let nn = extractQName n
        let nnn = extractFromexpr $ fromMaybe (Var nn) (lookup nn symtable)
        let ty = typedic ! nnn
        return $ if L.null (vars ty)
            then ty
            else TAuto
    typeIt (HsParen exp) = typeIt exp
    typeIt (HsLambda _ pats exp) = do
        expt <- typeIt exp
        patst <- mapM typeIt pats
        return $ L.foldr (-->) expt patst
    typeIt (HsApp exp1 exp2) = do
        TFun _ rt _ <- typeIt exp1
        return rt
    typeIt (HsLet decls exp) =
        withTypes [] $ do
            mapM_ typeIt decls
            typeIt exp
    typeIt (HsCase exp alts) = do
        types <- mapM typeIt alts
        return $ foldl1 mostSpecific types
    typeIt (HsIf exp1 exp2 exp3) = mostSpecific <$> typeIt exp2 <*> typeIt exp3
    typeIt (HsInfixApp exp1 op exp2) = do
        typedic <- lift get
        let nn = extractQOp op
        let nnn = extractFromexpr $ fromMaybe (Op2 nn) (lookup nn symtable)
        let TFun _ rt _ = typedic ! nnn
        return $ if L.null (vars rt)
            then rt
            else TAuto
    typeIt (HsExpTypeSig _ _ (HsQualType _ ty)) = return $ tranlateHaskellTypeToCPlusPlus ty
    typeIt (HsTuple exps) = do
        types <- mapM typeIt exps
        return $ TApp "std::tuple" types
    typeIt (HsList exps) = do
        types <- mapM typeIt exps
        return $ TApp "std::vector" [foldl mostSpecific TAuto types]
instance TypeIt HsAlt where
    typeIt (HsAlt _ pat (HsUnGuardedAlt exp) _) =
        withTypes [] $ do
            typeIt pat
            typeIt exp
instance TypeIt HsPat where
    typeIt (HsPParen pat) = typeIt pat
    typeIt (HsPVar _) = return TAuto
    typeIt (HsPWildCard) = return TAuto
    typeIt (HsPApp n vs) = do
        datadic <- get
        let nn = extractQName n
        let (_, types) = datadic ! nn
        let typesctx = concat $ zipWith (\pat (ty, _) -> case pat of
                HsPVar n1 -> [(extractName n1, ty)]
                _ -> [] ) vs types
        typedic <- lift get
        lift $ put $ typedic `union` fromList typesctx
        return TAuto
instance TypeIt HsDecl where
    typeIt (HsTypeSig _ [n] (HsQualType _ ty)) = do
        typedic <- lift get
        lift $ put $ typedic `union` fromList [(extractName n, tranlateHaskellTypeToCPlusPlus ty)]
        return TAuto
    typeIt (HsPatBind _ (HsPVar n) (HsUnGuardedRhs exp) _) = do
        typedic <- lift get
        ty <- typeIt exp
        lift $ put $ typedic `union` fromList [(extractName n, ty)]
        return TAuto
    typeIt _ = return TAuto

evalTran :: TranEnv a -> IO a
evalTran x = evalStateT (evalStateT (evalStateT (evalStateT (evalStateT x empty ) typetable) []) TAuto) []

main :: IO ()
main = do
    (input : _) <- getArgs
    str <- readFile input
    case parseModule str of
        ParseOk a -> do
            (x,y) <- evalTran (do
                h <- tranlateHaskellToHPlusPlus a
                c <- tranlateHaskellToCPlusPlus a
                return (h,c))
            putStrLn $ evalState (showI x) ""
            putStrLn $ "//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
            putStrLn $ evalState (showI y) ""
        ParseFailed loc str -> do
            print loc
            print str
