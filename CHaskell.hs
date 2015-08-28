{-# LANGUAGE FlexibleInstances , MultiParamTypeClasses #-}

module CHaskell where

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
import HaskellSrc hiding (StrL, IntL)
import qualified HaskellSrc as HS

import Prelude hiding (lookup)
import Language.Haskell.Parser
import Language.Haskell.Syntax
import System.Environment
import Control.Monad.State
import Data.List (sortBy)
import Data.Char (isUpper)
import qualified Data.List as L
import Data.Map hiding (map, foldl)
import qualified Data.Map as M
import Control.Applicative ((<$>),(<*>))
import Data.Maybe
import Data.Tree
import Debug.Trace

type DataDic = Map String [(String, Type)]
type TypeDic = Map String TypeScheme
type CurryMap = Map String Int

type TranEnv = StateT DataDic (StateT [[Stmt]] (StateT (Map String Type) IO))
type TypeEnv = StateT Int (StateT DataDic IO)
type OptEnv = StateT Int (StateT [[Stmt]] (StateT CurryMap IO))

setCurry :: String -> Int -> OptEnv ()
setCurry n i = do
    currymap <- lift $ lift get
    lift $ lift $ put $ insert n i currymap

getCurry :: String -> OptEnv (Maybe Int)
getCurry n = do
    currymap <- lift $ lift get
    return $ lookup n currymap
pushStmt0 :: (Monad m, Functor m) => Stmt -> StateT [[Stmt]] m ()
pushStmt0 stmt = do
    stmts : stmtst <- get
    put ((stmts ++ [stmt]) : stmtst)

pushStmt :: (Monad m, Functor m) => Stmt -> StateT a (StateT [[Stmt]] m) ()
pushStmt stmt =
    lift $ pushStmt0 stmt

popStmts0 :: (Monad m, Functor m) => StateT [[Stmt]] m [Stmt]
popStmts0 = do
    stmts : stmtst <- get
    put stmtst
    return stmts

popStmts :: (Monad m, Functor m) => StateT a (StateT [[Stmt]] m) [Stmt]
popStmts = lift popStmts0

peekStmts0 :: (Monad m, Functor m) => StateT [[Stmt]] m [Stmt]
peekStmts0 = do
    stmts : _ <- get
    return stmts
peekStmts :: (Monad m, Functor m) => StateT a (StateT [[Stmt]] m) [Stmt]
peekStmts = lift peekStmts0

renameStmts0 :: (Monad m, Functor m) => Renaming -> StateT [[Stmt]] m ()
renameStmts0 r = do
    stmtss <- get
    put $ rename r stmtss
renameStmts :: (Monad m, Functor m) => Renaming -> StateT a (StateT [[Stmt]] m) ()
renameStmts r = lift $ renameStmts0 r

block0 :: (Monad m, Functor m) => StateT [[Stmt]] m Block
block0 = Block <$> popStmts0

block :: (Monad m, Functor m) => StateT a (StateT [[Stmt]] m) Block
block = lift block0

withBlock0 :: (Functor m, Monad m) => StateT [[Stmt]] m a -> StateT [[Stmt]] m Block
withBlock0 action = do
    stmts <- get
    put ([] : stmts)
    action
    block0

withBlock :: (Functor m, Monad m) => StateT a (StateT [[Stmt]] m) b => StateT a (StateT [[Stmt]] m) Block
withBlock action = do
    stmts <- lift get
    lift $ put ([] : stmts)
    action
    block

getLocalVars :: TranEnv (Map String Type)
getLocalVars = lift . lift $ get

withLocalVars :: Map String Type -> TranEnv a -> TranEnv a
withLocalVars vns a = do
    vns0 <- getLocalVars
    lift . lift $ put $ vns `union` vns0
    r <- a
    lift . lift $ put vns0
    return r

nextVid :: Monad m => StateT Int m Int
nextVid = do
    vid <- get
    put (vid + 1)
    return vid


-- exprs
wrapLam rt pts lam = App (tToE (wrapTFun (curryType rt pts))) [lam]

unwrapLam (App (AppT "std::function" _) [lam]) = lam
unwrapLam expr = expr

curryType :: Type -> [Type] -> Type
curryType rt pts = L.foldr (-->) rt pts

uncurryExpr :: [(Type, String)] -> Expr -> ([String], Block)
uncurryExpr [] expr =
    ([], Block [Return expr])
uncurryExpr [_] (Lam [Param ty n] block) =
    ([n], block)
uncurryExpr (_:t) (Lam [Param ty n] (Block [Return expr])) =
    let (ps, block) = uncurryExpr t (unwrapLam expr) in
        (n : ps, block)
uncurryExpr ((ty, n):t) e =
    let (ps, Block [Return expr]) = uncurryExpr t e in
        (n : ps, Block [Return $ App expr [Var n ty]])

curryExpr rt fields e = snd $ L.foldr (\(n, ty) (rt', e') -> (ty --> rt', wrapLam rt' [ty] (Lam [Param ty n] $ Block [Return e']) )) (rt, App e (map (uncurry Var) fields)) fields
translateHaskellExprToCPlusPlus :: TypeExp -> TranEnv Expr
translateHaskellExprToCPlusPlus (ty, VarE n) = do
    localvars <- getLocalVars
    if isUpper $ head n
        then do
            -- constructor
            let (rt@(TCon n2), pts) = uncurryType ty
            let ps = ["_p"++show i | i<- [1..length pts]]
            let args = zipWith Var ps pts
            let params = zipWith Param pts ps
            let expr = snd $ L.foldr (\p@(Param ty _ ) (rt', expr') -> (ty --> rt', wrapLam rt' [ty] $ Lam [p] $ Block [Return expr'])) (rt, App (Var n2 ty) [App (Var n TAuto) args]) params -- same here
            return expr
        else
            return $ fromMaybe (Var n) (lookup n symtable) ty
translateHaskellExprToCPlusPlus (ty, LitE (HS.StrL s)) =
    return $ App (Con "chaskell::vectorchar") [StrL s]
translateHaskellExprToCPlusPlus (ty, LitE (HS.IntL i)) =
    return $ IntL (fromIntegral i)
translateHaskellExprToCPlusPlus (ty, AppE exp1 exp2) = do
    expr1 <- translateHaskellExprToCPlusPlus exp1
    expr2 <- translateHaskellExprToCPlusPlus exp2
    return $ App expr1 [expr2]
  -- transApp exp1 [exp2] where
  --     transApp (ty, AppE exp1 exp2) args = do
  --         transApp exp1 (exp2 : args)
  --     transApp exp1@(ty, _) args = do
  --         expr <- translateHaskellExprToCPlusPlus exp1
  --         let (rt, pts) = uncurryType ty
  --         let lpts = length pts
  --         let largs = length args
  --         let extraargs = map (\i -> "_param" ++ show i) [largs + 1 .. lpts]
  --         let argpts = take largs pts
  --         let extrapts = drop largs pts
  --         argsargs <- zipWithM transArg args argpts
  --         let allargs = argsargs ++ map Var extraargs
  --         let app = App expr allargs
  --         return $ if lpts /= largs
  --             then wrapLam rt extrapts (Lam (zipWith Param extrapts extraargs) (Block [Return app]))
  --             else app where
  --             transArg arg ty = do
  --                 arg <- translateHaskellExprToCPlusPlus arg
  --                 return $ wrapArg ty arg
translateHaskellExprToCPlusPlus (ty@(TFun rt pt), LamE vn exp) = do
    block <- withBlock $ do
        expr <- withLocalVars (singleton vn pt) $ translateHaskellExprToCPlusPlus exp
        pushStmt $ Return expr
    return $ wrapLam rt [pt] (Lam [Param pt vn] block)
translateHaskellExprToCPlusPlus (ty, LetE decls exp) = do
    translateHaskellDeclListToHPlusPlus LetBinding decls
    defs <- translateHaskellDeclListToCPlusPlus LetBinding decls
    mapM_ (pushStmt . Def) defs
    withLocalVars (declsVarmap decls) $ translateHaskellExprToCPlusPlus exp
translateHaskellExprToCPlusPlus (ty, IfE exp1 exp2 exp3) = do
    b<- translateHaskellExprToCPlusPlus exp1
    e1<-translateHaskellExprToCPlusPlus exp2
    e2<-translateHaskellExprToCPlusPlus exp3
    return $ If b e1 e2
translateHaskellExprToCPlusPlus (ty, CaseE exp alts) =
    translateHaskellAltsToCPlusPlus exp alts

-- translateHaskellExprToCPlusPlus (Node (HsDo _ _, ty) stmts) = error "not supported"
translateHaskellExprToCPlusPlus (ty, TupleE exps) =
    App (Con "std::make_tuple") <$> mapM translateHaskellExprToCPlusPlus exps
translateHaskellExprToCPlusPlus (ty, ListE exps) = do
    exprs <- mapM translateHaskellExprToCPlusPlus exps
    return $ InitApp (tToE (wrapTFun ty)) exprs
translateHaskellExprToCPlusPlus (ty, TypeSigE exp _) =
    translateHaskellExprToCPlusPlus exp

-- alts
translateHaskellAltsToCPlusPlus :: TypeExp -> [Alt] -> TranEnv Expr
translateHaskellAltsToCPlusPlus exp@(ty1, _) alts@(Alt _ (ty, _) : _) = do
    expr <- translateHaskellExprToCPlusPlus exp
    fdefs <- mapM (translateHaskellAltToCPlusPlus ty1) alts
    localvarsandtypes <- getLocalVars
    let freevars = freeVars alts
    let localvars' = L.filter (\(vn, ty) -> vn `L.elem` freevars) (toList localvarsandtypes)
    let ctorparam = map (\(vn,ty) -> Param ty (pname vn)) localvars'
    let fields = map (\(vn,ty) -> Field ty vn) localvars'
    let ctorinits = map (\(vn,ty) -> Init vn (Var (pname vn) ty)) localvars'
    let ctor = Ctor "" "visitor" ctorparam ctorinits (Block [])
    let stmt = StructDef "visitor"  [Public (TApp "boost::static_visitor" [ty])] (ctor : fields ++ map TemplateMember fdefs)
    pushStmt $ Def (NoTemplate stmt)
    return $ App (Con "boost::apply_visitor") [App (Con "visitor") (map (uncurry Var) localvars'), expr]


translateHaskellAltToCPlusPlus :: Type -> Alt -> TranEnv Template
translateHaskellAltToCPlusPlus ty alt@(Alt pat@(AppP cons pats) exp@(ty2, _)) = do
    block <- withBlock $ do
        (r, varmap) <- translateHaskellPatternToStmtsCpp ty (Var "_var" ty) pat
        expr <- withLocalVars varmap $ translateHaskellExprToCPlusPlus exp
        stmts <- peekStmts
        let (r', map2) = partitionWithKey (\n _ -> ocurrences n stmts + ocurrences n expr <= 1) r
        mapM_ (\(n, e) -> pushStmt $ Def $ NoTemplate $ VarDef (varmap ! n) n e) $ toList map2
        renameStmts r'
        pushStmt (Return $ rename r' expr)
    return $ NoTemplate (FuncDef (FuncSig ty2 "operator()" [Param (TCon cons) "_var"] ["const"]) block)

translateHaskellPatternToStmtsCpp :: Type -> Expr -> Pat -> TranEnv (Renaming, Map String Type)
translateHaskellPatternToStmtsCpp ty e (VarP n2) = return (singleton n2 e, singleton n2 ty)
translateHaskellPatternToStmtsCpp ty e (AppP n pats) =
    foldM (\(r, varmap) (n, ty, pat) -> do
        (r', varmap') <- translateHaskellPatternToStmtsCpp ty (Dot e (mname n)) pat
        return (r `union` r', varmap `union` varmap')) (empty, empty) pats
translateHaskellPatternToStmtsCpp _ _ (WildCardP) = return (empty, empty)

-- extract
extractConstructorParam (name, ty) = Param ty (pname name)


-- hpp
tranlateHaskellToHPlusPlus :: [Decl] -> TranEnv Prog
tranlateHaskellToHPlusPlus decls =
    Prog ["\"chaskell.hpp\"", "<string>", "<vector>", "<utility>", "<boost/variant.hpp>"] <$>
        translateHaskellDeclListToHPlusPlus TopLevel decls
translateHaskellDeclListToHPlusPlus :: Mode -> [Decl] -> TranEnv [Template]
translateHaskellDeclListToHPlusPlus mode decls = concat <$> mapM (tranlateHaskellDeclsToHPlusPlus mode) decls

data Mode = TopLevel | LetBinding deriving Eq

uncurryType :: Type -> (Type, [Type])
uncurryType (TFun rt pt) =
    let (rt', pts') = uncurryType rt in
        (rt', pt : pts')
uncurryType ty = (ty, [])

tranlateHaskellDeclsToHPlusPlus:: Mode -> Decl -> TranEnv [Template]
tranlateHaskellDeclsToHPlusPlus mode (DataD name tvars constructors) = do
    let constructorNames = map (\(ConD n _) -> n) constructors
        fwddecls = map (tranlateHaskellConsDeclsToFwdDeclsHPlusPlus mode) constructors
        tdef = NoTemplate (Typedef (variant (map TCon constructorNames)) name)
    return (fwddecls ++ (tdef : map (tranlateHaskellConsDeclsToHPlusPlus mode) constructors))

tranlateHaskellDeclsToHPlusPlus mode (TypeSigD n ty) = do
    let (rt, pts) = uncurryType ty
    let fundef = VarProto (wrapTFun ty) n
    let vns = vars ty in
        return [if L.null vns || mode == LetBinding
            then NoTemplate fundef
            else Template vns fundef
            ]
tranlateHaskellDeclsToHPlusPlus _ (BindD _ _) = return []

tranlateHaskellConsDeclsToFwdDeclsHPlusPlus mode (ConD name _) =
    NoTemplate (StructFwdDecl name)

tranlateHaskellConsDeclsToHPlusPlus :: Mode -> ConD -> Template
tranlateHaskellConsDeclsToHPlusPlus mode cons@(ConD name fields) =
    let params = map extractConstructorParam fields
        inits = map extractConstructorInit fields
        ctor = Ctor "explicit" name params inits (Block []) in
        NoTemplate (StructDef name [] (ctor : map tranlateHaskellRecFieldDeclsToHPlusPlus fields))

tranlateHaskellRecFieldDeclsToHPlusPlus :: (String, Type) -> Member
tranlateHaskellRecFieldDeclsToHPlusPlus (i, ty) =
    Field ty (mname i)

-- cpp
tranlateHaskellToCPlusPlus :: [Decl] -> TranEnv Prog
tranlateHaskellToCPlusPlus decls =
    Prog ["\"chaskell.hpp\"", "<string>", "<vector>", "<utility>", "<boost/variant.hpp>"] <$>
        translateHaskellDeclListToCPlusPlus TopLevel decls

translateHaskellDeclListToCPlusPlus:: Mode -> [Decl] -> TranEnv [Template]
translateHaskellDeclListToCPlusPlus mode decls = concat <$> mapM (tranlateHaskellDeclsToCPlusPlus mode) decls


tranlateHaskellDeclsToCPlusPlus mode (BindD n exp@(ty, _)) = do
    expr <- translateHaskellExprToCPlusPlus exp
    let vardef = VarDef (wrapTFun ty) n expr
    let vns = vars ty in
        return [if L.null vns || mode == LetBinding
            then NoTemplate vardef
            else Template vns vardef
            ]
tranlateHaskellDeclsToCPlusPlus _ _ = return []

-- tranlateHaskellGroupsToCPlusPlus :: Mode -> (HsName, [TypeTree]) -> TranEnv [Template]
-- tranlateHaskellGroupsToCPlusPlus mode (n, matches@[_]) = do
--     let match = transformMatchesToAlts matches
--     tranlateHaskellMatchToCPlusPlus mode (n, match)
--
-- tranlateHaskellGroupsToCPlusPlus mode (n, matches) = do
--     let alts = transformMatchesToAlts matches
--     tranlateHaskellMatchToCPlusPlus mode (n, alts)
--
-- tranlateHaskellMatchToCPlusPlus :: Mode -> (HsName, TypeTree) -> TranEnv [Template]
-- tranlateHaskellMatchToCPlusPlus mode (n, match@(Node (_, ty) (exp:pats))) = do
--     let ps = map (\(Node ( (HsPVar n), _) _) -> extractName n) pats
--     let fn = extractName n
--     let (rt, pts) = uncurried ty
--     let psl = length ps
--     let ptsl = length pts
--     let extra = map (HsVar . UnQual . HsIdent . ("_param" ++) . show) [psl + 1 .. ptsl]
--     let extraps = map ( ("_param" ++) . show) [psl + 1 .. ptsl]
--     let exp' = foldl (\n@(Node ( e, TFun rt pt) _) p->
--             let e' = HsApp e p in
--                 Node ( e', rt) [n, Node ( p, pt) []]) exp extra
--     let ps' = ps++extraps
--     block <- withBlock $ do
--         expr <- withLocalVars ps $ translateHaskellExprToCPlusPlus exp'
--         pushStmt $ Return expr
--     return $ case mode of
--         TopLevel ->
--             let fundef = FuncDef (FuncSig rt fn (zipWith Param pts ps') []) block
--                 vns = vars ty in
--                 [if L.null vns
--                     then NoTemplate fundef
--                     else Template vns fundef ]
--         LetBinding ->
--             let lam = Lam (zipWith Param pts ps') block in
--                 [NoTemplate (VarDef ty fn lam)]

-- type
type Substitution = Map String Type
class Subst a where
    subst :: Substitution -> a -> a

rigid :: Type -> Bool
rigid (TCon ('*':_)) = True
rigid (TApp n tys) = any rigid tys
rigid (TFun rt pt) = any rigid [rt, pt]
rigid _ = False

class Unify a where
    unify :: a -> a -> Maybe Substitution

instance Unify Type where
    unify TAuto TAuto = Just empty
    unify (TCon n1) (TCon n2) | n1 == n2 = Just empty
                              | otherwise = Nothing
    unify (TApp tc1 ts1) (TApp tc2 ts2) | tc1 == tc2 = unify ts1 ts2
                                        | otherwise = Nothing
    unify (TFun t11 t12) (TFun t21 t22) = unify [t11, t12] [t21, t22]
    unify (TVar n) t = do
        guard $ not (rigid t)
        Just $ singleton n t
    unify t (TVar n) = do
        guard $ not (rigid t)
        Just $ singleton n t
    unify _ _ = Nothing

instance (Subst a, Unify a, ShowI a) => Unify [a] where
    unify [] [] = Just empty
    unify [] (_ : _) = Nothing
    unify (_ : _) [] = Nothing
    unify (t1:ts1) (t2:ts2) = do
        s <- unify t1 t2
        s2 <- unify (subst s ts1) (subst s ts2)
        return (subst s2 s)

unify2 t t2 = do
    s <- unify t t2
    return (s, subst s t)

unifyTS :: TypeScheme -> TypeScheme -> TypeEnv (Maybe (TypeScheme, Substitution))
unifyTS (TypeScheme vs t) (TypeScheme vs2 t2) = do
    let n = length vs
    if n /= length vs2
        then return Nothing
        else do
            rigids <- replicateM n newRigid
            let s1 = fromList $ zip vs rigids
            let s2 = fromList $ zip vs2 rigids
            return $ do
                s <- unify (subst s1 t) (subst s2 t2)
                return (TypeScheme vs (subst s t), s)

instance Subst Type where
    subst _ t@TAuto = t
    subst _ t@(TCon _) = t
    subst s t@(TVar n) = fromMaybe t (lookup n s)
    subst s (TApp tc ts) = TApp tc (map (subst s) ts)
    subst s (TFun t1 t2) =
        let t1s = subst s t1
            t2s = subst s t2 in
            TFun t1s t2s

instance Subst TypeDic where
    subst s = M.map (subst s)

instance Subst TypeScheme where
    subst s (TypeScheme vs ty) = TypeScheme vs (subst (L.foldr delete s vs) ty)

instance Subst a => Subst [a] where
    subst s = map (subst s)

instance Subst TypeExp where
    subst s (ty, exp) = (subst s ty, subst s exp)

instance Subst Exp where
  subst s (AppE exp1 exp2) = AppE (subst s exp1) (subst s exp2)
  subst s (LetE decls exp) = LetE (subst s decls) (subst s exp)
  subst s (CaseE exp alts) = CaseE (subst s exp) (subst s alts)
  subst s (IfE exp1 exp2 exp3) = IfE (subst s exp1) (subst s exp2) (subst s exp3)
  subst s (TypeSigE exp ty) = TypeSigE (subst s exp) ty
  subst s (TupleE exps) = TupleE (subst s exps)
  subst s (ListE exps) = ListE (subst s exps)
  subst s (LamE ps block) = LamE ps (subst s block)
  subst _ e = e

instance Subst Alt where
    subst s (Alt pat exp) = Alt (subst s pat) (subst s exp)

instance Subst Decl where
    subst s (BindD n exp) = BindD n (subst s exp)
    subst _ decl = decl

instance Subst Pat where
    subst _ pat = pat

instance Subst Substitution where
    subst s sub = M.map (subst s) sub `union` s



instantiate :: TypeScheme -> TypeEnv Type
instantiate (TypeScheme vs ty) = do
    vids <- replicateM (length vs) nextVid
    let s = fromList $ zip vs (map (TVar . show) vids)
    return $ subst s ty

extractType typedic n = do
    let ty = fromMaybe (error $ "extractType:: cannot find type for symbol " ++ show n) $ lookup n typedic
    instantiate ty

typeExp :: TypeDic -> TypeExp -> TypeEnv (TypeExp, Substitution)
typePat :: TypeDic -> Pat -> TypeEnv (TypeDic, Type, Pat)
typePats :: TypeDic -> [Pat] -> TypeEnv (TypeDic, [Type], [Pat])
typeDecl :: TypeDic -> Decl -> TypeEnv (TypeDic, Decl, Substitution)
typeDecls :: TypeDic -> [Decl] -> TypeEnv (TypeDic, [Decl], Substitution)
typeAlt :: TypeDic -> Type -> Alt -> TypeEnv (Type, Alt, Substitution)
typeAlts :: TypeDic -> Type -> [Alt] -> TypeEnv (Type, [Alt], Substitution)

newTVar :: TypeEnv Type
newTVar = do
    vid <- nextVid
    return (TVar (show vid))
newRigid :: TypeEnv Type
newRigid = do
    vid <- nextVid
    return (TCon ("*" ++ show vid))
getDataDic :: TypeEnv DataDic
getDataDic = lift get

addDataDef :: String -> [(String, Type)] -> TypeEnv ()
addDataDef n fields = do
    datadic <- getDataDic
    lift $ put (insert n fields datadic)

typeExp _ (_, e@(LitE (HS.StrL _))) = return ((TApp "std::vector" [TCon "char"], e), empty)
typeExp _ (_, e@(LitE (HS.IntL _))) = return ((TCon "int", e), empty)
typeExp _ (_, e@(VarE "True")) = return ((TCon "bool", e), empty)
typeExp _ (_, e@(VarE "False")) = return ((TCon "bool", e), empty)
typeExp typedic (_, e@(VarE n)) = do
    nty <- extractType typedic n
    return ((nty, e), empty)
typeExp typedic (_, LamE n exp) = do
    ty <- newTVar
    (exp'@(ty2, _), s) <- typeExp (insert n (TypeScheme [] ty) typedic) exp
    return ((subst s ty --> ty2, LamE n exp'), s)
typeExp typedic (_, AppE exp1 exp2) = do
    -- trace "===================AppE==================" $ return ()
    ([exp1'@(ty1, _), exp2'@(ty2, _)], s1) <- typeExps typedic [exp1, exp2]
    -- trace ((showII ty1)) $ return ()
    -- trace ((showII ty2)) $ return ()
    ty <- newTVar
    let (s2, TFun rt _) = fromMaybe (error $ "cannot unify " ++ showII ty1 ++ " and " ++ showII (ty2 --> ty)) $ unify2 ty1 (ty2 --> ty)
    -- (trace $ show s2) $ return ()
    -- trace "\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"AppE\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"" $ return ()
    return ((rt, AppE (subst s2 exp1') (subst s2 exp2')), subst s2 s1)
typeExp typedic (_, LetE decls exp) = do
    (typedic', decls', s) <- typeDecls typedic decls
    (exp'@(ty, _), s2) <- typeExp typedic' exp
    return ((ty, LetE (subst s2 decls') exp'), subst s2 s)
typeExp typedic (_, CaseE exp alts) = do
    (exp'@(ty, _), s) <- typeExp typedic exp
    (ty2, alts', s2) <- typeAlts (subst s typedic) ty alts
    return ((ty2, CaseE (subst s2 exp') alts'), subst s2 s)
typeExp typedic (_, IfE exp1 exp2 exp3) = do
    ([exp1'@(ty1, _), exp2'@(ty2, _), exp3'@(ty3, _)], s) <- typeExps typedic [exp1, exp2, exp3]
    let s2 = fromJust $ unify ty1 (TCon "bool")
    let ty2' = subst s2 ty2
    let ty3' = subst s2 ty3
    let (s3, rt) = fromJust $ unify2 ty2' ty3'
    return ((rt, IfE (subst s3 (subst s2 exp1')) (subst s3 (subst s2 exp2')) (subst s3 (subst s2 exp3'))), subst s3 (subst s2 s))
typeExp typedic (_, TypeSigE exp ty) = do
    let vs = vars ty
    nty <- instantiate (TypeScheme vs ty)
    (exp'@(ty2, _), s) <- typeExp typedic exp
    let (s2, ty3) = fromJust $ unify2 nty ty2
    return ((ty3, TypeSigE exp' ty), subst s2 s)
typeExp typedic (_, TupleE exps) = do
    (exps', s) <- typeExps typedic exps
    let pts = map (\(ty, _) -> ty) exps'
    return ((TApp "std::tuple" pts, TupleE exps'), s)
typeExp typedic (_, ListE exps) = do
    (exps', s) <- typeExps typedic exps
    let pts = map (\(ty, _) -> ty) exps'
    case pts of
        [] -> do
            ty2 <- newTVar
            return ((TApp "std::vector" [ty2], ListE exps'), s)
        _ -> do
            let (s2, ty2:_) = fromJust $ unify2 (take (length pts - 1) pts) (drop 1 pts)
            return ((TApp "std::vector" [ty2], ListE (subst s2 exps')), subst s2 s)

typeExps typedic = foldM (\(exps,s) exp -> do
    (exp',s2) <- typeExp (subst s typedic) exp
    return (subst s2 exps ++ [exp'], subst s2 s)) ([], empty)

typeAlt typedic ty (Alt pat exp) = do
    (typedic', ty1, pat') <- typePat typedic pat
    let s = fromJust $ unify ty ty1
    (exp'@(ty2, _), s2) <- typeExp (subst s typedic') exp
    return (ty2, Alt pat' exp', subst s2 s)

typeAlts typedic ty alts = do
    t0 <- newTVar
    foldM (\(t1, ns,s) e -> do
        (t2, n, s2) <- typeAlt (subst s typedic) (subst s ty) e
        let  (s3, t3) = fromJust $ unify2 t1 t2
        return (t3, subst s3 (subst s2 ns) ++ [n], subst s3 (subst s2 s))) (t0, [], empty) alts

typePat typedic pat@(VarP n) = do
    ty <- newTVar
    return (insert n (TypeScheme [] ty) typedic, ty, pat)
typePat typedic pat@WildCardP = do
    ty <- newTVar
    return (typedic, ty, pat)
typePat typedic (AppP n vs) = do
    datadic <- getDataDic
    ty <- extractType typedic n
    let (rt, pts) = uncurryType ty
    let fields = fromMaybe (error $ "cannot find data constructor " ++ show n) $ lookup n datadic
    let pats = map (\(_, _, pat) -> pat) vs
    (typedic', pts', pats') <- typePats typedic pats
    let s = fromJust $ unify pts pts'
    return (subst s typedic', subst s rt, AppP n (zipWith (\(n, ty) pat ->(n, ty, pat)) fields pats'))

typePats typedic = foldM (\(typedic',tys, ns) e -> do
    (typedic'',ty, n) <- typePat typedic' e
    return (typedic'', tys ++ [ty], ns ++ [n])) (typedic, [], [])

typeDecl typedic decl@(TypeSigD n ty) = do
    let vs = vars ty
    return (insert n (TypeScheme vs ty) typedic, decl, empty)
typeDecl typedic (BindD n exp) = do
    (exp'@(ty, e), s) <- typeExp typedic exp
    case lookup n typedic of
        Nothing -> do
            let ts = TypeScheme [] ty
            return (insert n ts (subst s typedic), BindD n exp', s)
        Just ts2 ->
            return (insert n ts2 (subst s typedic), BindD n exp', s)

typeDecl typedic decl@(DataD nt dvars constructors) = do
    let constructorNames = map (\(ConD n _)-> n) constructors
    datadic <- getDataDic
    mapM_ (\(ConD n fields)->addDataDef n fields) constructors
    let types = map (\(ConD n fields) ->
            let pts = map (\(n,ty)->ty) fields
                ft = L.foldr (-->) (TCon nt) pts in
                (n, TypeScheme (vars ft) ft)) constructors
    let typedic' = typedic `union` fromList types
    return (typedic', decl, empty)

typeDecls typedic = foldM (\(typedic', decls, s) decl -> do
    (typedic'', decl', s2) <- typeDecl typedic' decl
    return (typedic'', subst s2 decls ++ [decl'], subst s2 s)) (typedic, [], empty)

typeIt :: [Decl] -> IO [Decl]
typeIt decls = evalStateT (evalStateT (do
    (_, decls', _) <- typeDecls typetable decls
    return decls') 1) empty

type Renaming = Map String Expr
class Rename a where
    rename :: Renaming -> a -> a
renameName r n def = fromMaybe def (lookup n r)
instance Rename Expr where
    rename r (App expr1 exprs2) =
        let expr1' = rename r expr1
            exprs2' = rename r exprs2 in
            App expr1' exprs2'
    rename r (InitApp expr1 exprs2) =
        let expr1' = rename r expr1
            exprs2' = rename r exprs2 in
            InitApp expr1' exprs2'
    rename r (Lam ps block) =
        let r' = foldl (\r' (Param _ n) -> delete n r') r ps
            block' = rename r' block in
            Lam ps block'
    rename r expr@(Var n ty) =
        renameName r n expr
    rename r expr@(Con _) = expr
    rename r expr@(AppT _ _) =  expr
    rename r expr@(IntL _) =  expr
    rename r expr@(StrL _) =  expr
    rename r expr@(BoolL _) =  expr
    rename r expr@(Op _ _) =  expr
    rename r expr@(Op2 _ _) =  expr
    rename r (If expr1 expr2 expr3) =
        let expr1' = rename r expr1
            expr2' = rename r expr2
            expr3' = rename r expr3 in
            If expr1' expr2' expr3'
    rename r (Dot expr n) =
        let expr' = rename r expr in
            Dot expr' n
    rename r (Deref expr) =
        Deref $ rename r expr

instance Rename Block where
    rename r (Block stmts) =
        Block $ rename r stmts

instance Rename Stmt where
    rename r (Def template) =
        Def $ rename r template
    rename r (Return expr) = do
        Return $ rename r expr

instance Rename Template where
    rename r (Template vs def) = Template vs $ rename r def
    rename r (NoTemplate def) = NoTemplate $ rename r def
instance Rename Member where
    rename r m@(Field _ _ ) = m
    rename r (Ctor mod n ps inits block) =
        let r' = foldl (\r' (Param _ n) -> delete n r') r ps in
            (Ctor mod n ps $ rename r inits) $ rename r block
    rename r m@(TemplateMember template) = TemplateMember $ rename r template
instance Rename Init where
    rename r (Init n expr) = Init n (rename r expr)
instance Rename Def where
    rename r (VarDef ty n expr) =
        let expr' = rename r expr in
            VarDef ty n expr'
    rename r (FuncDef sig block) = FuncDef sig $ rename r block
    rename r (FuncProto sig) = FuncProto sig
    rename r (StructDef n inherits members) = StructDef n inherits $ rename r members
    rename r def@(StructFwdDecl _) = def
    rename r def@(Typedef _ _) = def
    rename r def@(VarProto _ _) = def
instance Rename a => Rename [a] where
    rename r = map (rename r)
instance Rename Prog where
    rename r (Prog includes templates) = Prog includes $ rename r templates

class Ocurrences a where
    ocurrences :: String -> a -> Int
instance Ocurrences Expr where
    ocurrences s (App expr1 exprs2) =
        let expr1' = ocurrences s expr1
            exprs2' = ocurrences s exprs2 in
            expr1' + exprs2'
    ocurrences s (InitApp expr1 exprs2) =
        let expr1' = ocurrences s expr1
            exprs2' = ocurrences s exprs2 in
            expr1' + exprs2'
    ocurrences s (Lam ps block) =
        if any (\(Param _ n) -> n == s) ps then 0
            else ocurrences s block
    ocurrences r expr@(Var n ty) =
        if n == r then 1 else 0
    ocurrences r expr@(Con _) = 0
    ocurrences r expr@(AppT _ _) = 0
    ocurrences r expr@(IntL _) = 0
    ocurrences r expr@(StrL _) =  0
    ocurrences r expr@(BoolL _) =  0
    ocurrences r expr@(Op _ _) =  0
    ocurrences r expr@(Op2 _ _) =  0
    ocurrences r (If expr1 expr2 expr3) =
        let expr1' = ocurrences r expr1
            expr2' = ocurrences r expr2
            expr3' = ocurrences r expr3 in
            expr1' + expr2' + expr3'
    ocurrences r (Dot expr n) =
        ocurrences r expr
    ocurrences r (Deref expr) =
        ocurrences r expr

instance Ocurrences Block where
    ocurrences r (Block stmts) =
        ocurrences r stmts

instance Ocurrences Stmt where
    ocurrences r (Def template) =
        ocurrences r template
    ocurrences r (Return expr) =
        ocurrences r expr

instance Ocurrences Template where
    ocurrences r (Template vs def) = ocurrences r def
    ocurrences r (NoTemplate def) = ocurrences r def
instance Ocurrences Member where
    ocurrences r (Field _ _ ) = 0
    ocurrences r (Ctor mod n ps inits block) = if any (\(Param _ n) -> n==r) ps then 0 else ocurrences r inits + ocurrences r block
    ocurrences r (TemplateMember template) = ocurrences r template
instance Ocurrences Init where
    ocurrences r (Init n expr) = ocurrences r expr
instance Ocurrences Def where
    ocurrences r (VarDef ty n expr) =
        ocurrences r expr
    ocurrences r (FuncDef sig block) = ocurrences r block
    ocurrences r (FuncProto sig) = 0
    ocurrences r (StructDef n inherits members) = ocurrences r members
    ocurrences r (StructFwdDecl _) = 0
    ocurrences r (Typedef _ _) = 0
    ocurrences r (VarProto _ _) = 0
instance Ocurrences a => Ocurrences [a] where
    ocurrences r = sum . map (ocurrences r)
instance Ocurrences Prog where
    ocurrences r (Prog includes templates) = ocurrences r templates
instance Subst Expr where
    subst r (App expr1 exprs2) =
        let expr1' = subst r expr1
            exprs2' = subst r exprs2 in
            App expr1' exprs2'
    subst r (InitApp expr1 exprs2) =
        let expr1' = subst r expr1
            exprs2' = subst r exprs2 in
            InitApp expr1' exprs2'
    subst r (Lam ps block) =
        let ps' = subst r ps
            block' = subst r block in
            Lam ps' block'
    subst r expr@(Var n ty) =
        Var n $ subst r ty
    subst r expr@(Con _) = expr
    subst r (AppT n tys) = AppT n (subst r tys)
    subst r expr@(IntL _) =  expr
    subst r expr@(StrL _) =  expr
    subst r expr@(BoolL _) =  expr
    subst r expr@(Op _ _) =  expr
    subst r expr@(Op2 _ _) =  expr
    subst r (If expr1 expr2 expr3) =
        let expr1' = subst r expr1
            expr2' = subst r expr2
            expr3' = subst r expr3 in
            If expr1' expr2' expr3'
    subst r (Dot expr n) =
        let expr' = subst r expr in
            Dot expr' n
    subst r (Deref expr) =
        Deref $ subst r expr

instance Subst Block where
    subst r (Block stmts) =
        Block $ subst r stmts

instance Subst Stmt where
    subst r (Def template) =
        Def $ subst r template
    subst r (Return expr) = do
        Return $ subst r expr

instance Subst Template where
    subst r (Template vs def) = Template vs $ subst r def
    subst r (NoTemplate def) = NoTemplate $ subst r def
instance Subst Member where
    subst r (Field ty n ) = Field (subst r ty) n
    subst r (Ctor mod n ps inits block) = (Ctor mod n (subst r ps) $ subst r inits) $ subst r block
    subst r (TemplateMember template) = TemplateMember $ subst r template
instance Subst Param where
    subst r (Param ty n) = Param (subst r ty) n
instance Subst FuncSig where
    subst r (FuncSig rt n ps mod) = FuncSig (subst r rt) n (subst r ps) mod
instance Subst Init where
    subst r (Init n expr) = Init n (subst r expr)
instance Subst Inherit where
  subst s (Public ty) = Public $ subst s ty
  subst s (Private ty) = Private $ subst s ty

instance Subst Def where
    subst r (VarDef ty n expr) =
            VarDef (subst r ty) n (subst r expr)
    subst r (FuncDef sig block) = FuncDef (subst r sig) $ subst r block
    subst r (FuncProto sig) = FuncProto (subst r sig)
    subst r (StructDef n inherits members) = StructDef n (subst r inherits) $ subst r members
    subst r def@(StructFwdDecl _) = def
    subst r (Typedef ty n) = Typedef (subst r ty) n
    subst r (VarProto ty n) = VarProto (subst r ty) n
instance Subst Prog where
    subst r (Prog includes templates) = Prog includes $ subst r templates
class Optimize a where
    optimize :: a -> OptEnv a

optimizeSymbol n ty expr = do
      tys <- getCurry n
      case tys of
          Just i -> do
              let (rt0, pts0) = uncurryType $ unwrapTFun ty
              let pts = take i pts0
              let pts1 = drop i pts0
              let rt = wrapTFun $ curryType rt0 pts1
              let ps = map (\i -> ("_curry" ++ show i)) [1..length pts]
              let params = zipWith Param pts ps
              let args = zipWith Var ps pts
              let (_, expr') = L.foldr (\p@(Param ty _) (rt' ,expr) ->
                      (ty --> rt', wrapLam rt' [ty] $ Lam [p] $ Block [Return expr])) (rt, App expr  args) params
              return expr'
          _ -> return expr

instance Optimize Expr where
    optimize (App expr1 [expr2]) = do
        expr1' <- optimize expr1
        expr2' <- optimize expr2
        -- trace (showII expr1') (return ())
        case unwrapLam expr1' of
            Lam [Param ty v] (Block stmts) -> do
                -- beta
                id <- nextVid
                let var = v ++ show id
                -- let stmts' = rename (singleton v (Var var)) stmts
                -- pushStmt $ Def $ NoTemplate $ VarDef ty var expr2'
                let stmts' = rename (singleton v expr2') stmts
                let len = length stmts'
                let keep = take (len - 1) stmts'
                let (Return expr') = stmts' !! (len - 1)
                mapM_ pushStmt keep
                return expr'
            _ -> return $ App expr1' [expr2']
    -- eta doesn't work for top level functions
    optimize (Lam [Param _ n] (Block [Return (App expr [Var n2 _])])) | n == n2 && case expr of
            Var n _ | isUpper $ head n -> False -- don't eta if a constructor is being applied
            _ -> True =
        optimize expr

    optimize (App expr1 exprs2) = do
        expr1' <- optimize expr1
        exprs2' <- optimize exprs2
        return $ App expr1' exprs2'
    optimize (InitApp expr1 exprs2) = do
        expr1' <- optimize expr1
        exprs2' <- optimize exprs2
        return $ InitApp expr1' exprs2'
    optimize (Lam ps block) = do
        block' <- optimize block
        return $ Lam ps block'
    optimize expr@(Var n ty) = optimizeSymbol n ty expr -- we should use the uncurried type but since this type is no longer used after optimization, we leave the curried type
    optimize expr@(Con _) = return expr
    optimize expr@(AppT _ _) = return expr
    optimize expr@(IntL _) = return expr
    optimize expr@(StrL _) = return expr
    optimize expr@(BoolL _) = return expr
    optimize expr@(Op n ty) = optimizeSymbol n ty expr
    optimize expr@(Op2 n ty) = optimizeSymbol n ty expr
    optimize (If expr1 expr2 expr3) = do
        expr1' <- optimize expr1
        expr2' <- optimize expr2
        expr3' <- optimize expr3
        return $ If expr1' expr2' expr3'
    optimize (Dot expr n) = do
        expr' <- optimize expr
        return $ Dot expr' n
    optimize (Deref expr) =
        Deref <$> optimize expr

instance Optimize Block where
    optimize (Block stmts) =
        withBlock $ optimize stmts

instance Optimize Stmt where
    optimize (Def template) = do
        stmt <- Def <$> optimize' LetBinding template
        pushStmt stmt
        return stmt
    optimize (Return expr) = do
        stmt <- Return <$> optimize expr
        pushStmt stmt
        return stmt

instance Optimize' Template where
    optimize' mode (Template vs def) = Template vs <$> optimize' mode def
    optimize' mode (NoTemplate def) = NoTemplate <$> optimize' mode def
instance Optimize Member where
    optimize m@(Field _ _ ) = return m
    optimize m@(Ctor mod n ps inits block) = Ctor mod n ps inits <$> optimize block
    optimize m@(TemplateMember template) = TemplateMember <$> optimize' TopLevel template

class Optimize' a where
    optimize' :: Mode -> a -> OptEnv a
instance Optimize' Def where
    optimize' TopLevel (VarProto ty@(TApp "std::function" _) n) = do
        let ft = unwrapTFun ty
        let (rt, pts) = uncurryType ft
        setCurry n (length pts)
        return $ FuncProto (FuncSig rt n (map (\ty -> Param ty "") pts) [])
    optimize' TopLevel (VarDef ty@(TApp "std::function" _) n expr) = do
        let ft = unwrapTFun ty
        let (rt, pts) = uncurryType ft
        let i = length pts
        setCurry n i
        let (args, block0) = uncurryExpr (zip pts (map (("_p" ++) . show) [1..i])) (unwrapLam expr)
        block <- optimize block0
        return $ FuncDef (FuncSig rt n (zipWith Param pts args) []) block
    optimize' _ (FuncDef sig block) = FuncDef sig <$> optimize block
    optimize' _ (FuncProto sig) = return $ FuncProto sig
    optimize' _ (StructDef n inherits member) = StructDef n inherits <$> optimize member
    optimize' _ def@(StructFwdDecl _) = return def
    optimize' _ def@(Typedef _ _) = return def
    optimize' _ def@(VarDef ty n expr) = VarDef ty n <$> optimize expr
    optimize' _ def@(VarProto _ _) = return def
instance Optimize a => Optimize [a] where
    optimize = mapM optimize
instance Optimize' a => Optimize' [a] where
    optimize' mode = mapM $ optimize' mode
instance Optimize Prog where
    optimize (Prog includes templates) = Prog includes <$> optimize' TopLevel templates

optimizeIt :: Prog -> IO Prog
optimizeIt x = evalStateT (evalStateT (evalStateT (optimize x) 0) [[]]) currytable

class Specialize a b where
    specialize :: a -> StateT [String] IO b

instance Specialize Type Type where
    specialize TAuto = return TAuto
    specialize ty@(TVar _) = return ty
    specialize ty@(TCon _) = return ty
    specialize (TApp "std::vector" [TCon "char"]) = return $ TCon "std::string"
    specialize (TApp n pts) = TApp n <$> (specialize pts)
    specialize (TFun rt pts) = TFun <$> (specialize rt) <*> (specialize pts)

instance Specialize Expr Expr where
    specialize (App (Con "chaskell::vectorchar") ps) = return $ App (Con "std::string") ps
    specialize (InitApp (AppT "std::vector" [TCon "char"]) []) = return $ App (Con "std::string") []
    specialize (App expr1 exprs2) = do
        expr1' <- specialize expr1
        exprs2' <- specialize exprs2
        return $ App expr1' exprs2'

    specialize (InitApp expr1 exprs2) = do
        expr1' <- specialize expr1
        exprs2' <- specialize exprs2
        return $ InitApp expr1' exprs2'
    specialize (Lam ps block) = do
        ps' <- specialize ps
        block' <- specialize block
        return $ Lam ps' block'
    specialize expr@(Var n ty) = do
        ty' <- specialize ty
        return $  if n == "chaskell::append" -- specialize append to + for strings
            then case unwrapTFun ty' of
                TFun _ (TCon "std::string") -> Op2 "+" ty' -- it is sufficient to check the type of the first parameter
                _ -> Var n ty'
            else Var n ty'
    specialize expr@(Con _) = return expr
    specialize expr@(AppT n ty) = AppT n <$> specialize ty
    specialize expr@(IntL _) = return expr
    specialize expr@(StrL _) = return expr
    specialize expr@(BoolL _) = return expr
    specialize expr@(Op n ty) = Op n <$> specialize ty
    specialize expr@(Op2 n ty) = Op2 n <$> specialize ty
    specialize (If expr1 expr2 expr3) = do
        expr1' <- specialize expr1
        expr2' <- specialize expr2
        expr3' <- specialize expr3
        return $ If expr1' expr2' expr3'
    specialize (Dot expr n) = do
        expr' <- specialize expr
        return $ Dot expr' n
    specialize (Deref expr) =
        Deref <$> specialize expr

instance Specialize Block Block where
    specialize (Block stmts) =
        Block <$> specialize stmts

instance Specialize Stmt Stmt where
    specialize (Def template) = do
        Def <$> specialize template
    specialize (Return expr) = do
        Return <$> specialize expr

findVectors :: Type -> [String]
findVectors TAuto = []
findVectors (TVar _) = []
findVectors (TCon _) = []
findVectors (TFun rt pt) = findVectors rt `L.union` findVectors pt
findVectors (TApp "std::vector" [TVar n]) = [n]
findVectors (TApp n pts) = L.foldl L.union [] (map findVectors pts )

findVectorChars :: Type -> Bool
findVectorChars TAuto = False
findVectorChars (TVar _) = False
findVectorChars (TCon _) = False
findVectorChars (TFun rt pt) = findVectorChars rt || findVectorChars pt
findVectorChars (TApp "std::vector" [TCon "char"]) = True
findVectorChars (TApp n pts) = any findVectorChars pts

findVectorCharsParam :: Param -> Bool
findVectorCharsParam (Param ty _) = findVectorChars ty

findVectorCharsParams :: [Param] -> Bool
findVectorCharsParams ps = any findVectorCharsParam ps

instance Specialize Template [Template] where
    specialize  temp@(Template vs0 def@(FuncDef (FuncSig rt n ps mod) block)) = do
        list <- get
        if n `L.notElem` list
            then do
                temp' <- specialize temp
                return [temp']
            else do
                let vs = findVectors rt `L.union` L.foldl L.union [] (map (\(Param ty _)->findVectors ty) ps)
                let s = fromList [(v, TCon "char")|v<-vs]
                let def' = subst s def
                let vs' = vs0 L.\\ vs
                temp'' <- (if L.null vs'
                    then NoTemplate
                    else Template vs) <$> specialize def'
                if L.null vs
                    then return [temp'']
                    else do
                        temp' <- specialize temp
                        return [temp', Comment "~~~~~~~~~~~~~~~~specialization~~~~~~~~~~~~~~~~", temp'']


    specialize  temp@(Template vs0 def@(FuncProto sig@(FuncSig rt n ps mod))) = do
        let vs = findVectors rt `L.union` L.foldl L.union [] (map (\(Param ty _)->findVectors ty) ps)
        let s = fromList [(v, TCon "char")|v<-vs]
        let def'@(FuncProto (FuncSig rt' _ ps' _)) = subst s def
        if findVectorChars rt' || findVectorCharsParams ps'
            then do
                list <- get
                put $ list ++ [n]
                let vs' = vs0 L.\\ vs
                temp'' <- (if L.null vs'
                    then NoTemplate
                    else Template vs) <$> specialize def'
                if L.null vs
                    then return [temp'']
                    else do
                        temp' <- specialize temp
                        return [temp', Comment "~~~~~~~~~~~~~~~~specialization~~~~~~~~~~~~~~~~", temp'']
            else do
                temp' <- specialize temp
                return [temp']

    specialize  temp@(NoTemplate def@(FuncDef (FuncSig rt n ps mod) _)) = do
        if findVectorChars rt || any (\(Param ty _) -> findVectorChars ty) ps
            then do
                list <- get
                put $ list ++ [n]
                temp' <- NoTemplate <$> specialize def
                return [temp']
            else do
                temp' <- specialize temp
                return [temp']

    specialize  temp@(NoTemplate def@(FuncProto (FuncSig rt n ps mod))) = do
        if findVectorChars rt || any (\(Param ty _) -> findVectorChars ty) ps
            then do
                list <- get
                put $ list ++ [n]
                temp' <- NoTemplate <$> specialize def
                return [temp']
            else do
                temp' <- specialize temp
                return [temp']
    specialize  temp = do
        temp' <- specialize temp
        return [temp']
instance Specialize Template Template where
    specialize (Template vs def) = do
        Template vs <$> specialize def
    specialize (NoTemplate def) = do
        NoTemplate <$> specialize def

instance Specialize Member Member where
    specialize (Field ty n ) = Field <$> specialize ty <*> return n
    specialize m@(Ctor mod n ps inits block) = Ctor mod n <$> specialize ps <*> specialize inits <*> specialize block
    specialize m@(TemplateMember template) = TemplateMember <$> specialize template


instance Specialize FuncSig FuncSig where
    specialize (FuncSig rt n ps mod) = FuncSig <$> specialize rt <*> return n <*> specialize ps <*> return mod

instance Specialize Param Param where
    specialize (Param ty n) = Param <$> specialize ty <*> return n

instance Specialize Inherit Inherit where
    specialize (Public ty) = Public <$> specialize ty
    specialize (Private ty) = Private <$> specialize ty

instance Specialize Init Init where
    specialize (Init n expr) = Init n <$> specialize expr

instance Specialize Def Def where
    specialize (FuncDef sig block) = FuncDef <$> specialize sig <*> specialize block
    specialize (FuncProto sig) =  FuncProto <$> specialize sig
    specialize (StructDef n inherits member) = StructDef n <$> specialize inherits <*> specialize member
    specialize def@(StructFwdDecl _) = return def
    specialize (Typedef ty n) = Typedef <$> specialize ty <*> return n
    specialize (VarDef ty n expr) = VarDef <$> specialize ty <*> return n <*> specialize expr
    specialize (VarProto ty n) = VarProto <$> specialize ty <*> return n

instance Specialize a b => Specialize [a] [b] where
    specialize = mapM specialize

instance Specialize Prog Prog where
    specialize (Prog includes templates) = Prog includes . concat <$> specialize templates

evalTran :: TranEnv a -> IO a
evalTran x = evalStateT (evalStateT (evalStateT x empty ) []) empty

main :: IO ()
main = do
    (input : _) <- getArgs
    str <- readFile input
    case parseModule str of
        ParseOk a -> do
            (x,y) <- evalTran $ do
                let (HsModule _ _ _ _ hsdecls) = a
                let typetree = convert hsdecls
                typetree' <- liftIO $ typeIt typetree
                h0 <- tranlateHaskellToHPlusPlus typetree'
                c0 <- tranlateHaskellToCPlusPlus typetree'
                liftIO $ evalStateT (do
                    h1 <- liftIO $ optimizeIt h0
                    hs <- specialize h1
                    c1 <- liftIO $ optimizeIt c0
                    cs <- specialize c1
                    return (hs :: Prog, cs :: Prog)) []

            putStrLn $ showII x
            putStrLn "//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
            putStrLn $ showII y
        ParseFailed loc str -> do
            print loc
            print str
