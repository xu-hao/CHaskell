{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module HaskellSrc where

import CPlusPlus hiding (IntL, StrL)
import Mapping

import Prelude hiding (lookup, exp)
import Language.Haskell.Syntax
import qualified Data.List as L
import Data.List ((\\))
import Data.Tree
import Data.Map hiding (map, foldl, (\\))
import Control.Applicative ((<$>), (<*>))
import Data.Maybe

-- extract


class Pattern a where
    extractCons :: a -> String
    extractPat :: a -> HsPat
    extractExp :: a -> HsExp
    extractParams :: a -> [String]
    extractHead :: a -> String
    extractCons = extractPatCons . extractPat
instance Pattern HsMatch where
    extractPat (HsMatch _ _ (pat : _) _ _) = pat
    extractExp (HsMatch _ _ _ (HsUnGuardedRhs exp) _) = exp
    extractParams (HsMatch _ _ pats _ _) = zipWith extractParamFromPat pats  [1..length pats]
    extractHead (HsMatch _ n _ _ _) = extractName n

instance Pattern HsDecl where
    extractPat (HsPatBind _ pat _ _) = pat
    extractExp (HsPatBind _ _ (HsUnGuardedRhs exp) _) = exp
    extractParams (HsPatBind _ _ _ _) = []
    extractHead (HsPatBind _ (HsPVar n) _ _) = extractName n

instance Pattern HsAlt where
    extractPat (HsAlt _ pat _ _) = pat
    extractExp (HsAlt _ _ (HsUnGuardedAlt exp) _) = exp
    extractParams (HsAlt _ _ _ _) = []
    extractHead = error "not supported"

extractPatCons (HsPApp n _) = extractQName n
extractPatCons (HsPParen pat) = extractPatCons pat
extractPatCons pat = error $ show pat

extractParamFromPat (HsPVar n) _ = extractName n
extractParamFromPat (HsPApp _ _) i = "_param" ++ show i
extractParamFromPat (HsPParen pat) i = extractParamFromPat pat i



extractQOp (HsQVarOp name) = extractQName name
extractQOp (HsQConOp name) = extractQName name
extractQName (UnQual name) = extractName name
extractQName (Qual (Module m) n) = m ++ "::" ++ extractName n
extractQName (Special (HsTupleCon _) ) = "std::make_tuple"
extractQName (Special HsCons) = "chaskell::cons"
extractName (HsIdent n) = n
extractName (HsSymbol n) = n

extractConstructorName (HsRecDecl _ (HsIdent name) _) = name
extractConstructorName (HsConDecl _ (HsIdent name) _) = name

extractConstructorNameAndFields (HsRecDecl _ n fields) = (extractName n, map (extractFieldNameAndType) fields)
extractConstructorNameAndFields (HsConDecl _ n fields) = (extractName n, zipWith (\i (HsUnBangedTy ty) -> ("var" ++ show i, ty)) [1..length fields] fields)

extractFieldNameAndType (HsIdent name : _, (HsUnBangedTy ty)) = (name, ty)

extractConstructorInit (name, ty) = Init (mname name) (Var (pname name) ty)

-- transform
findPApp :: [HsPat] -> Maybe Int
findPApp [] = Nothing
findPApp (HsPApp _ _ : _) = return 0
findPApp (HsPParen pat : pats) = findPApp (pat : pats)
findPApp (_ : pats) = (1 + ) <$> findPApp pats

transformMatchesToAlts :: [HsMatch] -> HsMatch
transformMatchesToAlts matches@(match@(HsMatch srcloc fn params _ _) : _) =
  case findPApp params of
      Just n ->
          let paramnames = extractParams match
              params' = map (HsPVar . HsIdent) paramnames
              caseparam = paramnames !! n
              caseexp = HsVar (UnQual (HsIdent caseparam))
              alts = map (\(HsMatch srcloc2 _ params (HsUnGuardedRhs exp) decls) ->
                  HsAlt srcloc2 (params !! n) (HsUnGuardedAlt exp) decls) matches
              rhs' = HsUnGuardedRhs (HsCase caseexp alts) in
          HsMatch srcloc fn params' rhs' []
      Nothing -> if length matches == 1 then match else error $ "overlapping patterns" ++ show matches

transformPatBindToAlts :: HsDecl -> HsMatch
transformPatBindToAlts bind@(HsPatBind srcloc (HsPVar fn) rhs decls) =
    HsMatch srcloc fn [] rhs decls


-- free vars
class FreeVars a where
    freeVars :: a -> [String]

instance FreeVars Decl where
    freeVars (BindD _ exp) = freeVars exp
    freeVars (TypeSigD _ _) = []
    freeVars (DataD _ _ _) = []

instance FreeVars TypeExp where
    freeVars (_, exp) = freeVars exp

instance FreeVars Exp where
    freeVars (LitE _) = []
    freeVars (VarE n) = [n]
    freeVars (LamE n exp) = freeVars exp \\ [n]
    freeVars (AppE exp1 exp2) = freeVars exp1 `L.union` freeVars exp2
    freeVars (LetE decls exp) = freeVars decls `L.union` freeVars exp \\ boundVars decls
    freeVars (CaseE exp alts) = freeVars exp `L.union` freeVars alts
    freeVars (IfE exp1 exp2 exp3) = freeVars exp1 `L.union` freeVars exp2 `L.union` freeVars exp3
    freeVars (TypeSigE exp _) = freeVars exp
    freeVars (TupleE exps) = freeVars exps
    freeVars (ListE exps) = freeVars exps

instance FreeVars Alt where
    freeVars (Alt pat exp) = freeVars exp \\ freeVars pat

instance FreeVars (String, Type, Pat) where
    freeVars (_, _, pat) = freeVars pat

instance FreeVars Pat where
    freeVars (AppP _ pats) = freeVars pats
    freeVars (VarP n) = [n]
    freeVars WildCardP = []

instance FreeVars a => FreeVars [a] where
    freeVars = foldl (\fv a-> fv `L.union` freeVars a) []

class BoundVars a where
    boundVars :: a -> [String]

instance BoundVars Decl where
    boundVars (BindD n _) = [n]
    boundVars (TypeSigD _ _) = []
    boundVars (DataD _ _ cons) = map (\(ConD n _) -> n) cons

instance BoundVars a => BoundVars [a] where
    boundVars = foldl (\fv a-> fv `L.union` boundVars a) []

declsVarmap :: [Decl] -> Map String Type
declsVarmap = foldl (\map decl ->
    case decl of
        (BindD n (ty, _)) -> insert n ty map
        _ -> map) empty
data Lit = StrL String
         | IntL Int

type TypeExp = (Type, Exp)

data Exp = LitE Lit
             | VarE String
             | LamE String TypeExp
             | AppE TypeExp TypeExp
             | LetE [Decl] TypeExp
             | CaseE TypeExp [Alt]
             | IfE TypeExp TypeExp TypeExp
             | TypeSigE TypeExp Type
             | TupleE [TypeExp]
             | ListE [TypeExp]

data Decl = TypeSigD String Type
              | BindD String TypeExp
              | DataD String [String] [ConD]

data ConD = ConD String [(String, Type)]

data Alt = Alt Pat TypeExp

data Pat = VarP String
         | AppP String [(String, Type, Pat)]
         | WildCardP

class Convert a b where
    convert :: a -> b

u = TAuto

instance Convert HsLiteral Lit where
  convert (HsInt i) = IntL (fromIntegral i)
  convert (HsString s) = StrL s

lam :: Pat -> Exp -> Exp
lam (VarP v) e = LamE v (u, e)
lam (WildCardP) e = LamE "__dummy__" (u, e)
lam pat e = LamE "__param__" (u, CaseE (u, VarE "__param__") [Alt pat (u, e)])

lams :: [Pat] -> [TypeExp] -> [TypeExp]
lams (VarP v : _) es = map (\e -> (u, LamE v e)) es
lams (WildCardP : _) es = map (\e -> (u, LamE "__dummy__" e)) es
lams pats es = [(u, LamE "__param__" (u, CaseE (u, VarE "__param__") (zipWith Alt pats es)))]

app :: Exp -> Exp -> Exp
app e e' = AppE (u, e) (u, e')

-- types
instance Convert HsConDecl ConD where
  convert (HsRecDecl _ name fields) =
      ConD (extractName name) (convert fields)
  convert (HsConDecl _ name fields) =
      ConD (extractName name) (convert (zip [1..length fields] fields))

instance Convert ([HsName], HsBangType) (String, Type) where
  convert ([n], HsUnBangedTy ty) = (extractName n, convert ty)

instance Convert (Int, HsBangType) (String, Type) where
  convert (i, HsUnBangedTy ty) = ("param" ++ show i, convert ty)

instance Convert HsType Type where
  convert (HsTyVar (HsIdent name)) = TVar name
  convert (HsTyCon (UnQual (HsIdent name))) = fromMaybe (TCon name) (lookup name primtypetable)
  convert (HsTyApp (HsTyCon (Special HsListCon)) ty) = TApp "std::vector" [convert ty]
  convert (HsTyApp (HsTyCon (Special (HsTupleCon _))) ty) = TApp "std::tuple" [convert ty]
  convert (HsTyFun ty1 ty2) = convert ty1 --> convert ty2
  convert ty = error (show ty)

instance Convert a b => Convert [a] [b] where
  convert = map convert

instance Convert a Exp => Convert a TypeExp where
  convert a = (u, convert a)

instance Convert HsQOp Exp where
  convert op = VarE $ extractQOp op

instance Convert HsExp Exp where
  convert (HsLit l) =  LitE (convert l)
  convert (HsCon n) =  VarE (extractQName n)
  convert (HsVar n) =  VarE (extractQName n)
  convert (HsParen exp) = convert exp
  convert (HsLambda _ pats exp) = L.foldr lam (convert exp) (convert pats)
  convert (HsApp exp1 exp2) = AppE (convert exp1) (convert exp2)
  convert (HsLet decls exp) = LetE (convert decls) (convert exp)
  convert (HsCase exp alts) =  CaseE (convert exp) (convert alts)
  convert (HsIf exp1 exp2 exp3) = IfE (convert exp1) (convert exp2) (convert exp3)
  convert (HsInfixApp exp1 op exp2) = app (app (convert op) (convert exp1)) (convert exp2)
  convert (HsExpTypeSig _ exp (HsQualType _ ty)) = TypeSigE (convert exp) (convert ty)
  convert (HsTuple exps) = TupleE (convert exps)
  convert (HsList exps) =  ListE (convert exps)

instance Convert HsPat (String, Type, Pat) where
  convert pat = ("", u, convert pat)

instance Convert HsPat Pat where
  convert (HsPApp n pats) = AppP (extractQName n) (convert pats)
  convert (HsPVar n) =  VarP (extractName n)
  convert (HsPParen pat) = convert pat
  convert (HsPWildCard) = WildCardP

instance Convert HsAlt Alt where
  convert (HsAlt _ pat (HsUnGuardedAlt exp) _) = Alt (convert pat) (convert exp)

instance Convert HsMatch (String, [Pat], TypeExp) where
  convert (HsMatch _ n pats (HsUnGuardedRhs exp) _) = (extractName n, convert pats, convert exp)

patBind :: Pat -> Exp -> Decl
patBind (VarP n) e = BindD n (u, e)

funBind :: [(String, [Pat], TypeExp)] -> Decl
funBind matches@((n, pats, exp):_) =
    let patsst = L.transpose $ map (\(_, pats, _) -> pats) matches in
        BindD n (head (L.foldr lams (map (\(_, _, exp) -> exp) matches) patsst))

instance Convert HsDecl Decl where
  convert (HsPatBind _ pat (HsUnGuardedRhs exp) _) = patBind (convert pat) (convert exp)
  convert (HsTypeSig _ [n] (HsQualType _ ty)) = TypeSigD (extractName n) (convert ty)
  convert (HsFunBind matches) = funBind (convert matches)
  convert (HsDataDecl _ _ n ns cons _) = DataD (extractName n) (map extractName ns) (convert cons)
  convert decl = error $ "unsupported " ++ show decl
