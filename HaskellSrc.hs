module HaskellSrc where

import CPlusPlus

import Prelude hiding (lookup, exp)
import Language.Haskell.Syntax
import qualified Data.List as L
import Data.List ((\\))
import Data.Map hiding (map, foldl, (!), (\\))
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

extractConstructorInit (name, _) = Init (mname name) (App (Var "std::move") [Var (pname name)])

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

instance FreeVars HsDecl where
    freeVars (HsPatBind _ _ (HsUnGuardedRhs exp) _) = freeVars exp
    freeVars (HsTypeSig _ _ _) = []

instance FreeVars HsExp where
    freeVars (HsLit _) = []
    freeVars (HsCon _) = []
    freeVars (HsVar n) = [extractQName n]
    freeVars (HsParen exp) = freeVars exp
    freeVars (HsLambda _ pats exp) = freeVars exp \\ freeVars pats
    freeVars (HsApp exp1 exp2) = freeVars exp1 `L.union` freeVars exp2
    freeVars (HsLet decls exp) = freeVars decls `L.union` freeVars exp \\ boundVars decls
    freeVars (HsCase exp alts) = freeVars exp `L.union` freeVars alts
    freeVars (HsIf exp1 exp2 exp3) = freeVars exp1 `L.union` freeVars exp2 `L.union` freeVars exp3
    freeVars (HsInfixApp exp1 _ exp2) = freeVars exp1 `L.union` freeVars exp2
    freeVars (HsExpTypeSig _ exp _) = freeVars exp
    freeVars (HsTuple exps) = freeVars exps
    freeVars (HsList exps) = freeVars exps

instance FreeVars HsAlt where
    freeVars (HsAlt _ pats (HsUnGuardedAlt exp) _) = freeVars exp \\ freeVars pats

instance FreeVars HsPat where
    freeVars (HsPApp _ pats) = freeVars pats
    freeVars (HsPVar n) = [extractName n]
    freeVars (HsPParen pat) = freeVars pat
    freeVars (HsPWildCard) = []

instance FreeVars a => FreeVars [a] where
    freeVars = foldl (\fv a-> fv `L.union` freeVars a) []

class BoundVars a where
    boundVars :: a -> [String]

instance BoundVars HsDecl where
    boundVars (HsPatBind _ pat _ _) = freeVars pat
    boundVars (HsTypeSig _ _ _) = []
    boundVars decl = error $ "unsupported " ++ show decl

instance BoundVars a => BoundVars [a] where
    boundVars = foldl (\fv a-> fv `L.union` boundVars a) []
