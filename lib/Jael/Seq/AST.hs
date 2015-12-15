module Jael.Seq.AST where

import qualified Data.Functor.Foldable as F
import qualified Data.Set as S
import           Jael.Grammar
import           Jael.Seq.Literal
import           Jael.Seq.Prm
import           Jael.Seq.Types
import           Jael.Util

data TopExpr e t = TopGlob  { tgExpr  :: e }
                 | TopFunc  { tfArgs  :: [(Text, t)]
                            , tfRetTy :: t
                            , tfExpr  :: e
                            }
                 deriving (Show)

type S1TopExpr = TopExpr S1Ex S1Ty
type S2TopExpr = TopExpr S2TyEx S2Ty

instance (HMTypable e, HMTypable t) => HMTypable (TopExpr e t) where
  hmTyOf (TopGlob{..}) = hmTyOf tgExpr
  hmTyOf (TopFunc{..}) = typesToFun (map (hmTyOf . snd) tfArgs, hmTyOf tfRetTy)

--------------------------------------------------------------------------------
-- Stage 1
--------------------------------------------------------------------------------

data S1Ex = S1Call Text [S1Ex]
          | S1CallPrm Prm [S1Ex]
          | S1Let Text S1Ex S1Ex
          | S1If S1Ex S1Ex S1Ex
          | S1Tup [S1Ex]
          | S1Var Text
          | S1Lit Literal
          deriving (Eq, Show)

data S1ExF a = S1CallF Text [a]
             | S1CallPrmF Prm [a]
             | S1LetF Text a a
             | S1IfF a a a
             | S1TupF [a]
             | S1VarF Text
             | S1LitF Literal
             deriving (Eq, Functor, Show)

type instance F.Base S1Ex = S1ExF

instance F.Foldable S1Ex where
  project (S1Call x y)    = S1CallF x y
  project (S1CallPrm x y) = S1CallPrmF x y
  project (S1Let x y z)   = S1LetF x y z
  project (S1If x y z)    = S1IfF x y z
  project (S1Tup x)       = S1TupF x
  project (S1Var x)       = S1VarF x
  project (S1Lit x)       = S1LitF x

instance F.Unfoldable S1Ex where
  embed (S1CallF x y)    = S1Call x y
  embed (S1CallPrmF x y) = S1CallPrm x y
  embed (S1LetF x y z)   = S1Let x y z
  embed (S1IfF x y z)    = S1If x y z
  embed (S1TupF x)       = S1Tup x
  embed (S1VarF x)       = S1Var x
  embed (S1LitF x)       = S1Lit x

freeVars :: S1Ex -> S.Set Text
freeVars = F.cata alg
  where alg :: S1ExF (S.Set Text) -> S.Set Text
        alg (S1CallF n es) = n `S.insert` S.unions es
        alg (S1CallPrmF _ es) = S.unions es
        alg (S1LetF x e1 e2) = e1 `S.union` S.delete x e2
        alg (S1IfF e1 e2 e3) = e1 `S.union` e2 `S.union` e3
        alg (S1TupF es) = S.unions es
        alg (S1VarF x) = S.singleton x
        alg _ = S.empty

--------------------------------------------------------------------------------
-- Type inference (intermidiate stage 2)
--------------------------------------------------------------------------------

data HMEx = HMVar Text
          | HMLit Literal
          | HMApp HMEx HMEx
          | HMAbs Text HMEx
          | HMLet Text HMEx HMEx
          deriving (Eq, Show)

data HMExF a = HMVarF Text
             | HMLitF Literal
             | HMAppF a a
             | HMAbsF Text a
             | HMLetF Text a a
             deriving (Eq, Functor, Show)

type instance F.Base HMEx = HMExF

instance F.Foldable HMEx where
  project (HMVar x)     = HMVarF x
  project (HMLit x)     = HMLitF x
  project (HMApp x y)   = HMAppF x y
  project (HMAbs x y)   = HMAbsF x y
  project (HMLet x y z) = HMLetF x y z

instance F.Unfoldable HMEx where
  embed (HMVarF x)     = HMVar x
  embed (HMLitF x)     = HMLit x
  embed (HMAppF x y)   = HMApp x y
  embed (HMAbsF x y)   = HMAbs x y
  embed (HMLetF x y z) = HMLet x y z

data HMTypedEx = HMTypedEx (Ann HMTy HMExF HMTypedEx)
  deriving (Show)

data HMTypedExF a = HMTypedExF (Ann HMTy HMExF a)
  deriving (Show, Functor)

type instance F.Base HMTypedEx = HMTypedExF

instance F.Foldable HMTypedEx where
  project (HMTypedEx Ann {ann=t, unAnn=e}) = HMTypedExF Ann {ann=t, unAnn=e}

instance F.Unfoldable HMTypedEx where
  embed (HMTypedExF Ann {ann=t, unAnn=e}) = HMTypedEx Ann {ann=t, unAnn=e}

instance TIOps HMTypedEx where
  ftv (HMTypedEx (Ann {ann=t})) = ftv t
  apply s = F.cata alg
    where alg (HMTypedExF Ann {ann=t, unAnn=e}) = HMTypedEx Ann {ann=apply s t, unAnn=e}

instance HMTypable HMTypedEx where
  hmTyOf (HMTypedEx Ann {ann=t}) = t

--------------------------------------------------------------------------------
-- Stage 2
--------------------------------------------------------------------------------

-- A program after stage 2 is represented by the same types of expressions as
-- stage 1 (for now), but now every expression is annotated with a stage 2 type.

data S2TyEx = S2TyEx (Ann S2Ty S1ExF S2TyEx)
  deriving (Eq, Show)

data S2TyExF a = S2TyExF (Ann S2Ty S1ExF a)
  deriving (Show, Functor)

type instance F.Base S2TyEx = S2TyExF

instance F.Foldable S2TyEx where
  project (S2TyEx Ann{ann=t, unAnn=e}) = S2TyExF Ann{ann=t, unAnn=e}

instance F.Unfoldable S2TyEx where
  embed (S2TyExF Ann{ann=t, unAnn=e}) = S2TyEx Ann{ann=t, unAnn=e}

instance HMTypable S2TyEx where
  hmTyOf (S2TyEx Ann{ann=t}) = hmTyOf t

s2TyOf :: S2TyEx -> S2Ty
s2TyOf (S2TyEx Ann{ann=t}) = t

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

binPrm :: Prm -> GExpr -> GExpr -> S1Ex
binPrm p g1 g2 = S1CallPrm p $ map gToS1Ex [g1, g2]

gLetToS1Ex :: GELetExpr -> S1Ex
gLetToS1Ex (GELetExpr [] e)    = gToS1Ex e
gLetToS1Ex (GELetExpr (GELetIdent (LIdent i) h:t) e) =
  S1Let (pack i) (gToS1Ex h) (gLetToS1Ex $ GELetExpr t e)

gToS1Ex :: GExpr -> S1Ex
gToS1Ex (GELogOr     e1 e2) = binPrm  POr e1 e2
gToS1Ex (GELogAnd    e1 e2) = binPrm  PAnd e1 e2
gToS1Ex (GEEq        e1 e2) = binPrm  PEq e1 e2
gToS1Ex (GENotEq     e1 e2) = binPrm  PNeq e1 e2
gToS1Ex (GEGtEq      e1 e2) = binPrm  PGeq e1 e2
gToS1Ex (GELtEq      e1 e2) = binPrm  PLeq e1 e2
gToS1Ex (GEGt        e1 e2) = binPrm  PGt  e1 e2
gToS1Ex (GELt        e1 e2) = binPrm  PLt  e1 e2
gToS1Ex (GEPlus      e1 e2) = binPrm  PAdd  e1 e2
gToS1Ex (GEMinus     e1 e2) = binPrm  PSub  e1 e2
gToS1Ex (GETimes     e1 e2) = binPrm  PTimes  e1 e2
gToS1Ex (GEDiv       e1 e2) = binPrm  PDiv  e1 e2
gToS1Ex (GEMod       e1 e2) = binPrm  PMod  e1 e2
gToS1Ex (GEBitCat    e1 e2) = binPrm  PBitCat e1 e2
gToS1Ex (GELogNot    e    ) = S1CallPrm PNot [gToS1Ex e]

gToS1Ex (GEIf b e1 e2) = S1If (gToS1Ex b) (gLetToS1Ex e1) (gLetToS1Ex e2)

gToS1Ex (GEApp _ []) = error "The grammar should ensure there is always at\
                            \ least one argument to an application."
gToS1Ex (GEApp (LIdent n) as) = S1Call (pack n) (map (\(GEAppArg x) -> gToS1Ex x) as)
gToS1Ex (GEAppScoped _ []) = error "The grammar should ensure there is at least\
                                  \ one argument to a (scoped) application."
gToS1Ex (GEAppScoped (LScopedIdent n) as) = S1Call (pack n) (map (\(GEAppArg x) -> gToS1Ex x) as)

gToS1Ex (GEInt i) = S1Lit $ LInt $ parseDecInt i
gToS1Ex (GETrue)  = S1Lit $ LBool True
gToS1Ex (GEFalse) = S1Lit $ LBool False
gToS1Ex (GETup x xs) = if null xs
                          then error "The grammar should ensure at least 2\
                                    \ tuple arguments."
                          else S1Tup (map (\(GETupArg a) -> gToS1Ex a) (x:xs))
gToS1Ex (GEUnit)  = S1Lit LUnit
gToS1Ex (GEVar (LIdent i)) = S1Var (pack i)

