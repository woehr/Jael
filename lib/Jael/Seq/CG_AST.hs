module Jael.Seq.CG_AST where

import qualified Data.Functor.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S
import           Jael.Grammar
import           Jael.Seq.CG_Types
import qualified Jael.Seq.HM_AST as HM
import           Jael.Seq.HM_Types
import           Jael.Seq.Literal
import           Jael.Seq.Prm
import           Jael.Seq.TI (SeqTIErr, mgu, seqInferTypedEx)
import           Jael.Util

data CGEx = CGCall Text [CGEx]
          | CGCallPrm Prm [CGEx]
          | CGLet Text CGEx CGEx
          | CGIf CGEx CGEx CGEx
          | CGTup [CGEx]
          | CGVar Text
          | CGLit Literal
          deriving (Eq, Show)

data CGExF a = CGCallF Text [a]
             | CGCallPrmF Prm [a]
             | CGLetF Text a a
             | CGIfF a a a
             | CGTupF [a]
             | CGVarF Text
             | CGLitF Literal
             deriving (Functor, Show)

type instance F.Base CGEx = CGExF

instance F.Foldable CGEx where
  project (CGCall x y)    = CGCallF x y
  project (CGCallPrm x y) = CGCallPrmF x y
  project (CGLet x y z)   = CGLetF x y z
  project (CGIf x y z)    = CGIfF x y z
  project (CGTup x)       = CGTupF x
  project (CGVar x)       = CGVarF x
  project (CGLit x)       = CGLitF x

instance F.Unfoldable CGEx where
  embed (CGCallF x y)    = CGCall x y
  embed (CGCallPrmF x y) = CGCallPrm x y
  embed (CGLetF x y z)   = CGLet x y z
  embed (CGIfF x y z)    = CGIf x y z
  embed (CGTupF x)       = CGTup x
  embed (CGVarF x)       = CGVar x
  embed (CGLitF x)       = CGLit x

data CGTypedEx = CGTypedEx (Ann CGTy CGExF CGTypedEx)
  deriving (Show)

data CGTypedExF a = CGTypedExF (Ann CGTy CGExF a)
  deriving (Show, Functor)

type instance F.Base CGTypedEx = CGTypedExF

instance F.Foldable CGTypedEx where
  project (CGTypedEx Ann{ann=t, unAnn=e}) = CGTypedExF Ann{ann=t, unAnn=e}

instance F.Unfoldable CGTypedEx where
  embed (CGTypedExF Ann{ann=t, unAnn=e}) = CGTypedEx Ann{ann=t, unAnn=e}

data CGTypeErr = ArityMismatch (M.Map Text (Integer, Integer))
               | CGTypeMismatch Ty
               | InferenceErr SeqTIErr
               deriving (Eq, Show)

checkArityErr :: M.Map Text Integer -> Text -> [M.Map Text (Integer, Integer)] -> M.Map Text (Integer, Integer)
checkArityErr arityMap n as =
  let argErrs = M.unions as
      nArity = M.findWithDefault (error $ "Expected to find " <> show n <> " in\
                                          \ the seq type env.") n arityMap
      lengthInteger = toInteger $ length as
   in if nArity /= lengthInteger
         then M.insert n (nArity, lengthInteger) argErrs
         else argErrs

arityCheck :: TyEnv -> CGEx -> Either CGTypeErr ()
arityCheck env expr =
  let arityMap = M.map (\(PolyTy _ t) -> arityOf t) (toMap env)
      arityAlg (CGCallF n as)    = checkArityErr arityMap n as
      arityAlg (CGCallPrmF p as) = checkArityErr arityMap (tshow p) as
      arityAlg _ = M.empty
      arityErrs = F.cata arityAlg expr
   in unless (null arityErrs) (Left $ ArityMismatch arityErrs)

typeCheck' :: TyEnv -> HM.Ex -> Ty -> Either CGTypeErr HM.TypedEx
typeCheck' env expr ty = case seqInferTypedEx env expr of
       Left err -> Left $ InferenceErr err
       Right te ->
         let inferred = tyOf te
          in case mgu inferred ty of
                  Left  _ -> throwError $ CGTypeMismatch inferred
                  Right _ -> return te

-- Returns either an error or the type of the expression and a map of the types
-- inferred for the names within the expression.
typeCheck :: TyEnv -> CGEx -> Ty -> Either CGTypeErr HM.TypedEx
typeCheck env expr ty = do
  arityCheck env expr
  typeCheck' env (toHM expr) ty

-- CGEx can't do abstraction so we need to do some juggling to check a function
typeCheckFunc :: TyEnv -> CGEx -> ([(Text, Ty)], Ty) -> Either CGTypeErr HM.TypedEx
typeCheckFunc env expr (args, retTy) = do
  arityCheck env expr
  let hmExpr = toHM expr
      hmFunc = foldr (HM.EAbs . fst) hmExpr args
  typeCheck' env hmFunc $ foldr (TFun . snd) retTy args

typeInf :: TyEnv -> CGEx -> Either CGTypeErr HM.TypedEx
typeInf env expr = do
  arityCheck env expr
  case seqInferTypedEx env (toHM expr) of
       Left err -> Left $ InferenceErr err
       Right te -> return te

recoverCGTy :: M.Map Text CGTy -> Ty -> CGTy
recoverCGTy = undefined

-- Given a type-annotated HM.Ex convert it to a type-annotated CGEx.
-- The conversion from a CGTy to a Ty is lossy so we need a map of type names to
-- the original CGTy. Some of the type variables in the original type may now be
-- replaced with a concrete type.
recoverCGTypedEx :: M.Map Text CGTy -> HM.TypedEx -> CGTypedEx
recoverCGTypedEx m = F.ana coalg
  where coalg (HM.TypedEx Ann{ann=ty, unAnn=(HM.EVarF x)}) = CGTypedExF Ann{ann=recoverCGTy m ty, unAnn=CGVarF x}

toHM :: CGEx -> HM.Ex
toHM = F.cata alg
  where alg (CGCallF n as) = foldl' HM.EApp (HM.EVar n) as
        alg (CGCallPrmF p as) = foldl' HM.EApp(HM.EVar $ tshow p) as
        alg (CGLetF n e1 e2) = HM.ELet n e1 e2
        alg (CGIfF b e1 e2) = HM.EApp (HM.EApp (HM.EApp (HM.EVar "if") b) e1) e2
        alg (CGTupF as) = foldl' HM.EApp (HM.EVar $ tupFun $ length as) as
        alg (CGVarF n) = HM.EVar n
        alg (CGLitF l) = HM.ELit l

freeVars :: CGEx -> S.Set Text
freeVars = F.cata alg
  where alg :: CGExF (S.Set Text) -> S.Set Text
        alg (CGCallF n es) = n `S.insert` S.unions es
        alg (CGCallPrmF _ es) = S.unions es
        alg (CGLetF x e1 e2) = e1 `S.union` S.delete x e2
        alg (CGIfF e1 e2 e3) = e1 `S.union` e2 `S.union` e3
        alg (CGTupF es) = S.unions es
        alg (CGVarF x) = S.singleton x
        alg _ = S.empty

binPrm :: Prm -> GExpr -> GExpr -> CGEx
binPrm p g1 g2 = CGCallPrm p $ map gToCGEx [g1, g2]

gLetToCGEx :: GELetExpr -> CGEx
gLetToCGEx (GELetExpr [] e)    = gToCGEx e
gLetToCGEx (GELetExpr (GELetIdent (LIdent i) h:t) e) =
  CGLet (pack i) (gToCGEx h) (gLetToCGEx $ GELetExpr t e)

gToCGEx :: GExpr -> CGEx
gToCGEx (GELogOr     e1 e2) = binPrm  POr e1 e2
gToCGEx (GELogAnd    e1 e2) = binPrm  PAnd e1 e2
gToCGEx (GEEq        e1 e2) = binPrm  PEq e1 e2
gToCGEx (GENotEq     e1 e2) = binPrm  PNeq e1 e2
gToCGEx (GEGtEq      e1 e2) = binPrm  PGeq e1 e2
gToCGEx (GELtEq      e1 e2) = binPrm  PLeq e1 e2
gToCGEx (GEGt        e1 e2) = binPrm  PGt  e1 e2
gToCGEx (GELt        e1 e2) = binPrm  PLt  e1 e2
gToCGEx (GEPlus      e1 e2) = binPrm  PAdd  e1 e2
gToCGEx (GEMinus     e1 e2) = binPrm  PSub  e1 e2
gToCGEx (GETimes     e1 e2) = binPrm  PTimes  e1 e2
gToCGEx (GEDiv       e1 e2) = binPrm  PDiv  e1 e2
gToCGEx (GEMod       e1 e2) = binPrm  PMod  e1 e2
gToCGEx (GEBitCat    e1 e2) = binPrm  PBitCat e1 e2
gToCGEx (GELogNot    e    ) = CGCallPrm PNot [gToCGEx e]

gToCGEx (GEIf b e1 e2) = CGIf (gToCGEx b) (gLetToCGEx e1) (gLetToCGEx e2)

gToCGEx (GEApp _ []) = notEnoughElements 1 "GEAppArg" "GEApp"
gToCGEx (GEApp (LIdent n) as) = CGCall (pack n) (map (\(GEAppArg x) -> gToCGEx x) as)
gToCGEx (GEAppScoped _ []) = notEnoughElements 1 "GEAppArg" "GEAppScoped"
gToCGEx (GEAppScoped (LScopedIdent n) as) = CGCall (pack n) (map (\(GEAppArg x) -> gToCGEx x) as)

gToCGEx (GEInt i) = CGLit $ LInt $ parseDecInt i
gToCGEx (GETrue)  = CGLit $ LBool True
gToCGEx (GEFalse) = CGLit $ LBool False
gToCGEx (GETup x xs) = let as = map (\(GETupArg a) -> a) (x:xs)
                        in if null as
                              then notEnoughElements 1 "GETupArg" "GETup"
                              else CGTup (map gToCGEx as)
gToCGEx (GEUnit)  = CGLit LUnit
gToCGEx (GEVar (LIdent i)) = CGVar (pack i)

