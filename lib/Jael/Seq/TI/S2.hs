module Jael.Seq.TI.S2 where

import qualified Data.Functor.Foldable as F
import qualified Data.Map as M
import           Jael.Seq.AST
import           Jael.Seq.Env
import           Jael.Seq.TI.HM
import           Jael.Seq.Types
import           Jael.Util

data S2TypeErr = ArityMismatch (M.Map Text (Integer, Integer))
               | S2HMTyMismatch HMTy
               | InferenceErr SeqTIErr
               deriving (Eq, Show)

checkArityErr :: M.Map Text Integer -> Text -> [M.Map Text (Integer, Integer)]
              -> M.Map Text (Integer, Integer)
checkArityErr arityMap n as =
  let argErrs = M.unions as
      nArity = M.findWithDefault (error $ "Expected to find " <> show n <> " in\
                                          \ the seq type env.") n arityMap
      lengthInteger = toInteger $ length as
   in if nArity /= lengthInteger
         then M.insert n (nArity, lengthInteger) argErrs
         else argErrs

arityCheck :: HMTyEnv -> S1Ex -> Either S2TypeErr ()
arityCheck (HMTyEnv envMap) expr =
  let arityMap = M.map (\(HMPolyTy _ t) -> arityOf t) envMap
      arityAlg (S1CallF n as)    = checkArityErr arityMap n as
      arityAlg (S1CallPrmF p as) = checkArityErr arityMap (tshow p) as
      arityAlg _ = M.empty
      arityErrs = F.cata arityAlg expr
   in unless (null arityErrs) (Left $ ArityMismatch arityErrs)

typeCheckHM :: HMTyEnv -> HMEx -> HMTy -> Either S2TypeErr HMTypedEx
typeCheckHM env expr ty = case seqInferTypedEx env expr of
       Left err -> Left $ InferenceErr err
       Right te ->
         let inferred = hmTyOf te
          in case mgu inferred ty of
                  Left  _ -> throwError $ S2HMTyMismatch inferred
                  Right _ -> return te

toHM :: S1Ex -> HMEx
toHM = F.cata alg
  where alg (S1CallF n as) = foldl' HMApp (HMVar n) as
        alg (S1CallPrmF p as) = foldl' HMApp(HMVar $ tshow p) as
        alg (S1LetF n e1 e2) = HMLet n e1 e2
        alg (S1IfF b e1 e2) = HMApp (HMApp (HMApp (HMVar "if") b) e1) e2
        alg (S1TupF as) = foldl' HMApp (HMVar $ tupFun $ length as) as
        alg (S1VarF n) = HMVar n
        alg (S1LitF l) = HMLit l

-- Returns either an error or the type of the expression and a map of the types
-- inferred for the names within the expression.
typeCheck :: HMTyEnv -> S1Ex -> HMTy -> Either S2TypeErr S2TyEx
typeCheck env expr ty = do
  arityCheck env expr
  hmEx <- typeCheckHM env (toHM expr) ty
  hmToS2Expr expr hmEx

dropAbs :: Int -> HMTypedEx -> HMTypedEx
dropAbs 0 expr = expr
dropAbs n (HMTypedEx Ann{unAnn=(HMAbsF _ expr)}) = dropAbs (n-1) expr
dropAbs _ _ = error "Attempted to remove abstraction where there was none."

-- CGEx can't do abstraction so we need to do some juggling to check a function
typeCheckFunc :: HMTyEnv -> ([(Text, S1Ty)], S1Ty, S1Ex)
              -> Either S2TypeErr ([S2Ty], S2Ty, S2TyEx)
typeCheckFunc env (s1Args, s1RetTy, s1Expr) = do
  arityCheck env s1Expr
  let hmExpr = toHM s1Expr
      hmFunc = foldr (HMAbs . fst) hmExpr s1Args
      hmFunTy = typesToFun (map (hmTyOf . snd) s1Args, hmTyOf s1RetTy)
  hmTypedEx <- typeCheckHM env hmFunc hmFunTy
  let (hmArgs, hmRetTy) = funToTypes (hmTyOf hmTypedEx)
  hmToS2Func (map snd s1Args, s1RetTy, s1Expr) (hmArgs, hmRetTy, dropAbs (length hmArgs) hmTypedEx)

typeInf :: HMTyEnv -> S1Ex -> Either S2TypeErr S2TyEx
typeInf env s1Expr = do
  arityCheck env s1Expr
  hmTyEx <- case seqInferTypedEx env (toHM s1Expr) of
                 Left err -> Left $ InferenceErr err
                 Right te -> return te
  hmToS2Expr s1Expr hmTyEx

-- Merge the original S1Ex with the inferred-typed expression to get a S2TyEx
hmToS2Expr :: S1Ex -> HMTypedEx -> Either S2TypeErr S2TyEx
hmToS2Expr = undefined

hmToS2Func :: ([S1Ty], S1Ty, S1Ex)
           -> ([HMTy], HMTy, HMTypedEx)
           -> Either S2TypeErr ([S2Ty], S2Ty, S2TyEx)
hmToS2Func = undefined

