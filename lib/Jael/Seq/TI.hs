-- Implementation based off of https://github.com/wh5a/Algorithm-W-Step-By-Step

module Jael.Seq.TI where

import qualified Data.Map as M
import qualified Data.Set as S
import           Jael.Seq.Env
import           Jael.Seq.HM_AST
import           Jael.Seq.Types

data SeqTIErr = NamedUnificationMismatch Text Text
              | NonUnification HMTy HMTy
              | FreeTyVarBind Text HMTy
              | UnboundVar Text
  deriving (Eq, Show)

data SeqTIState = SeqTIState
  { tvCount :: Integer
  }

newtype SeqTI a = SeqTI (SeqTIState -> (Either SeqTIErr a, SeqTIState))

instance Monad SeqTI where
  (SeqTI p) >>= f = SeqTI $ \s -> case p s of
                                       (Right v,  s') -> let (SeqTI n) = f v
                                                          in  n s'
                                       (Left e, s') -> (Left e, s')
  return v = SeqTI $ \s -> (Right v, s)

instance Applicative SeqTI where
  pure = return
  (<*>) = ap

instance Functor SeqTI where
  fmap = liftM

type HMTySub = M.Map Text HMTy

seqInfer :: HMTyEnv -> Ex -> Either SeqTIErr HMTy
seqInfer env = runSeqTI . seqTypeInference env

seqInferTypedEx :: HMTyEnv -> Ex -> Either SeqTIErr HMTypedEx
seqInferTypedEx env = runSeqTI . seqTypedExInference env

runSeqTI :: SeqTI a -> Either SeqTIErr a
runSeqTI t = let (SeqTI stateFunc) = t
                 initState = SeqTIState{ tvCount = 0 }
              in fst (stateFunc initState)

seqTypeInference :: HMTyEnv -> Ex -> SeqTI HMTy
seqTypeInference env = liftM hmTyOf . seqTypedExInference env

seqTypedExInference :: HMTyEnv -> Ex -> SeqTI HMTypedEx
seqTypedExInference env e = do
  (sub, te) <- ti env e
  return $ apply sub te

getTvCount :: SeqTI Integer
getTvCount = SeqTI $ \s -> (Right (tvCount s), s)

incTvCount :: SeqTI ()
incTvCount = SeqTI $ \s -> (Right (), s{tvCount = tvCount s + 1})

newTV :: SeqTI HMTy
newTV = getTvCount >>= (\i -> (incTvCount >>) $ return . HMTyVar $ "a" <> (pack . show) i)

-- Halts inference and records the error
tiError :: SeqTIErr -> SeqTI a
tiError err = SeqTI $ \s -> (Left err, s)

teRemove :: HMTyEnv -> Text -> HMTyEnv
teRemove (HMTyEnv env) t = HMTyEnv $ M.delete t env

teInsert :: Text -> HMPolyTy -> HMTyEnv -> HMTyEnv
teInsert x t (HMTyEnv m) = HMTyEnv $ M.insert x t m

teLookup :: Text -> HMTyEnv -> Maybe HMPolyTy
teLookup x (HMTyEnv m) = M.lookup x m

nullSub :: HMTySub
nullSub = M.empty

compSub :: HMTySub -> HMTySub -> HMTySub
compSub a b = M.map (apply a) b `M.union` a

-- Creates a scheme from a type by adding the qualified type variables of the
-- environment
generalization :: HMTyEnv -> HMTy -> HMPolyTy
generalization env t = HMPolyTy (S.toList $ ftv t `S.difference` ftv env) t

-- Creates a type from a scheme by making new type variables and applying
-- a substituion from the old to the new
instantiation :: HMPolyTy -> SeqTI HMTy
instantiation (HMPolyTy vs ty) = do
  nvs <- mapM (const newTV) vs
  return $ apply (M.fromList $ zip vs nvs) ty

-- Most general unifier. Used in the application rule for determining the return
-- type after application to a function
mgu :: HMTy -> HMTy -> Either SeqTIErr HMTySub
mgu (HMTyFun l1 r1) (HMTyFun l2 r2) = do
  sub1 <- mgu l1 l2
  sub2 <- mgu (apply sub1 r1) (apply sub1 r2)
  return $ sub2 `compSub` sub1
mgu (HMTyVar u) t = varBind u t
mgu t (HMTyVar u) = varBind u t
mgu (HMTyNamed n xs) (HMTyNamed m ys) =
  if n /= m
     then Left $ NamedUnificationMismatch n m
     else foldM (\sub (x, y) ->
                   liftA (M.unionWith (error "Expected unique keys") sub) (mgu x y)
                ) M.empty (zip xs ys)
mgu t1 t2 | t1 == t2 = Right nullSub
mgu t1 t2 = Left $ NonUnification t1 t2

varBind :: Text -> HMTy -> Either SeqTIErr HMTySub
varBind u t@(HMTyVar t')
  | u == t'    = Right nullSub
  | otherwise  = Right $ M.singleton u t
varBind u t
  | S.member u (ftv t) = Left $ FreeTyVarBind u t
  | otherwise          = Right $ M.singleton u t

ti :: HMTyEnv -> Ex -> SeqTI (HMTySub, HMTypedEx)
-- Variables
ti env (EVar v) = case teLookup v env of
    Nothing -> tiError $ UnboundVar v
    Just sigma -> do
       t <- instantiation sigma
       return (nullSub, mkTyped t $ EVarF v)

-- Function application
ti env (EApp e1 e2) = do
  tv <- newTV
  (sub1, te1) <- ti env e1
  (sub2, te2) <- ti (apply sub1 env) e2
  let sub3 = mgu (apply sub2 (hmTyOf te1)) (HMTyFun (hmTyOf te2) tv)
  case sub3 of
       Left err -> tiError err
       Right sub3' -> return ( sub3' `compSub` sub2 `compSub` sub1
                             , mkTyped (apply sub3' tv) $ EAppF te1 te2)

-- Abstraction
ti env (EAbs x e) = do
  tv <- newTV
  let env' = teRemove env x
      env'' = teInsert x (HMPolyTy [] tv) env'
  (s1, te1) <- ti env'' e
  return (s1, mkTyped (HMTyFun (apply s1 tv) (hmTyOf te1)) $ EAbsF x te1)

-- Let
ti env (ELet x e1 e2) = do
  (s1, te1) <- ti env e1
  let env' = teRemove env x
      t' = generalization (apply s1 env) (hmTyOf te1)
      env'' = teInsert x t' env'
  (s2, te2) <- ti (apply s1 env'') e2
  return (s2 `compSub` s1, mkTyped (hmTyOf te2) $ ELetF x te1 te2)

-- Literals
ti _ (ELit lit) = return (nullSub, mkTyped (hmTyOf lit) $ ELitF lit)

-- Primitives
ti _ (EPrm x) = return (nullSub, mkTyped (hmTyOf x) $ EPrmF x)

mkTyped :: HMTy -> ExF HMTypedEx -> HMTypedEx
mkTyped t e = HMTypedEx Ann {ann=t, unAnn=e}

