{-# Language NoImplicitPrelude #-}

-- Implementation based off of https://github.com/wh5a/Algorithm-W-Step-By-Step

module Jael.Seq.TI where

import ClassyPrelude
import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Seq.AST

data SeqTIState = SeqTIState
  { tvCount :: Integer
  , tiErrors :: [Text]
  }

newtype SeqTI a = SeqTI (SeqTIState -> (Maybe a, SeqTIState))

instance Monad SeqTI where
  (SeqTI p) >>= f = SeqTI $ \s -> case p s of
                                       (Just v,  s') -> let (SeqTI n) = f v
                                                        in  n s'
                                       (Nothing, s') -> (Nothing, s')
  return v = SeqTI $ \s -> (Just v, s)

instance Applicative SeqTI where
  pure = return
  (<*>) = ap

instance Functor SeqTI where
  fmap = liftM

seqInfer :: TyEnv -> Ex -> Either [Text] Ty
seqInfer env = runSeqTI . seqTypeInference env

seqTypeEx :: TyEnv -> Ex -> Either [Text] TypedEx
seqTypeEx env = runSeqTI . seqTypeEx env

runSeqTI :: SeqTI a -> Either [Text] a
runSeqTI t = let (SeqTI stateFunc) = t
                 initState = SeqTIState{ tvCount = 0, tiErrors = [] }
             in  case stateFunc initState of
                      (Just v,  _) -> Right v
                      (Nothing, s) -> Left $ tiErrors s

seqTypeInference :: TyEnv -> Ex -> SeqTI Ty
seqTypeInference env e = do
  (sub, te) <- ti env e
  return $ apply sub (tyOf te)

seqTypeEx :: TyEnv -> Ex -> SeqTI TypedEx
seqTypeEx env e = do
  (sub, te) <- ti env e
  return $ apply sub te

getTvCount :: SeqTI Integer
getTvCount = SeqTI $ \s -> (Just (tvCount s), s)

incTvCount :: SeqTI ()
incTvCount = SeqTI $ \s -> (Just (), s{tvCount = tvCount s + 1})

newTV :: SeqTI Ty
newTV = getTvCount >>= (\i -> (incTvCount >>) $ return . TVar $ "a" ++ tshow i)

getTiErrors :: SeqTI [Text]
getTiErrors = SeqTI $ \s -> (Just $ tiErrors s, s)

putTiErrors :: [Text] -> SeqTI ()
putTiErrors ts = SeqTI $ \s -> (Just (), s{tiErrors=ts})

-- Halts inference and records the error
tiError :: Text -> SeqTI a
tiError t = getTiErrors >>= (\ts -> putTiErrors $ t:ts)
                        >> (SeqTI $ \s -> (Nothing, s))

remove :: TyEnv -> Text -> TyEnv
remove env t = M.delete t env

nullSub :: TySub
nullSub = M.empty

compSub :: TySub -> TySub -> TySub
compSub a b = M.map (apply a) b `M.union` a

-- Creates a scheme from a type by adding the qualified type variables of the
-- environment
generalization :: TyEnv -> Ty -> PolyTy
generalization env t = PolyTy (S.toList $ ftv t `S.difference` ftv env) t

-- Creates a type from a scheme by making new type variables and applying
-- a substituion from the old to the new
instantiation :: PolyTy -> SeqTI Ty
instantiation (PolyTy vs ty) = do
  nvs <- mapM (const newTV) vs
  return $ apply (M.fromList $ zip vs nvs) ty

-- Most general unifier. Used in the application rule for determining the return
-- type after application to a function
mgu :: Ty -> Ty -> Either Text TySub
mgu (TFun l1 r1) (TFun l2 r2) = do
  sub1 <- mgu l1 l2
  sub2 <- mgu (apply sub1 r1) (apply sub1 r2)
  return $ sub2 `compSub` sub1
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu TInt    TInt    = Right nullSub
mgu TBool   TBool   = Right nullSub
mgu (TNamed n xs) (TNamed m ys) =
  if n /= m
     then Left $ "Attempted to unify named types with different names: " ++
                 tshow n ++ " " ++ tshow m
     else foldM (\sub (x, y) ->
                   liftA (M.unionWith (error "Expected unique keys") sub) (mgu x y)
                ) M.empty (zip xs ys)
mgu t1 t2 = Left $ "Types \"" ++ tshow t1 ++ "\" and \"" ++ tshow t2 ++
                   "\" do not unify."

varBind :: Text -> Ty -> Either Text TySub
varBind u t
  | t == TVar u        = Right nullSub
  | S.member u (ftv t) = Left $ "Can not bind \"" ++ tshow u ++ "\" to \""
      ++ tshow t ++ "\" because \"" ++ tshow u
      ++ "\" is a free type variable of \"" ++ tshow t
  | otherwise          = Right $ M.singleton u t

ti :: TyEnv -> Ex -> SeqTI (TySub, TypedEx)
-- Variables
ti env (EVar v) = case M.lookup v env of
    Nothing -> tiError $ "unbound variable \"" ++ tshow v ++ "\""
    Just sigma -> do
       t <- instantiation sigma
       return (nullSub, TEVar t v)

-- Literals
ti _ (EUnit)   = return (nullSub, TEUnit TUnit)
ti _ (EInt x)  = return (nullSub, TEInt TInt x)
ti _ (EBool x) = return (nullSub, TEBool TBool x)

-- Function application
ti env (EApp e1 e2) = do
  tv <- newTV
  (sub1, te1) <- ti env e1
  (sub2, te2) <- ti (apply sub1 env) e2
  let sub3 = mgu (apply sub2 (tyOf te1)) (TFun (tyOf te2) tv)
  case sub3 of
       Left err -> tiError
                     (err ++ "\n\n"
                          ++ "Type variable : " ++ tshow tv ++ "\n\n"
                          ++ "Inference 1   : " ++ tshow (te1, sub1) ++ "\n"
                          ++ "   for expr   : " ++ tshow e1 ++ "\n\n"
                          ++ "Inference 2   : " ++ tshow (te2, sub2) ++ "\n"
                          ++ "   for expr   : " ++ tshow e2 ++ "\n\n"
                     )
       Right sub3' -> return (sub3' `compSub` sub2 `compSub` sub1, TEApp (apply sub3' tv) te1 te2)

-- Abstraction
ti env (EAbs x e) = do
  tv <- newTV
  let env' = remove env x
      env'' = env' `M.union` M.singleton x (PolyTy [] tv)
  (s1, te1) <- ti env'' e
  return (s1, TEAbs (TFun (apply s1 tv) (tyOf te1)) x te1)

-- Let
ti env (ELet x e1 e2) = do
  (s1, te1) <- ti env e1
  let env' = remove env x
      t' = generalization (apply s1 env) (tyOf te1)
      env'' = M.insert x t' env'
  (s2, te2) <- ti (apply s1 env'') e2
  return (s2 `compSub` s1, TELet (tyOf te2) x te1 te2)

