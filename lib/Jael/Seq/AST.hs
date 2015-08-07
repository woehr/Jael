{-# Language NoImplicitPrelude, TypeSynonymInstances, FlexibleInstances #-}

-- Implementation based off of https://github.com/wh5a/Algorithm-W-Step-By-Step

module Jael.Seq.AST where

import ClassyPrelude
import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Grammar

data Lit  = LInt Integer
          | LBool Bool
            deriving (Show)

data Ex = EVar Text
        | ELit Lit
        | EApp Ex Ex
        | EAbs Text Ex
        | ELet Text Ex Ex
          deriving (Show)

data Ty = TVar Text
        | TInt
        | TBool
        | TFun Ty Ty
          deriving (Eq, Show)

data PolyTy = PolyTy [Text] Ty
              deriving (Show)

type TyEnv = M.Map Text PolyTy

type TySub = M.Map Text Ty

data SeqTIState = SeqTIState {
  tvCount :: Integer,
  tiErrors :: [Text]
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

runSeqTI :: SeqTI a -> Either [Text] a
runSeqTI t = let (SeqTI stateFunc) = t
                 initState = SeqTIState{ tvCount = 0, tiErrors = [] }
             in  case stateFunc initState of
                      (Just v,  s) -> Right v
                      (Nothing, s) -> Left $ tiErrors s

getTvCount :: SeqTI Integer
getTvCount = SeqTI $ \s -> (Just (tvCount s), s)

incTvCount :: SeqTI ()
incTvCount = SeqTI $ \s -> (Just (), s{tvCount = (tvCount s) + 1})

newTV :: SeqTI Ty
newTV = incTvCount >> getTvCount >>= (\i -> return $ TVar ("a" ++ tshow i))

getTiErrors :: SeqTI [Text]
getTiErrors = SeqTI $ \s -> (Just $ tiErrors s, s)

putTiErrors :: [Text] -> SeqTI ()
putTiErrors ts = SeqTI $ \s -> (Just (), s{tiErrors=ts})

tiError :: Text -> SeqTI a
tiError t = getTiErrors >>= (\ts -> putTiErrors $ t:ts) >> (SeqTI $ \s -> (Nothing, s))

class TyOps a where
  ftv :: a -> S.Set Text
  apply :: TySub -> a -> a

instance TyOps Ty where
  ftv (TVar t)     = S.singleton t
  ftv TInt         = S.empty
  ftv TBool        = S.empty
  ftv (TFun t1 t2) = S.union (ftv t1) (ftv t2)

  apply s t@(TVar v) =
    case M.lookup v s of
      Nothing   -> t
      Just newt -> newt
  apply s (TFun t1 t2) = TFun (apply s t1) (apply s t2)
  apply _ t = t

instance TyOps PolyTy where
  -- Free type variables of a type scheme are the ones not bound by a universal
  -- quantifier. I.e., the type variables within t not in vs
  ftv (PolyTy vs t) = (ftv t) `S.difference` (S.fromList vs)
  -- This first deletes the variables of the scheme from the substitution then
  -- applies the substitution
  apply s (PolyTy vs t) = PolyTy vs (apply (foldr M.delete s vs) t)

instance TyOps a => TyOps [a] where
  ftv ls = foldr S.union S.empty $ map ftv ls
  apply s ls = map (apply s) ls

instance TyOps TyEnv where
  ftv env = ftv $ M.elems env
  apply sub env = M.map (apply sub) env

remove :: TyEnv -> Text -> TyEnv
remove env t = M.delete t env

nullSub :: TySub
nullSub = M.empty

compSub :: TySub -> TySub -> TySub
compSub a b = M.union (M.map (apply a) b) a

-- Creates a scheme from a type by adding the qualified type variables of the
-- environment
generalization :: TyEnv -> Ty -> PolyTy
generalization env t = PolyTy (S.toList $ ftv t `S.difference` ftv env) t

-- Creates a type from a scheme by making new type variables and applying
-- a substituion from the old to the new
instantiation :: PolyTy -> SeqTI Ty
instantiation (PolyTy vs ty) = do
  nvs <- mapM (\_ -> newTV) vs
  return $ apply (M.fromList $ zip vs nvs) ty

-- Most general unifier. Used in the application rule for determining the return
-- type after application to a function
mgu :: Ty -> Ty -> Either Text TySub
mgu (TFun l1 r1) (TFun l2 r2) = do
  sub1 <- mgu l1 l2
  sub2 <- mgu (apply sub1 r1) (apply sub1 r2)
  return $ compSub sub1 sub2
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu TInt    TInt    = Right nullSub
mgu TBool   TBool   = Right nullSub
mgu t1 t2 = Left $ "Types \"" ++ tshow t1 ++ "\" and \"" ++ tshow t2 ++ "\" do not unify."

varBind :: Text -> Ty -> Either Text TySub
varBind u t
  | t == TVar u        = Right nullSub
  | S.member u (ftv t) = Left $ "Can not bind \"" ++ tshow u ++ "\" to \""
      ++ tshow t ++ "\" because \"" ++ tshow u ++ "\" is a free type variable of \""
      ++ tshow t
  | otherwise          = Right $ M.singleton u t

ti :: TyEnv -> Ex -> SeqTI (TySub, Ty)
-- Literals
ti _ (ELit (LInt _))    = return (nullSub, TInt)
ti _ (ELit (LBool _))   = return (nullSub, TBool)

-- Variables
ti env (EVar v) = do
  case M.lookup v env of
    Nothing -> tiError $ "unbound variable \"" ++ tshow v ++ "\""
    Just sigma -> do
       t <- instantiation sigma
       return (nullSub, t)

-- Function application
ti env (EApp e1 e2) = do
  tv <- newTV
  (sub1, t1) <- ti env e1
  (sub2, t2) <- ti (apply sub1 env) e2
  let sub3 = mgu (apply sub2 t1) (TFun t2 tv)
  case sub3 of
       Left err -> tiError (err ++ "\n\n"
                                ++ "Type variable : " ++ tshow tv ++ "\n\n"
                                ++ "Inference 1   : " ++ tshow (t1, sub1) ++ "\n"
                                ++ "   for expr   : " ++ tshow e1 ++ "\n\n"
                                ++ "Inference 2   : " ++ tshow (t2, sub2) ++ "\n"
                                ++ "   for expr   : " ++ tshow e2 ++ "\n\n"
                           )
       Right sub3 -> return (sub3, apply sub3 tv)

-- Abstraction
-- Let

-- TODO: Remove when implemented
ti _ _ = tiError "Unimplemented"

seqTypeInference :: TyEnv -> Ex -> SeqTI Ty
seqTypeInference env e = do
  (sub, ty) <- ti env e
  return $ apply sub ty

-- The LetExpr grammar is only allowed in certain places so it isn't of the GExpr type
letExprToEx :: GELetExpr -> Ex
letExprToEx (GELetExpr [] e)    = toSeqEx e
-- i[dentifier]; h[ead] e[xpression]; t[ail] l[et expression]s; e[xpression]
letExprToEx (GELetExpr ((GELetIdent (LIdent i) he):tls) e) = ELet (pack i) (toSeqEx he) (letExprToEx $ GELetExpr tls e)

-- Converts grammar to AST0 but does not verify its correctness
toSeqEx :: GExpr -> Ex

toSeqEx (GEAbs [] le) = error "This case should be forbidden by the grammar."
toSeqEx (GEAbs ((GEAbsArg (LIdent i)):[]) le) = EAbs (pack i) (letExprToEx le)
toSeqEx (GEAbs ((GEAbsArg (LIdent i)):xs) le) = EAbs (pack i) (toSeqEx $ GEAbs xs le)

toSeqEx (GEIf b e1 e2) = EApp (EApp (EApp (EVar "if") (toSeqEx b)) (letExprToEx e1)) (letExprToEx e2)

toSeqEx (GEPlus  e1 e2) = EApp (EApp (EVar "+") (toSeqEx e1)) (toSeqEx e2)
toSeqEx (GEMinus e1 e2) = EApp (EApp (EVar "-") (toSeqEx e1)) (toSeqEx e2)
toSeqEx (GETimes e1 e2) = EApp (EApp (EVar "*") (toSeqEx e1)) (toSeqEx e2)

toSeqEx (GEVar (LIdent i)) = EVar (pack i)

toSeqEx (GEInt    i) = ELit (LInt  i)
toSeqEx (GEIntNeg i) = ELit (LInt (-i))

toSeqEx (GETrue)  = ELit (LBool True)
toSeqEx (GEFalse) = ELit (LBool False)

