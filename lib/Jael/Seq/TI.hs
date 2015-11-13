-- Implementation based off of https://github.com/wh5a/Algorithm-W-Step-By-Step

module Jael.Seq.TI where

import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Seq.AST
import Jael.Seq.Types

data SeqTIErr = NamedUnificationMismatch Text Text
              | NonUnification Ty Ty
              | FreeTyVarBind Text Ty
              | UnboundVar Text
  deriving (Eq, Show)

data SeqTIState = SeqTIState
  { tvCount :: Integer
  , tiErrors :: [SeqTIErr]
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

seqInfer :: TyEnv -> Ex -> Either [SeqTIErr] Ty
seqInfer env = runSeqTI . seqTypeInference env

seqInferTypedEx :: TyEnv -> Ex -> Either [SeqTIErr] TypedEx
seqInferTypedEx env = runSeqTI . seqTypedExInference env

runSeqTI :: SeqTI a -> Either [SeqTIErr] a
runSeqTI t = let (SeqTI stateFunc) = t
                 initState = SeqTIState{ tvCount = 0, tiErrors = [] }
             in  case stateFunc initState of
                      (Just v,  _) -> Right v
                      (Nothing, s) -> Left $ tiErrors s

seqTypeInference :: TyEnv -> Ex -> SeqTI Ty
seqTypeInference env = liftM tyOf . seqTypedExInference env

seqTypedExInference :: TyEnv -> Ex -> SeqTI TypedEx
seqTypedExInference env e = do
  (sub, te) <- ti env e
  return $ apply sub te

getTvCount :: SeqTI Integer
getTvCount = SeqTI $ \s -> (Just (tvCount s), s)

incTvCount :: SeqTI ()
incTvCount = SeqTI $ \s -> (Just (), s{tvCount = tvCount s + 1})

newTV :: SeqTI Ty
newTV = getTvCount >>= (\i -> (incTvCount >>) $ return . TyVar $ "a" <> (pack . show) i)

getTiErrors :: SeqTI [SeqTIErr]
getTiErrors = SeqTI $ \s -> (Just $ tiErrors s, s)

putTiErrors :: [SeqTIErr] -> SeqTI ()
putTiErrors ts = SeqTI $ \s -> (Just (), s{tiErrors=ts})

-- Halts inference and records the error
tiError :: SeqTIErr -> SeqTI a
tiError t = getTiErrors >>= (\ts -> putTiErrors $ t:ts)
                        >> (SeqTI $ \s -> (Nothing, s))

remove :: TyEnv -> Text -> TyEnv
remove (TyEnv env) t = TyEnv $ M.delete t env

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
mgu :: Ty -> Ty -> Either SeqTIErr TySub
mgu (TFun l1 r1) (TFun l2 r2) = do
  sub1 <- mgu l1 l2
  sub2 <- mgu (apply sub1 r1) (apply sub1 r2)
  return $ sub2 `compSub` sub1
mgu (TyVar u) t = varBind u t
mgu t (TyVar u) = varBind u t
mgu TInt    TInt    = Right nullSub
mgu TBool   TBool   = Right nullSub
mgu (TNamed n xs) (TNamed m ys) =
  if n /= m
     then Left $ NamedUnificationMismatch n m
     else foldM (\sub (x, y) ->
                   liftA (M.unionWith (error "Expected unique keys") sub) (mgu x y)
                ) M.empty (zip xs ys)
mgu t1 t2 = Left $ NonUnification t1 t2

varBind :: Text -> Ty -> Either SeqTIErr TySub
varBind u t@(TyVar t')
  | u == t'    = Right nullSub
  | otherwise  = Right $ M.singleton u t
varBind u t
  | S.member u (ftv t) = Left $ FreeTyVarBind u t
  | otherwise          = Right $ M.singleton u t

ti :: TyEnv -> Ex -> SeqTI (TySub, TypedEx)
-- Variables
ti (TyEnv env) (EVar v) = case M.lookup v env of
    Nothing -> tiError $ UnboundVar v
    Just sigma -> do
       t <- instantiation sigma
       return (nullSub, mkTyped t $ EVarF v)

-- Function application
ti env (EApp e1 e2) = do
  tv <- newTV
  (sub1, te1) <- ti env e1
  (sub2, te2) <- ti (apply sub1 env) e2
  let sub3 = mgu (apply sub2 (tyOf te1)) (TFun (tyOf te2) tv)
  case sub3 of
       Left err -> tiError err
       Right sub3' -> return ( sub3' `compSub` sub2 `compSub` sub1
                             , mkTyped (apply sub3' tv) $ EAppF te1 te2)

-- Abstraction
ti env (EAbs x e) = do
  tv <- newTV
  let (TyEnv env') = remove env x
      env'' = TyEnv $ env' `M.union` M.singleton x (PolyTy [] tv)
  (s1, te1) <- ti env'' e
  return (s1, mkTyped (TFun (apply s1 tv) (tyOf te1)) $ EAbsF x te1)

-- Let
ti env (ELet x e1 e2) = do
  (s1, te1) <- ti env e1
  let (TyEnv env') = remove env x
      t' = generalization (apply s1 env) (tyOf te1)
      env'' = TyEnv $ M.insert x t' env'
  (s2, te2) <- ti (apply s1 env'') e2
  return (s2 `compSub` s1, mkTyped (tyOf te2) $ ELetF x te1 te2)

-- Literals
ti _ (ELit LUnit)     = return (nullSub, mkTyped TUnit $ ELitF LUnit)
ti _ (ELit (LInt x))  = return (nullSub, mkTyped TInt  $ ELitF $ LInt x)
ti _ (ELit (LBool x)) = return (nullSub, mkTyped TBool $ ELitF $ LBool x)
ti _ (ELit (LBit x))  = return (nullSub, mkTyped TBit  $ ELitF $ LBit x)

-- Primitives
ti _ (EPrm x) =
  let instPrim :: PolyTy -> SeqTI (TySub, TypedEx)
      instPrim p = instantiation p >>= \t -> return (nullSub, mkTyped t $ EPrmF x)
   in instPrim $
        case x of
             PIf    -> PolyTy ["a"] (TFun TBool (TFun (TyVar "a") (TFun (TyVar "a") (TyVar "a"))))
             PAdd   -> PolyTy ["a"] (TFun (TyVar "a") (TFun (TyVar "a") (TyVar "a")))
             PSub   -> PolyTy ["a"] (TFun (TyVar "a") (TFun (TyVar "a") (TyVar "a")))
             PTimes -> PolyTy ["a"] (TFun (TyVar "a") (TFun (TyVar "a") (TyVar "a")))
             PDiv   -> PolyTy [] (TFun TInt (TFun TInt (TNamed "IntDivRes" [])))
             PMod   -> PolyTy [] (TFun TInt (TFun TInt TInt))
             POr    -> PolyTy [] (TFun TBool (TFun TBool TBool))
             PAnd   -> PolyTy [] (TFun TBool (TFun TBool TBool))
             PEq    -> PolyTy ["a"] (TFun (TyVar "a") (TFun (TyVar "a") TBool))
             PNeq   -> PolyTy ["a"] (TFun (TyVar "a") (TFun (TyVar "a") TBool))
             PGeq   -> PolyTy ["a"] (TFun (TyVar "a") (TFun (TyVar "a") TBool))
             PLeq   -> PolyTy ["a"] (TFun (TyVar "a") (TFun (TyVar "a") TBool))
             PGt    -> PolyTy ["a"] (TFun (TyVar "a") (TFun (TyVar "a") TBool))
             PLt    -> PolyTy ["a"] (TFun (TyVar "a") (TFun (TyVar "a") TBool))
             PNot   -> PolyTy ["a"] (TFun (TyVar "a") (TyVar "a"))
             PBitCat -> PolyTy [] (TFun TBit (TFun TBit TBit))

