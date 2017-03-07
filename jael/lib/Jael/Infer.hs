{-# Language FlexibleInstances #-}
{-# Language NoImplicitPrelude #-}
{-# Language TypeSynonymInstances #-}

module Jael.Infer where

import           Jael.Prelude
import qualified Control.Comonad.Trans.Cofree as C
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import           Jael.Classes
import           Jael.Types
import           Jael.Util

type TypeSub = M.Map T.Text Type

nullSub :: TypeSub
nullSub = M.empty

compSub :: TypeSub -> TypeSub -> TypeSub
l `compSub` r = (M.map (apply l) r) `M.union` l

type HmEnv = M.Map T.Text TScheme

emptyEnv :: HmEnv
emptyEnv = M.empty

data HmInfE = HMTypeE
  deriving (Eq, Show)

data HmInfS = HmInfS
  { hmsCount :: Integer }
  deriving (Eq, Show)

initState :: HmInfS
initState = HmInfS 0

type HmInfM = RWST HmEnv [Constraint] HmInfS (Except T.Text)

hmInf :: HmEnv -> MaybeTypedExpr -> Either T.Text HMTypedExpr
hmInf env mTyped =
  let x = runIdentity $ runExceptT $ runRWST (cata doHm mTyped) env initState
  in case x of
       Left e -> Left e
       Right (typed, _, w) -> do
         s <- runIdentity $ runExceptT $ unificationSolver (nullSub, w)
         Right $ apply s typed

doHm :: C.CofreeF ExprF [QType] (HmInfM HMTypedExpr)
     -> HmInfM HMTypedExpr

doHm ([] C.:< EVarF n) = do
  env <- ask
  case M.lookup (value n) env of
    Nothing -> error $ "unbound variable " ++ show (value n)
    Just (Scheme tvs t) ->
      if null tvs
         then return $ t :< EVarF n
         else do tvs' <- mapM (const freshTv) tvs
                 return $ TIns (zip tvs tvs') t :< EVarF n

doHm ([] C.:< EIteF b t e) = do
  te1 <- b
  te2 <- t
  te3 <- e
  let (t1, t2, t3) = (getType te1, getType te2, getType te3)
  unify t1 $ TBool
  unify t2 t3
  return $ t2 :< EIteF te1 te2 te3

doHm ([] C.:< ELetF n e1 e2) = do
  env <- ask
  (t1 :< e1') <- e1
  let (s, t1') = generalize env t1
  te2 <- inEnv (value n, s) e2
  let t2 = getType te2
  return $ t2 :< ELetF n (t1' :< e1') te2

doHm ([] C.:< EAppF e1 e2) = do
  tv <- freshTv
  (te1, te2) <- liftM2 (,) e1 e2
  let (t1, t2) = (getType te1, getType te2)
  unify t1 (TFun t2 tv)
  return $ tv :< EAppF te1 te2

doHm ([] C.:< EAbsF n@(Token v _) e) = do
  tv <- freshTv
  te <- inEnv (v, Scheme [] tv) e
  return $ (TFun tv $ getType te) :< EAbsF n te

doHm ([] C.:< ETupF es) = do
  tes <- sequence es
  let tvs = map getType tes
  tupTv <- freshTv
  let t = TTup tvs
  unify tupTv t
  return $ t :< ETupF tes

doHm ([] C.:< EConF c) = do
  tv <- freshTv
  let t = case c of
            CUnit   -> TUnit
            CBool _ -> TBool
            CInt _  -> TInt
            CAdd    -> int2int2int
            CSub    -> int2int2int
            CMul    -> int2int2int
            CDiv    -> int2int2int
            CMod    -> int2int2int
            COr     -> bool2bool2bool
            CAnd    -> bool2bool2bool
            CEq     -> bool2bool2bool
            CNe     -> bool2bool2bool
            CGe     -> bool2bool2bool
            CLe     -> bool2bool2bool
            CGt     -> bool2bool2bool
            CLt     -> bool2bool2bool
            CBitCat -> TFun TBits TBits
            CNot    -> bool2bool
  unify tv t
  return $ tv :< EConF c

doHm (ts C.:< e) = do
  te <- doHm $ [] C.:< e
  let t = getType te
  mapM_ (unify t . removeAnn) ts
  return te

inEnv :: (T.Text, TScheme) -> HmInfM a -> HmInfM a
inEnv (n, s) m = do
  let scope e = M.insert n s $ M.delete n e
  local scope m

unify :: Type -> Type -> HmInfM ()
unify t1 t2 = tell [(t1, t2)]

generalize :: HmEnv -> Type -> (TScheme, Type)
generalize env t =
  let fvs = ftv t S.\\ ftv env
      t' = if null fvs
              then t
              else TGen (S.toList fvs) t
      s = Scheme (S.toList fvs) t'
  in (s, t')

type Constraint = (Type, Type)
type Unifier = (TypeSub, [Constraint])
type SolveM = ExceptT T.Text Identity

unificationSolver :: Unifier -> SolveM TypeSub
unificationSolver (s, cs) =
  case cs of
    [] -> return s
    ((t1, t2): cs') -> do
      s' <- unifier t1 t2
      unificationSolver (s' `compSub` s, apply s' cs')

unifier :: Type -> Type -> SolveM TypeSub
unifier t1 t2 | t1 == t2 = return nullSub
unifier (TIns s t1) t2 = unifier (apply (M.fromList s) t1) t2
unifier t1 (TIns s t2) = unifier t1 (apply (M.fromList s) t2)
unifier (TGen _ t1) t2 = unifier t1 t2
unifier t1 (TGen _ t2) = unifier t1 t2
unifier (TFun l r) (TFun l' r') = unifyMany [l, r] [l', r']
unifier (TVar n) t = bind n t
unifier t (TVar n) = bind n t
unifier (TTup ts) (TTup ts') = unifyMany ts ts'
unifier (TCon (Token n _) ts) (TCon (Token n' _) ts') =
  if n == n'
     then unifyMany ts ts'
     else error $ "can't match " ++ show n ++ " and " ++ show n'
unifier t t' = error $ "Non-unifying types " ++ show t ++ " and " ++ show t'

bind :: Ident -> Type -> SolveM TypeSub
bind v t
  | ((Token n _), (TVar (Token n' _))) <- (v, t)
  , n == n'
  = return nullSub
  | S.member (value v) (ftv t)
  = error "occurs check"
  | otherwise
  = return $ M.singleton (value v) t

unifyMany :: [Type] -> [Type] -> SolveM TypeSub
unifyMany [] [] = return nullSub
unifyMany (t1:t1s) (t2:t2s) = do
  s1 <- unifier t1 t2
  s2 <- unifyMany (apply s1 t1s) (apply s1 t2s)
  return (s2 `compSub` s1)
unifyMany _ _ = error "bleh"

emptyUnifier :: Unifier
emptyUnifier = (nullSub, [])

int2int2int :: Type
int2int2int = TFun TInt (TFun TInt TInt)

bool2bool :: Type
bool2bool = TFun TBool TBool

bool2bool2bool :: Type
bool2bool2bool = TFun TBool bool2bool

freshTv :: HmInfM Type
freshTv = do
  c <- gets hmsCount
  modify (\_ -> HmInfS $ c + 1)
  return $ TVar $ Token (T.pack $ 'a' : show c) dummyLoc
