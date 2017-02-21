{-# Language FlexibleInstances #-}
{-# Language NoImplicitPrelude #-}
{-# Language TypeSynonymInstances #-}

module Jael.Infer where

import           BasePrelude hiding (TVar)
import           MTLPrelude

import           Control.Comonad.Cofree
import qualified Control.Comonad.Trans.Cofree as C
import           Data.Functor.Foldable
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import           Jael.Class
import           Jael.Types
import           Jael.Util

type TypeSub = M.Map T.Text Type

nullSub :: TypeSub
nullSub = M.empty

compSub :: TypeSub -> TypeSub -> TypeSub
l `compSub` r = (M.map (apply l) r) `M.union` l

instance TIOps TScheme where
  ftv (Scheme vs t) = ftv t S.\\ S.fromList vs
  apply s (Scheme vs t) = Scheme vs $ apply (foldr M.delete s vs) t

type HmEnv = M.Map T.Text TScheme

instance TIOps HmEnv where
  ftv = S.unions . map ftv. M.elems
  apply s = M.map (apply s)

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

--instantiate :: Scheme -> HmInfM Type
--instantiate (Scheme vs t) = do
--  vs' <- mapM (const freshTv) vs
--  return $ apply (M.fromList $ zip vs vs') t
doHm ([] C.:< EVarF n) = do
  env <- ask
  case M.lookup (value n) env of
    Nothing -> error $ "unbound variable " ++ show (value n)
    Just (Scheme vs t) -> do
      vs' <- mapM (const freshTv) vs
      let s = M.fromList $ zip vs vs'
      return $ (apply s t) :< ETIns s (t :< EVarF n)
      --t <- instantiate s
      --return $ t :< EVarF n

doHm ([] C.:< EIteF b t e) = do
  te1 <- b
  te2 <- t
  te3 <- e
  let (t1, t2, t3) = (getType te1, getType te2, getType te3)
  unify t1 $ TBuiltin BTBool
  unify t2 t3
  return $ t2 :< EIteF te1 te2 te3

--generalize :: HmEnv -> Type -> Scheme
--generalize env t =
--  let vs = ftv t S.\\ ftv env
--  in Scheme (S.toList vs) t
doHm ([] C.:< ELetF n e1 e2) = do
  env <- ask
  te1 <- e1
  let t1 = getType te1
  let fvs = S.toList $ ftv t1 S.\\ ftv env
  te2 <- inEnv (value n, Scheme fvs t1) e2
  let t2 = getType te2
  return $ t2 :< ELetF n (t1 :< ETGen fvs te1) te2

doHm ([] C.:< EAppF e1 e2) = do
  t <- freshTv
  te1 <- e1
  te2 <- e2
  let (t1, t2) = (getType te1, getType te2)
  unify t1 (TFun t2 t)
  return $ t :< EAppF te1 te2

doHm ([] C.:< EAbsF n@(Token v _) e) = do
  t <- freshTv
  te <- inEnv (v, Scheme [] t) e
  return $ (TFun t (getType te)) :< EAbsF n te

doHm ([] C.:< ETupF es) = do
  tes <- sequence es
  let ts = map getType tes
  tupTv <- freshTv
  let t = TTup ts
  unify tupTv t
  return $ t :< ETupF tes

doHm ([] C.:< EConF c) = do
  tv <- freshTv
  let t = case c of
            CUnit   -> TBuiltin BTUnit
            CBool _ -> TBuiltin BTBool
            CInt _  -> TBuiltin BTInt
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
            CBitCat -> TFun (TBuiltin BTBits)
                            (TBuiltin BTBits)
            CNot    -> bool2bool
  unify tv t
  return $ tv :< EConF c

doHm (_ C.:< (ETGen _ _)) = error "Should not be present prior to type checking"
doHm (_ C.:< (ETIns _ _)) = error "Should not be present prior to type checking"

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
unifier x y = doUnify (unfix x) (unfix y)
  where doUnify t1 t2 | t1 == t2 = return nullSub
        doUnify (TFunF l r) (TFunF l' r') = unifyMany [l, r] [l', r']
        doUnify (TVarF n) t = bind n t
        doUnify t (TVarF n) = bind n t
        doUnify (TTupF ts) (TTupF ts') = unifyMany ts ts'
        doUnify (TNamedF (Token n _) ts) (TNamedF (Token n' _) ts') =
          if n == n'
            then unifyMany ts ts'
            else error $ "can't match " ++ show n ++ " and " ++ show n'
        doUnify t t' = error $ "Non-unifying types " ++ show t ++ " and " ++ show t'

        bind v t
          | ((Token n _), (TVarF (Token n' _))) <- (v, t)
          , n == n'
          = return nullSub
          | S.member (value v) (ftv $ Fix t)
          = error "occurs check"
          | otherwise
          = return $ M.singleton (value v) (Fix t)

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
int2int2int = TFun (TBuiltin BTInt)
                   (TFun (TBuiltin BTInt)
                         (TBuiltin BTInt)
                    )

bool2bool :: Type
bool2bool = TFun (TBuiltin BTBool)
                 (TBuiltin BTBool)

bool2bool2bool :: Type
bool2bool2bool = TFun (TBuiltin BTBool)
                      (bool2bool)

freshTv :: HmInfM Type
freshTv = do
  c <- gets hmsCount
  modify (\_ -> HmInfS $ c + 1)
  return $ TVar $ Token (T.pack $ 'a' : show c) (0,0) -- Dummy location

