{-# Language FlexibleInstances #-}
{-# Language NoImplicitPrelude #-}
{-# Language TypeSynonymInstances #-}

module Jael.Infer where

import           BasePrelude
import           MTLPrelude

import           Control.Comonad.Cofree
import qualified Control.Comonad.Trans.Cofree as C
import           Data.Functor.Foldable
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import           Jael.Expr
import           Jael.TIOps
import           Jael.Type
import           Jael.Util
import           Jael.Util.Ann

data Scheme = Scheme [T.Text] Type

instance TIOps Scheme where
  ftv (Scheme vs t) = ftv t S.\\ S.fromList vs
  apply s (Scheme vs t) = Scheme vs $ apply (foldr M.delete s vs) t

type HmEnv = M.Map T.Text Scheme

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

hmInf :: HmEnv -> MaybeTypedExpr -> Either T.Text TypedExpr
hmInf env mTyped =
  let x = runIdentity $ runExceptT $ runRWST (cata doHm mTyped) env initState
  in case x of
       Left e -> Left e
       Right (typed, _, w) -> do
         s <- runIdentity $ runExceptT $ unificationSolver (nullSub, w)
         Right $ apply s typed

doHm :: C.CofreeF (ExprF (Ident, Maybe QType)) (Maybe QType) (HmInfM TypedExpr) -> HmInfM TypedExpr
doHm (Just t C.:< e) = do
  te <- doHm $ Nothing C.:< e
  unify (getType te) (removeAnn t)
  return $ setAnn te t

doHm (Nothing C.:< EVarF n) = do
  env <- ask
  case M.lookup (value n) env of
    Nothing -> error $ "unbound variable " ++ show (value n)
    Just s -> do
      t <- instantiate s
      return $ noQual t :< EVarF n

doHm (Nothing C.:< EIteF b t e) = do
  te1 <- b
  te2 <- t
  te3 <- e
  let (t1, t2, t3) = (getType te1, getType te2, getType te3)
  unify t1 (Fix $ TBuiltinF BTBool)
  unify t2 t3
  return $ noQual t2 :< EIteF te1 te2 te3

doHm (Nothing C.:< ELetF n e1 e2) = do
  env <- ask
  te1 <- e1
  let s = generalize env (getType te1)
  te2 <- inEnv (value n, s) e2
  return $ noQual (getType te2) :< ELetF n te1 te2

doHm (Nothing C.:< EAppF e1 e2) = do
  t <- freshTv
  te1 <- e1
  te2 <- e2
  let (t1, t2) = (getType te1, getType te2)
  unify t1 (Fix $ TFunF t2 t)
  return $ noQual t :< EAppF te1 te2

doHm (Nothing C.:< EAbsF (n@(Token v _), mt) e) = do
  t <- case mt of
         Just v'sType -> return $ removeAnn v'sType
         Nothing -> freshTv
  te <- inEnv (v, Scheme [] t) e
  return $ noQual (Fix $ TFunF t (getType te)) :< EAbsF n te

doHm (Nothing C.:< ETupF es) = do
  tes <- sequence es
  let ts = map getType tes
  tupTv <- freshTv
  let t = Fix $ TTupF ts
  unify tupTv t
  return $ noQual t :< ETupF tes

doHm (Nothing C.:< EConF c) = do
  tv <- freshTv
  let t = case c of
            CUnit   -> TBuiltinF BTUnit
            CBool _ -> TBuiltinF BTBool
            CInt _  -> TBuiltinF BTInt
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
            CBitCat -> TFunF (Fix $ TBuiltinF BTBits)
                             (Fix $ TBuiltinF BTBits)
            CNot    -> bool2bool
  unify tv (Fix t)
  return $ noQual tv :< EConF c

instantiate :: Scheme -> HmInfM Type
instantiate (Scheme vs t) = do
  vs' <- mapM (const freshTv) vs
  return $ apply (M.fromList $ zip vs vs') t

generalize :: HmEnv -> Type -> Scheme
generalize env t =
  let vs = ftv t S.\\ ftv env
  in Scheme (S.toList vs) t

type TypeSub = M.Map T.Text Type

nullSub :: TypeSub
nullSub = M.empty

compSub :: TypeSub -> TypeSub -> TypeSub
l `compSub` r = (M.map (apply l) r) `M.union` l

inEnv :: (T.Text, Scheme) -> HmInfM a -> HmInfM a
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

int2int2int :: TypeF Type
int2int2int = TFunF (Fix $ TBuiltinF BTInt)
                    (Fix $ TFunF (Fix $ TBuiltinF BTInt)
                                 (Fix $ TBuiltinF BTInt)
                    )

bool2bool :: TypeF Type
bool2bool = TFunF (Fix $ TBuiltinF BTBool)
                  (Fix $ TBuiltinF BTBool)

bool2bool2bool :: TypeF Type
bool2bool2bool = TFunF (Fix $ TBuiltinF BTBool)
                       (Fix $ bool2bool)

freshTv :: HmInfM Type
freshTv = do
  c <- gets hmsCount
  modify (\_ -> HmInfS $ c + 1)
  return $ Fix $ TVarF $ Token (T.pack $ 'a' : show c) (0,0) -- Dummy location

