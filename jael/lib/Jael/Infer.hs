{-# Language FlexibleInstances #-}
{-# Language NoImplicitPrelude #-}
{-# Language TypeSynonymInstances #-}
{-# Language OverloadedStrings #-}

module Jael.Infer where

import           Jael.Prelude
import qualified Control.Comonad.Trans.Cofree as C
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import qualified Jael.Constants as JC
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

typeToScheme :: S.Set T.Text -> M.Map T.Text Type -> Type -> TScheme
typeToScheme is su t =
  let su' = foldr M.delete su (S.toList is)
      t'  = apply su' t
      fvs = ftv t' S.\\ is
      is' = S.toList $ is `S.intersection` ftv t'
      ins = M.fromList (zip is' $ map (\i -> TVar $ Token i dummyLoc) is')
  in  Scheme fvs (M.map (apply su) ins) t'

hmInf :: HmEnv -> MaybeTypedExpr -> Either T.Text HMTypedExpr
hmInf env mTyped =
  let x = runIdentity $ runExceptT $ runRWST (cata doHm mTyped) env initState
  in case x of
       Left e -> Left e
       Right ((typed, is), _, w) -> do
         s <- runIdentity $ runExceptT $ unificationSolver (nullSub, w)
         Right $ fmap (typeToScheme is s) typed

tvar :: Type -> T.Text
tvar (TVar (Token n _)) = n
tvar _ = error "tvar not called on a type variable"

doHm :: C.CofreeF ExprF [QType] (HmInfM (Cofree ExprF Type, S.Set T.Text))
                              -> HmInfM (Cofree ExprF Type, S.Set T.Text)

doHm ([] C.:< EVarF n) = do
  env <- ask
  case M.lookup (value n) env of
    Nothing -> error $ "unbound variable " ++ show (value n)
    Just (Scheme gen ins t) -> do
      assert (ins == M.empty) return ()
      let tvs = S.toList gen
      tvs' <- mapM (const freshTv) tvs
      return ( apply (M.fromList $ zip tvs tvs') t :< EVarF n
             , S.fromList $ map tvar tvs')

doHm ([] C.:< EIteF b t e) = do
  (te1, is1) <- b
  (te2, is2) <- t
  (te3, is3) <- e
  let (t1, t2, t3) = (extract te1, extract te2, extract te3)
  unify t1 $ TBool
  unify t2 t3
  return (t2 :< EIteF te1 te2 te3, S.unions [is1, is2, is3])

doHm ([] C.:< ELetF n e1 e2) = do
  env <- ask
  (te1, is1) <- e1
  let s = generalize env (extract te1)
  (te2, is2) <- inEnv (value n, s) e2
  let t2 = extract te2
  return (t2 :< ELetF n te1 te2, is1 `S.union` is2)

doHm ([] C.:< EAppF e1 e2) = do
  tv <- freshTv
  ((te1, is1), (te2, is2)) <- liftM2 (,) e1 e2
  let (t1, t2) = (extract te1, extract te2)
  unify t1 (TFun ("nil") t2 tv)
  return (tv :< EAppF te1 te2, is1 `S.union` is2)

doHm ([] C.:< EAbsF n@(Token v _) e) = do
  tv <- freshTv
  (te, is1) <- inEnv (v, Scheme S.empty M.empty tv) e
  return (TFun n tv (extract te) :< EAbsF n te, is1)

doHm ([] C.:< ETupF es) = do
  (tes, is) <- liftM unzip $ sequence es
  tupTv <- freshTv
  let t = TTup $ map extract tes
  unify tupTv t
  return (t :< ETupF tes, S.unions is)

doHm ([] C.:< EConF c) = do
  tv <- freshTv
  let t = case c of
            CUnit   -> TUnit
            CBool _ -> TBool
            CInt _  -> TInt
            CAdd    -> removeAnn (schType JC.add)
            CSub    -> removeAnn undefined
            CMul    -> removeAnn undefined
            CDiv    -> removeAnn undefined
            CMod    -> removeAnn undefined
            COr     -> removeAnn undefined
            CAnd    -> removeAnn undefined
            CEq     -> removeAnn undefined
            CNe     -> removeAnn undefined
            CGe     -> removeAnn undefined
            CLe     -> removeAnn undefined
            CGt     -> removeAnn undefined
            CLt     -> removeAnn undefined
            CBitCat -> removeAnn undefined
            CNot    -> removeAnn undefined
  unify tv t
  return (tv :< EConF c, S.empty)

doHm (ts C.:< e) = do
  (te, is) <- doHm $ [] C.:< e
  let t = extract te
  mapM_ (unify t . removeAnn) ts
  return (te, is)

inEnv :: (T.Text, TScheme) -> HmInfM a -> HmInfM a
inEnv (n, s) m = do
  let scope e = M.insert n s $ M.delete n e
  local scope m

unify :: Type -> Type -> HmInfM ()
unify t1 t2 = tell [(t1, t2)]

generalize :: HmEnv -> Type -> TScheme
generalize env t =
  let fvs = ftv t S.\\ ftv env
  in  Scheme fvs M.empty t

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
--unifier (TIns s t1) t2 = unifier (apply (M.fromList s) t1) t2
--unifier t1 (TIns s t2) = unifier t1 (apply (M.fromList s) t2)
--unifier (TGen _ t1) t2 = unifier t1 t2
--unifier t1 (TGen _ t2) = unifier t1 t2
unifier (TFun _ l r) (TFun _ l' r') = unifyMany [l, r] [l', r']
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

freshTv :: HmInfM Type
freshTv = do
  c <- gets hmsCount
  modify (\_ -> HmInfS $ c + 1)
  return $ TVar $ Token (T.pack $ 'a' : show c) dummyLoc
