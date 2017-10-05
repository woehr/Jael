{-# Language FlexibleInstances #-}
{-# Language TypeSynonymInstances #-}
{-# Language OverloadedStrings #-}

module Jael.New.HMInfer where

--import qualified Control.Comonad.Trans.Cofree as C
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

--import           Jael.New.Expr
import           Jael.New.Parser
import           Jael.New.Type

type TypeSub = M.Map T.Text Type
type Constraint = (Type, Type)
type Unifier = (TypeSub, [Constraint])
type SolveM = ExceptT T.Text Identity
type HmEnv = M.Map T.Text Type

nullSub :: TypeSub
nullSub = M.empty

compSub :: TypeSub -> TypeSub -> TypeSub
l `compSub` r = (M.map (apply l) r) `M.union` l

emptyEnv :: HmEnv
emptyEnv = M.empty

data HmInfS = HmInfS
  { hmsCount :: Integer }
  deriving (Eq, Show)

initState :: HmInfS
initState = HmInfS { hmsCount = 0 }

type HmInfM = RWST HmEnv [Constraint] HmInfS (Except T.Text)

data TypeErr

infer :: HmEnv -> E -> Either [TypeErr] E
infer env expr = undefined env expr
--  let x = runIdentity $ runExceptT $ runRWST (cata doHm mTyped) env initState
--  in case x of
--       Left e -> Left e
--       Right ((typed, is), _, w) -> do
--         s <- runIdentity $ runExceptT $ unificationSolver (nullSub, w)
--         Right $ fmap (typeToScheme is s) typed
--
--tvar :: Type -> T.Text
--tvar (TVar n) = n
--tvar _ = error "tvar not called on a type variable"
--
--doHm :: E -> HmInfM E
--doHm (ETAbsF _ _ _) = error "ETAbsF in expression before inference"
--doHm (ETAppF _ _ _) = error "ETAbsF in expression before inference"
--doHm (EVarF n) = do
--  env <- ask
--  case M.lookup (value n) env of
--    Nothing -> error $ "unbound variable " ++ show (value n)
--    Just (TAll tvs t) -> do
--      tvs' <- mapM (const freshTv) tvs
--      return ( apply (M.fromList $ zip tvs tvs') t :< EVarF n
--             , S.fromList $ map tvar tvs')
--
--doHm (EIfF b t e) = do
--  (te1, is1) <- b
--  (te2, is2) <- t
--  (te3, is3) <- e
--  let (t1, t2, t3) = (extract te1, extract te2, extract te3)
--  unify t1 TBool
--  unify t2 t3
--  return (t2 :< EIteF te1 te2 te3, S.unions [is1, is2, is3])
--
--doHm (ELetF n e1 e2) = do
--  env <- ask
--  (te1, is1) <- e1
--  let s = generalize env (extract te1)
--  (te2, is2) <- inEnv (value n, s) e2
--  let t2 = extract te2
--  return (t2 :< ELetF n te1 te2, is1 `S.union` is2)
--
--doHm (EAppF e1 e2) = do
--  tv <- freshTv
--  ((te1, is1), (te2, is2)) <- liftM2 (,) e1 e2
--  let (t1, t2) = (extract te1, extract te2)
--  unify t1 (TFun ("nil") t2 tv)
--  return (tv :< EAppF te1 te2, is1 `S.union` is2)
--
--doHm (EAbsF n@(Token v _) e) = do
--  tv <- freshTv
--  (te, is1) <- inEnv (v, Scheme S.empty M.empty tv) e
--  return (TFun n tv (extract te) :< EAbsF n te, is1)
--
--doHm (ETupF es) = do
--  (tes, is) <- liftM unzip $ sequence es
--  tupTv <- freshTv
--  let t = TTup $ map extract tes
--  unify tupTv t
--  return (t :< ETupF tes, S.unions is)
--
--doHm (EConF c) = do
--  tv <- freshTv
--  unify tv $ constantType c
--  return (tv :< EConF c, S.empty)
--
--inEnv :: (T.Text, Type) -> HmInfM a -> HmInfM a
--inEnv (n, s) m = do
--  let scope e = M.insert n s $ M.delete n e
--  local scope m
--
--unify :: Type -> Type -> HmInfM ()
--unify t1 t2 = tell [(t1, t2)]
--
--generalize :: HmEnv -> Type -> Type
--generalize env t =
--  let fvs = ftv t S.\\ ftv env
--  in  Scheme fvs M.empty t
--
--unificationSolver :: Unifier -> SolveM TypeSub
--unificationSolver (s, cs) =
--  case cs of
--    [] -> return s
--    ((t1, t2): cs') -> do
--      s' <- unifier t1 t2
--      unificationSolver (s' `compSub` s, apply s' cs')

unifier :: Type -> Type -> SolveM TypeSub
unifier t1 t2 | t1 == t2 = return nullSub
unifier (TFun l r) (TFun l' r') = unifyMany [l, r] [l', r']
unifier (TVar n) t = bind n t
unifier t (TVar n) = bind n t
unifier (TCon n ts) (TCon n' ts')
  | n == n' = unifyMany ts ts'
  | otherwise = error $ "can't match " ++ show n ++ " and " ++ show n'
unifier (TTup ts) (TTup ts') = unifyMany ts ts'
unifier (TArr t i) (TArr t' i')
  | i == i' = unifier t t'

unifier (TRec fs) (TRec fs') = undefined fs fs'
unifier t t' = error $ "Non-unifying types " ++ show t ++ " and " ++ show t'

bind :: T.Text -> Type -> SolveM TypeSub
bind v t
  | (n, (TVar n')) <- (v, t)
  , n == n'
  = return nullSub
  | S.member v (ftv t)
  = error "occurs check"
  | otherwise
  = return $ M.singleton v t

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
  return $ TVar $ (T.pack $ 'a' : show c)

class TIOps a where
  ftv :: a -> S.Set T.Text
  apply :: M.Map T.Text Type -> a -> a

instance TIOps Type where
  ftv = cata alg
    where alg (TVarF v)     = S.singleton v
          alg (TFunF t1 t2) = t1 `S.union` t2
          alg (TConF _ ts)  = S.unions ts
          alg (TTupF ts)    = S.unions ts
          alg (TRecF fs)    = S.unions $ map snd fs
          alg (TArrF t _)   = t
          alg (TAllF vs t)  = t S.\\ S.fromList vs

  apply s (TVar v)     = M.findWithDefault (TVar v) v s
  apply s (TFun t1 t2) = TFun (apply s t1) (apply s t2)
  apply s (TCon n ts)  = TCon n $ map (apply s) ts
  apply s (TTup ts)    = TTup $ map (apply s) ts
  apply s (TRec fs)    = TRec $ map (fmap $ apply s) fs
  apply s (TArr t i)   = TArr (apply s t) i
  apply s (TAll vs t)  = undefined s vs t
  apply _ _ = error "huh?"

instance TIOps (Type, Type) where
  ftv (t1, t2) = S.union (ftv t1) (ftv t2)
  apply s ts = join bimap (apply s) ts

instance TIOps a => TIOps [a] where
  ftv = S.unions . map ftv
  apply s = map (apply s)

instance TIOps (M.Map T.Text Type) where
  ftv = S.unions . map ftv. M.elems
  apply s = M.map (apply s)
