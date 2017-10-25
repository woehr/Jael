{-# Language FlexibleInstances #-}
{-# Language LambdaCase #-}
{-# Language TupleSections #-}
{-# Language TypeSynonymInstances #-}
{-# Language OverloadedStrings #-}

module Jael.New.HMInfer where

import qualified Control.Comonad.Trans.Cofree as C
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Text.Trifecta

import           Jael.New.Check
import           Jael.New.Expr
import           Jael.New.Parser
import           Jael.New.Type

type Bind = T.Text
type TypeSub = M.Map T.Text Type
type Constraint = (Type, Type)
type Unifier = (TypeSub, [Constraint])
type SolveM = ExceptT [TypeErr] Identity
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

type HmInfM = RWST HmEnv [Constraint] HmInfS (Except TypeErr)
type TypedE = Cofree (ExprF Type [P] T.Text) Span
type E' = Cofree (ExprF () [P] T.Text) Span

data TypeErr = TypeErr
  deriving (Show)

infer :: HmEnv -> E' -> Either [TypeErr] (Type, TypedE)
infer env expr =
  let r = runIdentity $ runExceptT $ runRWST (cata doHm expr) env initState
  in case r of
       Left e -> Left [e]
       Right ((t, te), _, w) -> do
         s <- runIdentity $ runExceptT $ unificationSolver (nullSub, w)
         Right (apply s t, te)

doHm :: C.CofreeF (ExprF () [P] T.Text) Span (HmInfM (Type, TypedE))
     -> HmInfM (Type, TypedE)

doHm (_ C.:< ETAbsF _ _) = error "ETAbsF in expression before inference"
doHm (_ C.:< ETAppF _ _) = error "ETAbsF in expression before inference"

doHm (sp C.:< EVarF n) = do
  env <- ask
  case M.lookup n env of
    Nothing ->
      error "unbound variable (previous checks should prevent)"
    Just (TAll tvs t) -> do
      tvs' <- forM tvs (const freshTv)
      return ( apply (M.fromList $ zip tvs tvs') t
             , sp :< EVarF n
             )
    Just t -> return (t, sp :< EVarF n)

doHm (sp C.:< EIfF b t e) = do
  (t1, te1) <- b
  (t2, te2) <- t
  (t3, te3) <- e
  unify t1 TBool
  unify t2 t3
  return (t2, sp :< EIfF te1 te2 te3)

doHm (sp C.:< ELetF xs e) = do
  es <- sequence $ map snd xs
  let xs' = zipWith (\ps (t, te) -> (ps, t, te)) (map fst xs) es
  -- unifyPatterns :: [(Type, [P])] -> [(Bind, Type)] -> HmInfM ()
  undefined

  {-
  let sch = generalize t1
  (t2, te2) <- inEnv (n, sch) e2
  return (t2, sp :< ELetF n te1 te2)
  -}

doHm (sp C.:< EAppF e as) = do
  tv <- freshTv
  (t1, te) <- e
  as' <- sequence as
  let t2 = foldr (TFun . fst) tv as'
  unify t1 t2
  return (tv, sp :< EAppF te (map snd as'))

doHm (sp C.:< EAbsF pss [] e) = do
  lamTvs <- mapM (const freshTv) pss

  let binds = map fst $ concatMap patternBinds (concat pss)
  bindTvs <- mapM (const freshTv) binds

  let bts = zip binds bindTvs
  (t, te) <- inEnv' bts e

  -- Need to check patterns and unify bind type variables with the types
  -- expected in patterns. Also, unify top level bind type variables with
  -- lambda type variables
  unifyPatterns (zip lamTvs pss) (zip binds bindTvs)

  return (foldr TFun t lamTvs, sp :< EAbsF pss bts te)

doHm (_ C.:< EAbsF _ (_:_) _) =
  error "Expected no typed binds before inference."

doHm (sp C.:< ETupF es) = do
  es' <- sequence es
  tv <- freshTv
  let t = TTup $ map fst es'
  unify tv t
  return (t, sp :< ETupF (map snd es'))

doHm (sp C.:< EConstF c) = return $
  case c of
    CInt  _ -> (TCon "Int" [], sp :< EConstF c)
    CChar _ -> (TCon "Int" [], sp :< EConstF c)

doHm (sp C.:< EUnaryOpF op) = return . (, sp :< EUnaryOpF op) $
  case op of
    OpNot -> TCon "TODO" []

doHm (sp C.:< EBinOpF op) = return . (, sp :< EBinOpF op) $
  case op of
    OpAdd -> TFun (TCon "Int" []) (TFun (TCon "Int" []) (TCon "Int" []))

-- All binds in the patterns of argTvs are keys in bindTvs
unifyPatterns :: [(Type, [P])] -> [(Bind, Type)] -> HmInfM ()
unifyPatterns argTvs bindTvs =
  let bindTvMap = M.fromList bindTvs

      patternType :: C.CofreeF PatternF Span (HmInfM Type) -> HmInfM Type
      patternType (_ C.:< PPatF _n _xs) = error "TODO"
      patternType (_ C.:< PTupF xs)     = liftM TTup $ sequence xs
      patternType (_ C.:< POrF _xs)     = error "TODO"
      patternType (_ C.:< PRecF _fs)    = error "TODO"
      patternType (_ C.:< PArrF _xs)    = error "TODO"
      patternType (_ C.:< PConstF _c)   = error "TODO"
      patternType (_ C.:< PWildF)       = freshTv
      patternType (_ C.:< PMultiWildF)  = freshTv
      patternType (_ C.:< PBindF n Nothing) = return $ unsafeLookup n bindTvMap
      patternType (_ C.:< PBindF n (Just p)) =
        let nTy = unsafeLookup n bindTvMap
         in p >>= unify nTy >> return nTy

   in forM_ argTvs $ \(tv, ps) -> forM_ ps $ \case
    _ :< PBindF n Nothing  -> unify tv (unsafeLookup n bindTvMap)
    _ :< PBindF n (Just p) -> unify tv (unsafeLookup n bindTvMap) >>
                              cata patternType p >>= unify tv
    p -> cata patternType p >>= unify tv

inEnv :: (T.Text, Type) -> HmInfM a -> HmInfM a
inEnv (n, s) m = do
  let errFn _new _old = error $ "Shadowed variable"
  let scope e = M.insertWith errFn n s e
  local scope m

inEnv' :: [(T.Text, Type)] -> HmInfM a -> HmInfM a
inEnv' bs m = foldr inEnv m bs

unify :: Type -> Type -> HmInfM ()
unify t1 t2 = tell [(t1, t2)]

generalize :: Type -> HmInfM Type
generalize t = do
  env <- ask
  return $ TAll (S.toList $ ftv t S.\\ ftv env) t

unificationSolver :: Unifier -> SolveM TypeSub
unificationSolver (s, cs) =
  case cs of
    [] -> return s
    ((t1, t2): cs') -> do
      s' <- unifier t1 t2
      unificationSolver (s' `compSub` s, apply s' cs')

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

unifier (TRec _fs) (TRec _fs') = undefined
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
  return $ TVar $ T.pack $ 'a' : show c

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
