{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language LambdaCase #-}
{-# Language RecordWildCards #-}
{-# Language TupleSections #-}
{-# Language TypeSynonymInstances #-}
{-# Language OverloadedStrings #-}

module Jael.New.HMInfer where

import qualified Control.Comonad.Trans.Cofree as C
import qualified Data.DList as DL
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Jael.Data.MultiMap as MM
import qualified Jael.New.Constants as JC

import Text.Trifecta

import Jael.New.Check
import Jael.New.Expr
import Jael.New.Parser
import Jael.New.Type

type TypeSub = M.Map T.Text Type
type RowSub = M.Map T.Text Row
type RowVar = T.Text
type Constraint = (Type, Type)
type Unifier = ((TypeSub, RowSub), [Constraint])
type SolveM = StateT SolveS (WriterT (DL.DList TypeErr) Identity)
type HmEnv = M.Map T.Text Type

data SolveS = SolveS { ssNextTv :: Integer, ssNextRowTv :: Integer }
  deriving (Eq, Show)

initSolveSt :: SolveS
initSolveSt = SolveS { ssNextTv = 0, ssNextRowTv = 0 }

class Substitutable a where
  applySub :: M.Map T.Text a -> a -> a

  compSub :: M.Map T.Text a -> M.Map T.Text a -> M.Map T.Text a
  l `compSub` r = fmap (applySub l) r `M.union` l

  nullSub :: M.Map T.Text a
  nullSub = M.empty

instance Substitutable Type where
  applySub = apply

instance Substitutable Row where
  applySub = rapply

compSubs :: (TypeSub, RowSub) -> (TypeSub, RowSub) -> (TypeSub, RowSub)
compSubs (s1, r1) (s2, r2) = (s1 `compSub` s2, r1 `compSub` r2)

subBoth :: (TIOps a, RowOps a) => (TypeSub, RowSub) -> a -> a
subBoth (tsub, rsub) = rapply rsub . apply tsub

newtype HmInfW
  = HMWError TypeErr
  deriving (Eq, Show)

data HmInfS = HmInfS
  { hmsCount :: Integer
  , hmsRowCount :: Integer
  , hmsConstraints :: [Constraint]
  } deriving (Eq, Show)

initState :: HmInfS
initState = HmInfS
  { hmsCount = 0
  , hmsRowCount = 0
  , hmsConstraints = []
  }

type HmInfM = RWST HmEnv (DL.DList HmInfW) HmInfS (Except [TypeErr])
type E' = Cofree (ExprF () [P] T.Text) Span
type TypedP t = Cofree (PatternF (T.Text, t)) Span
type TypedE t = Cofree (ExprF t [TypedP t] T.Text) Span
type TypedPat  = TypedP Type
type TypedExpr = TypedE Type
--TypedExpr == Cofree (ExprF Type [Cofree (PatternF (T.Text, Type)) Span] T.Text) Span

instance TIOps TypedExpr where
  apply s (sp :< ETAbsF vs e) =
    let subTVs :: Type -> S.Set T.Text -> S.Set T.Text
        subTVs t acc = ftv t `S.union` acc
        vs' = S.toList $ foldr (subTVs . apply s . TVar) S.empty vs
     in if null vs'
           then apply s e
           else sp :< ETAbsF vs' (apply s e)

  apply s (sp :< e) =
    let applyToPat :: TypedPat -> TypedPat
        applyToPat = hoistCofree (mapPatB $ second (apply s))
     in sp :< mapExprP (map applyToPat) (mapExprT (apply s) $ fmap (apply s) e)

  ftv = cata alg where
    alg (_ C.:< ETAbsF vs e)  = e S.\\ S.fromList vs
    alg (_ C.:< ETAppF ts e)  = ftv ts `S.union` e
    alg (_ C.:< EAbsF pt's e) = S.unions $ e : map (ftv . snd) pt's
    alg (_ C.:< ELamCaseF t pe's) = S.unions $ ftv t : map snd pe's
    alg (_ C.:< ELetF pte's e)    = S.unions $ e : map snd pte's
    alg (_ C.:< e) = foldr S.union S.empty e

instance RowOps TypedExpr where
  rapply s (sp :< ETAbsF vs e) =
    let subTVs :: Row -> S.Set T.Text -> S.Set T.Text
        subTVs t acc = rftv t `S.union` acc
        vs' = S.toList $ foldr (subTVs . rapply s . Row [] . Just) S.empty vs
     in if null vs'
           then rapply s e
           else sp :< ETAbsF vs' (rapply s e)

  rapply s (sp :< e) =
    let applyToPat :: TypedPat -> TypedPat
        applyToPat = hoistCofree (mapPatB $ second (rapply s))
     in sp :< mapExprP (map applyToPat) (mapExprT (rapply s) $ fmap (rapply s) e)

  rftv = cata alg where
    alg (_ C.:< ETAbsF vs e)  = e S.\\ S.fromList vs
    alg (_ C.:< ETAppF ts e)  = rftv ts `S.union` e
    alg (_ C.:< EAbsF pt's e) = S.unions $ e : map (rftv . snd) pt's
    alg (_ C.:< ELamCaseF t pe's) = S.unions $ rftv t : map snd pe's
    alg (_ C.:< ELetF pte's e)    = S.unions $ e : map snd pte's
    alg (_ C.:< e) = foldr S.union S.empty e

data TypeErr
  = TEOccurs T.Text Type
  | TERowOccurs T.Text Row
  | TEUnification Type Type
  | TERowUnification Row Row
  | TEPatternType P Type
  | TEPatternLength T.Text Integer
  | TEUnknownDataConstructor T.Text
  deriving (Eq, Show)

exprType :: M.Map T.Text Type -> TypedExpr -> Type
exprType m = generalize' M.empty . go m . removeAnn where
  go :: M.Map T.Text Type -> Fix (ExprF Type [TypedP Type] T.Text) -> Type
  go env (ETAbs vs e) = TAll vs (go env e)
  go env (ETApp ts e)
    | TAll vs t <- go env e
    = apply (M.fromList $ zip vs ts) t

  go env (EAbs pt's e) =
    foldr (TFun Nothing . snd)
          (go (foldr (envWithPats . fst) env pt's) e)
          pt's

  go env (ELamCase t ((p,e):_)) = TFun Nothing t (go (envWithPats p env) e)

  go env (EApp e _)      = returnType (go env e)
  go env (ETup es)       = TTup $ map (go env) es
  go env (EArr es@(e:_)) = TArr (go env e) (toInteger $ length es)

  go env (ELet pte's e)    = go (foldr (envWithPats . fst . fst) env pte's) e

  go _ ERec = TRec $ Row [] Nothing

  go env (ERecUpdate l e r)
    | Just r' <- rowUpdate' l (go env e) (unfix $ go env r)
    = Fix r'

  go env (ERecExtend l  e r)
    | Just r' <- rowExtend' l (go env e) (unfix $ go env r)
    = Fix r'

  go env (ERecRename l' l r)
    | Just r' <- rowRename' l' l (unfix $ go env r)
    = Fix r'

  go env (ERecRemove r l)
    | Just r' <- rowRemove' (unfix $ go env r) l
    = Fix r'

  go env (ERecSelect r l)
    | Just t <- rowSelect' (unfix $ go env r) l
    = t

  go env (ECase _ ((p,e):_)) = go (envWithPats p env) e
  go env (EIf _ _ e)         = go env e
  go env (EMultiIf (Guarded _ e : _) _) = go env e

  go env (EVar n)            = unsafeLookup n env
  go _   (EConst c)          = JC.constType c

  -- "If type inference was successful, this shouldn't happen."
  go env x = error $ show x ++ "\n\n" ++ show env

  envWithPats :: [TypedP Type] -> M.Map T.Text Type -> M.Map T.Text Type
  envWithPats ps env = foldr (uncurry M.insert . fst) env binds
    where binds = concatMap patternBinds ps

-- Partial function: All bound variables in the provided patterns must have
-- a key in the map.
mapSubPatBinds :: [(T.Text, Type)] -> [P] -> [TypedP Type]
mapSubPatBinds bt's = map (hoistCofree subPatternBind)
  where
    sub :: M.Map T.Text (T.Text, Type)
    sub = M.mapWithKey (,) $ M.fromList bt's

    subPatternBind :: PatternF T.Text p -> PatternF (T.Text, Type) p
    subPatternBind = mapPatB $ flip unsafeLookup sub

infer :: HmEnv -> E' -> Either [TypeErr] TypedExpr
infer env expr
  | Left e <- runHm = Left e
  | Right ((_, te@(sp :< _)), hmState, hmWriter) <- runHm
  = case DL.toList hmWriter of
      [] -> case runUnify (hmsConstraints hmState) of
        ((s, r), []) -> Right $ let te' = subBoth (s, r) te
                                    fvs = S.toList $ ftv te'
                                 in if null fvs
                                       then te'
                                       else sp :< ETAbsF fvs te'
        (_, es) -> Left es
      es  -> Left $ map (\(HMWError e) -> e) es
  where
    runHm = runIdentity $ runExceptT $ runRWST (cata doHm expr) env initState

runUnify :: [Constraint] -> ((TypeSub, RowSub), [TypeErr])
runUnify =
    first fst
  . second DL.toList
  . runIdentity . runWriterT . flip runStateT initSolveSt
  . unificationSolver . ((nullSub, nullSub), )

doHm :: C.CofreeF (ExprF () [P] T.Text) Span (HmInfM (Type, TypedExpr))
     -> HmInfM (Type, TypedExpr)

doHm (_ C.:< ETAbsF _ _) = error "ETAbsF in expression before inference"
doHm (_ C.:< ETAppF _ _) = error "ETAbsF in expression before inference"

doHm (sp C.:< EAbsF tps's e) = do
  let (pss, _) = unzip tps's
  lamTvs <- mapM (const freshTv) pss

  -- Need to check patterns and unify bind type variables with the types
  -- expected in patterns. Also, unify top level bind type variables with
  -- lambda type variables
  bts <- unifyPatterns (zip lamTvs pss)
  let pss' = map (mapSubPatBinds bts) pss

  (t, te) <- inEnv' bts e
  return (foldr (TFun Nothing) t lamTvs, sp :< EAbsF (zip pss' lamTvs) te)

doHm (sp C.:< ELamCaseF () pe's) = do
  let (pss, es) = unzip pe's
  tv <- freshTv

  -- The binds for each case
  bts's <- mapM (unifyPatterns . (\b -> [(tv, b)])) pss
  let pss' = zipWith mapSubPatBinds bts's pss
  -- The expressions evaluated in the context of the binds
  -- Not sure why the compiler doesn't complain about not handling the empty
  -- list case, but in any case, the parser should make sure pe's is
  -- non-empty and therefore (t:ts) will always match.
  (t:ts, tes) <- unzip <$> forM (zip bts's es) (uncurry inEnv')

  -- All expression types must unify
  mapM_ (unify t) ts

  return (TFun Nothing tv t, sp :< ELamCaseF tv (zip pss' tes))

doHm (sp C.:< EAppF e as) = do
  tv <- freshTv
  (t1, te) <- e
  as' <- sequence as
  let t2 = foldr (TFun Nothing . fst) tv as'
  unify t1 t2
  return (tv, sp :< EAppF te (map snd as'))

doHm (sp C.:< ETupF es) = do
  es' <- sequence es
  let t = TTup $ map fst es'
  return (t, sp :< ETupF (map snd es'))

doHm (sp C.:< EArrF es) = do
  es' <- sequence es
  t <- case es' of
         [] -> flip TArr 0 <$> freshTv
         (t,_):xs -> do
           forM_ xs (unify t . fst)
           return (TArr t . toInteger . length $ es')
  return (t, sp :< EArrF (map snd es'))

doHm (sp C.:< ELetF xs e) = do
  (t, xs', te) <- foldr f ((\(t,te) -> (t,[],te)) <$> e) xs
  return (t, sp :< ELetF xs' te)
  where
    f :: (([P], ()), HmInfM (Type, TypedExpr))
      -> HmInfM (Type, [(([TypedP Type], Type), TypedExpr)], TypedExpr)
      -> HmInfM (Type, [(([TypedP Type], Type), TypedExpr)], TypedExpr)
    -- let x = e1 in e2
    f ((ps, ()), mtte) macc = do
      (xt, e1@(e1sp :< _)) <- mtte

      -- Get generalized types for the binds in the pattern
      bts' <- unifyPatterns [(xt, ps)]
      unifyResult <- runUnify <$> gets hmsConstraints
      s <- case unifyResult of
             (s, []) -> return s
             (_, es) -> throwError es
      bts'' <- local (subBoth s) $ mapM (mapM $ generalize . subBoth s) bts'

      -- Determine the generalized type of x, substitute pattern binds with
      -- types, and generalize the expression bound to x
      xt' <- local (subBoth s) (generalize $ subBoth s xt)
      let ps' = mapSubPatBinds bts'' ps
      let e1' = case xt' of
                  TAll vs _ -> e1sp :< ETAbsF vs e1
                  _         -> e1

      (t', xs', te') <- inEnv' bts'' $ local (subBoth s) macc

      return (t', ((ps', xt'), e1'):xs', te')

doHm (sp C.:< ERecF) = return (TRec $ Row [] Nothing, sp :< ERecF)

doHm (sp C.:< ERecExtendF l e rec) = do
  (rt, rte) <- rec
  (t, te) <- e
  rtv <- freshRowTv
  unify rt (TRec $ Row [] $ Just rtv)
  return (TRec $ Row [(l, t)] $ Just rtv, sp :< ERecExtendF l te rte)

doHm (sp C.:< ERecUpdateF l e rec) = do
  (rt, rte) <- rec
  (t, te) <- e
  rtv <- freshRowTv
  tv <- freshTv
  unify rt (TRec $ Row [(l, tv)] $ Just rtv)
  return (TRec $ Row [(l, t)] $ Just rtv, sp :< ERecUpdateF l te rte)

doHm (sp C.:< ERecRenameF to from rec) = do
  (rt, rte) <- rec
  rtv <- freshRowTv
  tv <- freshTv
  unify rt (TRec $ Row [(from, tv)] $ Just rtv)
  return (TRec $ Row [(to, tv)] $ Just rtv, sp :< ERecRenameF to from rte)

doHm (sp C.:< ERecRemoveF rec l) = do
  (rt, rte) <- rec
  tv <- freshTv
  rtv <- freshRowTv
  unify rt (TRec $ Row [(l, tv)] $ Just rtv)
  return (TRec $ Row [] (Just rtv), sp :< ERecRemoveF rte l)

doHm (sp C.:< ERecSelectF rec l) = do
  (rt, rte) <- rec
  tv <- freshTv
  rtv <- freshRowTv
  unify rt (TRec $ Row [(l, tv)] $ Just rtv)
  return (tv, sp :< ERecSelectF rte l)

doHm (_ C.:< ECaseF _ [])
  = error "The parser should make this impossible"

doHm (sp C.:< ECaseF e (pe:pe's)) = do
  (t, te) <- e
  let (headPats, headExpr) = pe
  let (tailPats's, tailExprs) = unzip pe's

  -- headExpr and tailExprs must be evaluated in a context with pattern binds
  -- and the patterns must unify with the type of e
  head'bts <- unifyPatterns [(t, headPats)]
  (headType, headTypedExpr) <- inEnv' head'bts headExpr

  tail'bts's <- mapM (unifyPatterns . (:[]) . (t,)) tailPats's
  let tailExprsInEnv = zipWith inEnv' tail'bts's tailExprs
  (tailTypes, tailTypedExprs) <- unzip <$> sequence tailExprsInEnv

  -- All cases must unify to the same type
  forM_ tailTypes $ unify headType

  let headPats' = mapSubPatBinds head'bts headPats
  let tailPats's' = zipWith mapSubPatBinds tail'bts's tailPats's

  return ( headType
         , sp :< ECaseF te (    (headPats',  headTypedExpr)
                           : zip tailPats's' tailTypedExprs
                           )
         )

doHm (sp C.:< EIfF b t e) = do
  (t1, te1) <- b
  (t2, te2) <- t
  (t3, te3) <- e
  unify t1 TBool
  unify t2 t3
  return (t2, sp :< EIfF te1 te2 te3)

doHm (_ C.:< EMultiIfF [] _) = error "Parser shouldn't allow this case"

doHm (sp C.:< EMultiIfF (Guarded g e : gs) me) = do
  g' <- g
  e' <- e
  (gs', es') <- unzip <$> forM gs (\(Guarded h f) -> (,) <$> h <*> f)

  -- All guards must be bools
  forM_ (g':gs') $ unify TBool . fst
  -- All expressions must unify
  forM_ es' $ unify (fst e') . fst

  -- If there's a default, it must also unify
  me' <- sequence me
  forM_ me' $ unify (fst e') . fst

  return (fst e', sp :< EMultiIfF (zipWith Guarded (map snd (g':gs')) (map snd (e':es'))) (snd <$> me'))

doHm (sp C.:< EVarF n) = do
  env <- ask
  case M.lookup n env of
    Nothing ->
      error "unbound variable (previous checks should prevent)"
    Just t -> do
      (t', insts) <- instantiate t
      let e = if not (null insts)
                 then sp :< ETAppF insts (sp :< EVarF n)
                 else sp :< EVarF n
      return (t', e)

doHm (sp C.:< EConstF c) = return . (, sp :< EConstF c) . JC.constType $ c

patternTypes :: P -> HmInfM (Type, [(Bind, Type)])
patternTypes = cata alg where
  alg :: C.CofreeF (PatternF T.Text) Span (HmInfM (Type, [(Bind, Type)]))
                                        -> HmInfM (Type, [(Bind, Type)])
  alg (_ C.:< PPatF n xs) = do
    mt <- M.lookup n <$> ask
    case mt of
      Just conTy -> do
        conTy' <- fst <$> instantiate conTy

        when (arity conTy' /= toInteger (length xs)) $
          throwError [TEPatternLength n . toInteger . length $ xs]

        (ts, bs) <- unzip <$> sequence xs
        mapM_ (uncurry unify) $ zip ts (argTypes conTy')
        return (returnType conTy', concat bs)

      Nothing -> throwError [TEUnknownDataConstructor n]

  alg (_ C.:< PTupF xs) = sequence xs >>= \xs' ->
    return (TTup (fst <$> xs'), concatMap snd xs')

  alg (_ C.:< POrF _) = error "Should not happen since patterns are expanded"
  alg (_ C.:< PRecF fs rtail) = do
    (ls, (ts, btss)) <- second unzip . unzip <$> mapM (\(l,m) -> (l,) <$> m) fs
    let bts = concat btss :: [(Bind, Type)]
        lts = zip ls ts   :: [(Label, Type)]
    case rtail of
      TailBind r -> freshRowTv >>= \tv ->
                  let t  = TRec $ Row lts (Just tv)
                      rt = TRec $ Row []  (Just tv)
                   in return (t, (r, rt) : bts)
      TailWild -> (\tv -> (TRec $ Row lts (Just tv), bts)) <$> freshRowTv
      TailEmpt -> return (TRec $ Row lts Nothing, bts)

  alg (_ C.:< PArrF xs) = sequence xs >>= \case
    [] -> freshTv >>= \tv -> return (TArr tv 0, [])
    xs'@((t,_):_) -> do
      mapM_ (unify t . fst) xs'
      return (TArr t $ toInteger (length xs'), concatMap snd xs')

  alg (_ C.:< PConstF (CInt _)) = return (TInt, [])
  alg (_ C.:< PConstF _) = error "Should not happen since parser doesn't \
                          \parse other constants (like operators) in patterns"
  alg (_ C.:< PWildF) = (,[]) <$> freshTv
  alg (_ C.:< PBindF n Nothing) = freshTv >>= \tv -> return (tv, [(n, tv)])
  alg (_ C.:< PBindF n (Just p)) = p >>= \(t, bs) -> return (t, (n,t):bs)

unifyPatterns :: [(Type, [P])] -> HmInfM [(Bind, Type)]
unifyPatterns tps = do
  bindMMap <- forM tps $ \(t, ps) -> flip (`foldrM` MM.empty) ps $
    \p m -> do
      (patTy, bindTys) <- patternTypes p
      unify t patTy
      return $ foldr (uncurry MM.insert) m bindTys
  unifyDups . MM.unionsWith (++) $ bindMMap

unifyDups :: MM.MultiMap Bind Type -> HmInfM [(Bind, Type)]
unifyDups bs = let
  unifyList1 :: [Type] -> HmInfM Type
  unifyList1 (t:ts) = mapM_ (unify t) ts >> return t
  unifyList1 _ = error "Unexpected empty list."

  -- MultiMaps never have keys with no values
   in forM (MM.assocs bs) $ \(b,ts) -> (b,) <$> unifyList1 ts

inEnv :: (T.Text, Type) -> HmInfM a -> HmInfM a
inEnv (n, s) m = do
  let errFn _new _old =
        error "Shadowed variable. Previous checks should prevent."
  let scope = M.insertWith errFn n s
  local scope m

inEnv' :: [(T.Text, Type)] -> HmInfM a -> HmInfM a
inEnv' bs m = foldr inEnv m bs

unify :: Type -> Type -> HmInfM ()
unify t1 t2 =
  tellC (t1, t2)

generalize :: Type -> HmInfM Type
generalize t = (`generalize'` t) <$> ask

instantiate :: Type -> HmInfM (Type, [Type])
instantiate (TAll vs t) = do
  sub <- forM vs $ \v -> (v,) <$> freshTv
  return (apply (M.fromList sub) t, map snd sub)
instantiate t = return (t, [])

unificationSolver :: Unifier -> SolveM (TypeSub, RowSub)
unificationSolver (s, []) = return s
-- Prior to unification records' row types may be in arbitrary order, so we do
-- a sort first. During unification further sorts are unnecessary because
-- substitutions maintain the fields ordering (see rapply).
unificationSolver (s, (t1, t2):cs) = unifier (sortRecords t1) (sortRecords t2)
  >>= (\s' -> unificationSolver (s' `compSubs` s, subBoth s' cs))

unifier :: Type -> Type -> SolveM (TypeSub, RowSub)
unifier t1 t2 | t1 == t2 = return (nullSub, nullSub)
unifier (TFun _ l r) (TFun _ l' r') = unifyMany [l, r] [l', r']

unifier (TVar n) t = (, nullSub) <$> bind n t
unifier t (TVar n) = (, nullSub) <$> bind n t

unifier (TCon n ts) (TCon n' ts')
  | n == n' = unifyMany ts ts'
unifier (TTup ts) (TTup ts')
  = unifyMany ts ts'
unifier (TArr t i) (TArr t' i')
  | i == i' = unifier t t'

unifier (TRec r) (TRec r') = rowUnifier r r'

unifier (TAll _ _) _ = error "Unexpected"
unifier _ (TAll _ _) = error "Unexpected"

unifier t t' = unificationError (TEUnification t t') >> return (nullSub, nullSub)

unifyMany :: [Type] -> [Type] -> SolveM (TypeSub, RowSub)
unifyMany [] [] = return (nullSub, nullSub)
unifyMany (t1:t1s) (t2:t2s) = do
  s1 <- unifier t1 t2
  s2 <- unifyMany (subBoth s1 t1s)
                  (subBoth s1 t2s)
  return $ s2 `compSubs` s1

unifyMany _ _ = error "bleh"

rowUnifier :: Row -> Row -> SolveM (TypeSub, RowSub)

-- rules: uni-varl and uni-varr
-- Just a row type variable on either side, same as bind for things of kind *
rowUnifier (Row [] (Just v)) r = rowBind v r
rowUnifier r (Row [] (Just v)) = rowBind v r

-- rule: uni-row
rowUnifier (Row ((l, t):fs) mv) (Row ((l', t'):fs') mv')
  | l == l'
  = do
      -- s1 is the null substitution since no rewrite is necessary
      s2 <- unifier t t'
      s3 <- rowUnifier (subBoth s2 $ Row fs mv) (subBoth s2 $ Row fs' mv')
      return (s3 `compSubs` s2)

-- Consider the following sets of labels for the left row and right row,
-- respectively
-- 1) a, b, c    b, c
-- 2) b, c       a, b, c
-- In the first case we need to rewrite the right side to have an a,
-- but in the second case we need to rewrite the left side to have the a.
-- Also note that whichever side is rewritten must have a type variable
-- present (it's where the new label "comes" from).

-- Is l not in sFields?
-- Rewrite right side to have l
rowUnifier (Row ((l, tau):rFields) rVar) (Row sFields msVar)
  | Just sVar <- msVar
  , not $ l `S.member` S.fromList (map fst sFields)
  = rowUni (l, tau, rFields, rVar) (sFields, sVar)

-- Identical to previous case except the left and right patterns are swapped
rowUnifier (Row sFields msVar) (Row ((l, tau):rFields) rVar)
  | Just sVar <- msVar
  , not $ l `S.member` S.fromList (map fst sFields)
  = rowUni (l, tau, rFields, rVar) (sFields, sVar)

-- rule: row-head, uni-const, uni-var
rowUnifier r r' | r == r' = return (nullSub, nullSub)

rowUnifier r1 r2 =
  unificationError (TERowUnification r1 r2) >> return (nullSub, nullSub)

-- The heavy lifting part of uni-row when rewriting one side is necessary.
rowUni :: (Label, Type, [(Label, Type)], Maybe RowVar)
       -> ([(Label, Type)], RowVar)
       -> SolveM (TypeSub, RowSub)
rowUni (l, tau, rFields, rVar) (sFields, sVar) = do
  tau' <- freshUniTv

  -- s' is the tail of s under the substitution s1 which adds the label l and
  -- changes the row type variable
  beta <- freshUniRowTv
  let s' = Row sFields (Just beta)

  let s1 :: (TypeSub, RowSub)
      s1 = (nullSub, M.singleton sVar $ Row [(l, tau')] (Just beta))

  s2 <- unifier (subBoth s1 tau) (subBoth s1 tau')
  let s2s1 = s2 `compSubs` s1

  -- Termination check
  if null rFields && rVar == Just sVar
    then unificationError (TERowUnification
           (Row ((l, tau):rFields)       rVar)
           (Row           sFields  (Just sVar)))
         >> return (nullSub, nullSub)
    else do
      s3 <- rowUnifier (subBoth s2s1 $ Row rFields rVar) (subBoth s2s1 s')
      return $ s3 `compSubs` s2s1

bind :: T.Text -> Type -> SolveM TypeSub
bind v t
  | TVar n <- t
  , v == n
  = return nullSub

  | v `S.member` ftv t
  = unificationError (TEOccurs v t) >> return nullSub

  | TAll _ _ <- t
  = error "Unexpected"

  | otherwise
  = return $ M.singleton v t

rowBind :: T.Text -> Row -> SolveM (TypeSub, RowSub)
rowBind v t
  | Row [] (Just n) <- t
  , v == n
  = return (nullSub, nullSub)

  | v `S.member` rftv t
  = unificationError (TERowOccurs v t) >> return (nullSub, nullSub)

  | otherwise
  = return (nullSub, M.singleton v t)

freshTv' :: MonadState s m => (s -> Integer) -> (s -> s) -> T.Text -> m T.Text
freshTv' prj upd pre = do
  st <- gets prj
  let r = (pre `T.append`) . T.pack . show $ st
  modify upd
  return r

freshUniTv :: SolveM Type
freshUniTv = TVar <$>
  freshTv' ssNextTv (\s@SolveS{..} -> s { ssNextTv = ssNextTv + 1 }) "b"

freshUniRowTv :: SolveM T.Text
freshUniRowTv =
  freshTv' ssNextRowTv (\s@SolveS{..} -> s { ssNextRowTv = ssNextRowTv + 1 }) "s"

freshTv :: HmInfM Type
freshTv = TVar <$>
  freshTv' hmsCount (\s@HmInfS{..} -> s { hmsCount = hmsCount + 1 }) "a"

freshRowTv :: HmInfM T.Text
freshRowTv =
  freshTv' hmsRowCount (\s@HmInfS{..} -> s { hmsRowCount = hmsRowCount + 1 }) "r"

tellE :: TypeErr -> HmInfM ()
tellE = tell . DL.singleton . HMWError

tellC :: Constraint -> HmInfM ()
tellC c = modify (\s@HmInfS{..} -> s { hmsConstraints = c:hmsConstraints })

unificationError :: TypeErr -> SolveM ()
unificationError = tell . DL.singleton
