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

import Text.Trifecta

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

data HmInfW = HMWError TypeErr
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
type TypedE = Cofree (ExprF Type [P] T.Text) Span
type E' = Cofree (ExprF () [P] T.Text) Span

data TypeErr = TEOccurs T.Text Type
             | TERowOccurs T.Text Row
             | TEUnification Type Type
             | TERowUnification Row Row
             | TEPatternType P Type
             | TEPatternLength T.Text Integer
             | TEUnknownDataConstructor T.Text
             deriving (Eq, Show)

infer :: HmEnv -> E' -> Either [TypeErr] (Type, TypedE)
infer env expr
  | Left e <- runHm = Left e
  | Right ((t, _te), hmState, hmWriter) <- runHm
  = case DL.toList hmWriter of
      [] -> case  runUnify . hmsConstraints $ hmState of
        ((s, r), []) -> Right $ ( generalize' M.empty . rapply r . apply s $ t
                                , error "TODO: Perform substitution on te")
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

doHm :: C.CofreeF (ExprF () [P] T.Text) Span (HmInfM (Type, TypedE))
     -> HmInfM (Type, TypedE)

doHm (_ C.:< ETAbsF _ _) = error "ETAbsF in expression before inference"
doHm (_ C.:< ETAppF _ _) = error "ETAbsF in expression before inference"

doHm (sp C.:< EAbsF pss [] e) = do
  lamTvs <- mapM (const freshTv) pss

  -- Need to check patterns and unify bind type variables with the types
  -- expected in patterns. Also, unify top level bind type variables with
  -- lambda type variables
  bts <- unifyPatterns (zip lamTvs pss)

  (t, te) <- inEnv' bts e
  return (foldr TFun t lamTvs, sp :< EAbsF pss bts te)

doHm (_ C.:< ELamCaseF _pe's) = undefined

doHm (_ C.:< EAbsF _ (_:_) _) =
  error "Expected no typed binds before inference."

doHm (sp C.:< EAppF e as) = do
  tv <- freshTv
  (t1, te) <- e
  as' <- sequence as
  let t2 = foldr (TFun . fst) tv as'
  unify t1 t2
  return (tv, sp :< EAppF te (map snd as'))

doHm (sp C.:< ETupF es) = do
  es' <- sequence es
  let t = TTup $ map fst es'
  return (t, sp :< ETupF (map snd es'))

doHm (sp C.:< EArrF es) = do
  es' <- sequence es
  t <- case es' of
         [] -> freshTv >>= return . flip TArr 0
         (t,_):xs -> do
           forM_ xs (unify t . fst)
           return (TArr t . toInteger . length $ es')
  return (t, sp :< EArrF (map snd es'))

doHm (sp C.:< ELetF xs e) = do
  (t, xs', te) <- foldr f (liftM (\(t,te)->(t,[],te)) e) xs
  return (t, sp :< ELetF xs' te)
  where
    f :: ([P], HmInfM (Type, TypedE))
      -> HmInfM (Type, [([P], TypedE)], TypedE)
      -> HmInfM (Type, [([P], TypedE)], TypedE)
    f (ps, mtte) macc = do
      (t, te) <- mtte

      bts' <- unifyPatterns [(t, ps)]
      unifyResult <- runUnify <$> gets hmsConstraints
      s <- case unifyResult of
             (s, []) -> return s
             (_, es) -> throwError es
      bts'' <- local (subBoth s) $ mapM (mapM $ generalize . subBoth s) bts'

      (t', xs', te') <- inEnv' bts'' $ local (subBoth s) macc
      return $ (t', (ps, te):xs', te')

doHm (sp C.:< ERecF le's) = do
  l'tte's <- forM le's $ \(l, mtte) -> (l,) <$> mtte
  return $ ( TRec $ Row (map (second fst) l'tte's) Nothing
           , sp :< ERecF (map (second snd) l'tte's)
           )

--doHm (_ C.:< ERecUpF _le's _e) = undefined

-- Extend record with labelled expressions of exts
doHm (sp C.:< ERecExtF exts rec) = do
  (rt, rte) <- rec
  (ls, ts, tes) <- unzip3 <$>
    mapM (\(l',mtte) -> (\(y,z)-> (l',y,z)) <$> mtte) exts

  rtv <- freshRowTv
  unify rt $ TRec (Row [] $ Just rtv)

  return (TRec $ Row (zip ls ts) (Just rtv), sp :< ERecExtF (zip ls tes) rte)

doHm (_ C.:< ERecResF _e _l) = undefined
doHm (_ C.:< ERecSelF _e _l) = undefined

doHm (_ C.:< ECaseF _e _pe's) = undefined

doHm (sp C.:< EIfF b t e) = do
  (t1, te1) <- b
  (t2, te2) <- t
  (t3, te3) <- e
  unify t1 TBool
  unify t2 t3
  return (t2, sp :< EIfF te1 te2 te3)

doHm (_ C.:< EMultiIfF _gs _me) = undefined

doHm (sp C.:< EVarF n) = do
  env <- ask
  case M.lookup n env of
    Nothing ->
      error "unbound variable (previous checks should prevent)"
    Just t@(TAll _ _) -> do
      t' <- instantiate t
      return (t', sp :< EVarF n)
    Just t -> return (t, sp :< EVarF n)

doHm (sp C.:< EConstF c) = return $
  case c of
    CInt  _ -> (TCon "Int" [], sp :< EConstF c)
    CChar _ -> (TCon "Int" [], sp :< EConstF c)

doHm (sp C.:< EUnaryOpF op) = return . (, sp :< EUnaryOpF op) $
  case op of
    OpNot -> TFun (TCon "Bool" []) (TCon "Bool" [])

doHm (sp C.:< EBinOpF op) = return . (, sp :< EBinOpF op) $
  case op of
    OpAdd -> TFun (TCon "Int" []) (TFun (TCon "Int" []) (TCon "Int" []))

patternTypes :: P -> HmInfM (Type, [(Bind, Type)])
patternTypes = cata alg where
  alg :: C.CofreeF PatternF Span (HmInfM (Type, [(Bind, Type)]))
                               -> HmInfM (Type, [(Bind, Type)])
  alg (_ C.:< PPatF n xs) = do
    mt <- M.lookup n <$> ask
    case mt of
      Just conTy -> do
        conTy' <- instantiate conTy

        when (arity conTy' /= toInteger (length xs)) $
          throwError [TEPatternLength n . toInteger . length $ xs]

        (ts, bs) <- unzip <$> sequence xs
        mapM_ (uncurry unify) $ zip ts (argTypes conTy')
        return (returnType conTy', concat bs)

      Nothing -> throwError [TEUnknownDataConstructor n]

  alg (_ C.:< PTupF xs) = sequence xs >>= \xs' ->
    return $ (TTup (fst <$> xs'), concatMap snd xs')

  alg (_ C.:< POrF _xs) = error "TODO"
  alg (_ C.:< PRecF fs mrow) = do
    (ls, (ts, btss)) <- second unzip . unzip <$> mapM (\(l,m) -> (l,) <$> m) fs
    let bts = concat btss :: [(Bind, Type)]
        lts = zip ls ts   :: [(Label, Type)]
    case mrow of
      Just r -> freshRowTv >>= \tv ->
                  let t  = TRec $ Row lts (Just tv)
                      rt = TRec $ Row []  (Just tv)
                   in return (t, bool ((r, rt):bts) bts (r == "_"))
      Nothing -> return (TRec $ Row lts Nothing, bts)

  alg (_ C.:< PArrF xs) = sequence xs >>= \case
    [] -> freshTv >>= \tv -> return (TArr tv 0, [])
    xs'@((t,_):_) -> do
      mapM_ (unify t . fst) xs'
      return (TArr t $ toInteger (length xs'), concatMap snd xs')

  alg (_ C.:< PConstF (CInt _)) = return (TInt, [])
  alg (_ C.:< PConstF _) = error "TODO"
  alg (_ C.:< PWildF) = (,[]) <$> freshTv
  alg (_ C.:< PMultiWildF) = error "TODO"
  alg (_ C.:< PBindF n Nothing) = freshTv >>= \tv -> return (tv, [(n, tv)])
  alg (_ C.:< PBindF n (Just p)) = p >>= \(t, bs) -> return (t, (n,t):bs)

unifyPatterns :: [(Type, [P])] -> HmInfM [(Bind, Type)]
unifyPatterns tps = do
  bindMMap <- forM tps $ \(t, ps) -> flip (flip foldrM MM.empty) ps $
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
  let errFn _new _old = error $ "Shadowed variable"
  let scope e = M.insertWith errFn n s e
  local scope m

inEnv' :: [(T.Text, Type)] -> HmInfM a -> HmInfM a
inEnv' bs m = foldr inEnv m bs

unify :: Type -> Type -> HmInfM ()
unify t1 t2 = --traceM (show (t1,t2)) >>
  tellC (t1, t2)

generalize :: Type -> HmInfM Type
generalize t = liftM (flip generalize' t) ask

instantiate :: Type -> HmInfM Type
instantiate (TAll vs t) = do
  sub <- forM vs $ \v -> (v,) <$> freshTv
  return $ apply (M.fromList sub) t
instantiate t = return t

unificationSolver :: Unifier -> SolveM (TypeSub, RowSub)
unificationSolver (s, []) = return s
-- Prior to unification records' row types may be in arbitrary order, so we do
-- a sort first. During unification further sorts are unnecessary because
-- substitutions maintain the fields ordering (see rapply).
unificationSolver (s, (t1, t2):cs) = unifier (sortRecords t1) (sortRecords t2)
  >>= (\s' -> unificationSolver (s' `compSubs` s, subBoth s' cs))

unifier :: Type -> Type -> SolveM (TypeSub, RowSub)
unifier t1 t2 | t1 == t2 = return (nullSub, nullSub)
unifier (TFun l r) (TFun l' r') = unifyMany [l, r] [l', r']

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
  if rFields == [] && rVar == Just sVar
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
  = return $ (nullSub, M.singleton v t)

freshTv' :: MonadState s m => (s -> Integer) -> (s -> s) -> T.Text -> m T.Text
freshTv' prj upd pre = do
  st <- gets prj
  let r = (pre `T.append`) . T.pack . show $ st
  modify upd
  return r

freshUniTv :: SolveM Type
freshUniTv = TVar <$>
  freshTv' ssNextTv (\s@(SolveS{..}) -> s { ssNextTv = ssNextTv + 1 }) "b"

freshUniRowTv :: SolveM T.Text
freshUniRowTv =
  freshTv' ssNextRowTv (\s@(SolveS{..}) -> s { ssNextRowTv = ssNextRowTv + 1 }) "s"

freshTv :: HmInfM Type
freshTv = TVar <$>
  freshTv' hmsCount (\s@(HmInfS{..}) -> s { hmsCount = hmsCount + 1 }) "a"

freshRowTv :: HmInfM T.Text
freshRowTv =
  freshTv' hmsRowCount (\s@(HmInfS{..}) -> s { hmsRowCount = hmsRowCount + 1 }) "r"

tellE :: TypeErr -> HmInfM ()
tellE = tell . DL.singleton . HMWError

tellC :: Constraint -> HmInfM ()
tellC c = modify (\s@(HmInfS{..}) -> s { hmsConstraints = c:hmsConstraints })

unificationError :: TypeErr -> SolveM ()
unificationError = tell . DL.singleton
