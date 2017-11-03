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

type Bind = T.Text
type TypeSub = M.Map T.Text Type
type RowSub = M.Map T.Text Row
type Constraint = (Type, Type)
type Unifier = (TypeSub, [Constraint])
type SolveM = WriterT (DL.DList TypeErr) Identity
type HmEnv = M.Map T.Text Type

class Substitutable a where
  nullSub :: M.Map T.Text a
  nullSub = M.empty

  compSub :: M.Map T.Text a -> M.Map T.Text a -> M.Map T.Text a
  l `compSub` r = fmap (applySub l) r `M.union` l

  applySub :: M.Map T.Text a -> a -> a

instance Substitutable Type where
  applySub = apply

instance Substitutable Row where
  applySub = rapply

emptyEnv :: HmEnv
emptyEnv = M.empty

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
        (s, []) -> Right $ ( generalize' M.empty . apply s $ t
                           , error "TODO: Perform substitution on te")
        (_, es) -> Left es
      es  -> Left $ map (\(HMWError e) -> e) es
  where
    runHm = runIdentity $ runExceptT $ runRWST (cata doHm expr) env initState

runUnify :: [Constraint] -> (TypeSub, [TypeErr])
runUnify =
  second DL.toList . runIdentity . runWriterT . unificationSolver . (nullSub,)

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

      --bts <- MM.fromList . concat <$> mapM (flip matchPatternToType t) ps
      ---- Check bts for duplicate binds and make sure their types unify
      --bts' <- unifyDups bts
      --bts'' <- forM bts' $ \(b,bt) -> generalize bt >>= return . (b,)

      bts' <- unifyPatterns [(t, ps)]
      unifyResult <- runUnify <$> gets hmsConstraints
      sub <- case unifyResult of
               (s, []) -> return s
               (_, es)  -> throwError es
      bts'' <- local (apply sub) $ mapM (mapM $ generalize . apply sub) bts'

      (t', xs', te') <- inEnv' bts'' (local (apply sub) macc)
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

--mostSpecificType :: P -> Type -> [(Bind, Type)]
--mostSpecificType pat typ = go pat typ [] where
--  go = undefined

--matchPatternToType :: P -> Type -> HmInfM [(Bind, Type)]
--matchPatternToType p t = go p t (return []) where
--  go :: P -> Type -> HmInfM [(Bind, Type)] -> HmInfM [(Bind, Type)]
--  go (_ :< PPatF n xs) _t' a = do
--    mt <- M.lookup n <$> ask
--    case mt of
--      Just conTy -> do
--        -- Ensure that the shape of the constructor and t' match
--        -- We don't care what the types of conTy get instantiated to
--        -- Actually, this does nothing since t' gets generalized
--        --join $ unify t' <$> instantiate (returnType conTy)
--        when (arity conTy /= toInteger (length xs)) $
--          throwError $ TEPatternLength n xs
--        foldr (\(x, y) acc -> go x y acc) a (zip xs $ argTypes conTy)
--
--      Nothing -> throwError $ TEUnknownDataConstructor n
--
--  go p'@(_ :< PTupF xs) t'@(TTup xs') a = do
--    when (length xs == length xs') $ throwError $ TEPatternType p' t'
--    a' <- a
--    foldrM (\(x, y) acc -> (++acc) <$> matchPatternToType x y) a' (zip xs xs')
--
--  go (_ :< POrF _xs)     _ _ = error "Shouldn't happen. Patterns should \
--                                     \be expanded before type inference."
--
--  go (_ :< PRecF _fs)    _ _ = error "TODO"
--
--  go p'@(_ :< PArrF xs) t'@(TArr u n) a = do
--    when (toInteger (length xs) /= n) $ throwError $ TEPatternType p' t'
--    foldr (\x a' -> go x u a') a xs
--
--  go (_ :< PConstF _c)   _ _ = error "TODO"
--  go (_ :< PWildF)       _ a = a
--  go (_ :< PMultiWildF)  _ a = a
--  go (_ :< PBindF n Nothing)  u a = ((n,u):) <$> a
--  go (_ :< PBindF n (Just q)) u a = ((n,u):) <$> go q u a
--  go _ _ _ = error "TODO"

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
  alg (_ C.:< PRecF fs) = do
    fs' <- mapM (\(l,m) -> (l,) <$> m) fs
    rtv <- freshRowTv
    return ( TRec $ Row (map (second fst) fs') (Just rtv)
           , join $ map (snd . snd) fs'
           )

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

--unifyPatterns :: [(Type, [P])] -> HmInfM [(Bind, Type)]
--unifyPatterns tps = do
--  let bs = concatMap (fmap fst . patternBinds) (concatMap snd tps)
--  bindTvs <- forM bs $ \b -> (b,) <$> freshTv
--  let bindMap = M.fromList bindTvs
--  forM_ tps $ \(t, ps) -> forM_ ps $ \p -> do
--    (patTy, bindTys) <- patternTypes p
--    unify t patTy
--    sequence_ $ M.intersectionWith unify bindMap $ M.fromList bindTys
--  return $ M.toList bindMap

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

unificationSolver :: Unifier -> SolveM TypeSub
unificationSolver (s, []) = return s
unificationSolver (s, (t1, t2):cs) = unifier t1 t2 >>=
  \s' -> unificationSolver (s' `compSub` s, apply s' cs)

unifier :: Type -> Type -> SolveM TypeSub
unifier t1 t2 | t1 == t2 = return nullSub
unifier (TFun l r) (TFun l' r') = unifyMany [l, r] [l', r']

unifier (TVar n) t = bind n t
unifier t (TVar n) = bind n t

unifier t@(TCon n ts) t'@(TCon n' ts')
  | n == n' = unifyMany ts ts'
  | otherwise = unificationErr (TEUnification t t') >> unifyMany ts ts'
unifier (TTup ts) (TTup ts') = unifyMany ts ts'

unifier (TArr t i) (TArr t' i')
  | i == i' = unifier t t'

-- Sorts row fields so rowUnifier can assume their already ordered
unifier (TRec r) (TRec r') = undefined rowUnifier (sortRows r) (sortRows r')

unifier (TAll _ _) _ = error "Unexpected"
unifier _ (TAll _ _) = error "Unexpected"

unifier t t' = unificationErr (TEUnification t t') >> return nullSub

rowUnifier :: Row -> Row -> SolveM (TypeSub, RowSub)
rowUnifier r1@(Row fs1 Nothing) r2@(Row fs2 Nothing)
  | map fst fs1 == map fst fs2
  = (,nullSub) <$> unifyMany (map snd fs1) (map snd fs2)
  | otherwise
  = unificationErr (TERowUnification r1 r2) >> return (nullSub, nullSub)

rowUnifier (Row _fs1 (Just _v)) (Row _fs2 (Just _v')) = undefined

rowUnifier (Row _fs1 (Just _v)) (Row _fs2 Nothing) = undefined

rowUnifier (Row _fs1 Nothing) (Row _fs2 (Just _v)) = undefined

bind :: T.Text -> Type -> SolveM TypeSub
bind v t
  | TVar n <- t
  , v == n
  = return nullSub

  | v `S.member` ftv t
  = unificationErr (TEOccurs v t) >> return nullSub

  | TAll _ _ <- t
  = error "Unexpected"

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
  modify (\s@(HmInfS{..}) -> s { hmsCount = c + 1 })
  return $ TVar $ T.pack $ 'a' : show c

freshRowTv :: HmInfM T.Text
freshRowTv = do
  c <- gets hmsRowCount
  modify (\s@(HmInfS{..}) -> s { hmsRowCount = c + 1 })
  return $ T.pack $ 'r' : show c

tellE :: TypeErr -> HmInfM ()
tellE = tell . DL.singleton . HMWError

tellC :: Constraint -> HmInfM ()
tellC c = modify (\s@(HmInfS{..}) -> s { hmsConstraints = c:hmsConstraints })

unificationErr :: TypeErr -> SolveM ()
unificationErr = tell . DL.singleton
