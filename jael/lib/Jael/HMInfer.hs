{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Jael.HMInfer where

import           Control.Exception                        ( assert )

import qualified Data.DList                    as DL
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import qualified Jael.Constants                as JC
import qualified Jael.Data.MultiMap            as MM

import           Jael.Classes
import           Jael.Expr
import           Jael.Pattern
import           Jael.Prelude                      hiding ( Constraint )
import           Jael.TH
import           Jael.Type

type TypeSub = M.Map T.Text (Either Type' Row')
type Constraint = Pair Type'

type SolveM = StateT SolveS (WriterT (DL.DList TypeErr) Identity)
type HmEnv = M.Map T.Text Type'

type ParsePatternF' = PatternF' T.Text T.Text Literal
type ParsePattern'  = Pattern'  T.Text T.Text Literal
type TypedPat = Pattern' (T.Text, Type') T.Text Literal
type TypedPats = [TypedPat]

-- Input expression type
-- A parsed expression whose patterns are expanded to remove "or" patterns
type E' = Expr' [ParsePattern']
type EF' = ExprF' [ParsePattern']
-- Output expression type
type TypedExpr = ExprT' Type' TypedPats

data SolveS = SolveS { ssNextTv :: Integer, ssNextRowTv :: Integer }
  deriving (Eq, Show)

initSolveSt :: SolveS
initSolveSt = SolveS {ssNextTv = 0, ssNextRowTv = 0}

class ApplyTypeSub a where
  apply :: TypeSub -> a -> a

instance {-# Overlappable #-} (Functor f, ApplyTypeSub a) => ApplyTypeSub (f a) where
  apply s = fmap (apply s)

instance ApplyTypeSub Type' where
  apply s t = subTVar (lookupType s) (cata alg t)
    where

    alg :: Type'F Type' -> Type'
    alg (FV (TRecF r)) = TRec @T.Text @T.Text (apply s r)
    alg (FV (TAllF v t')) = case M.lookup v s of
      Just (Left (TVar v'))     -> TAll @T.Text v' t'
      Just (Right (TRowVar v')) -> TAll @T.Text v' t'
      _                         -> TAll v t'
    alg t'             = Fix t'

    lookupType :: TypeSub -> T.Text -> Maybe Type'
    lookupType m v = case M.lookup v m of
      Just (Left x) -> Just x
      _             -> Nothing

instance ApplyTypeSub Row' where
  apply s r = subRVar (lookupRow s) (cata alg r)
    where

    alg :: Row'F Row' -> Row'
    alg (FV (TRowExtF (b::T.Text, t::Type') r')) = TRowExt (b, apply s t) r'
    alg r'                                       = Fix r'

    lookupRow  :: TypeSub -> T.Text -> Maybe Row'
    lookupRow m v = case M.lookup v m of
      Just (Right x) -> Just x
      _              -> Nothing

instance ApplyTypeSub (Either Type' Row') where
  apply s = bimap (apply s) (apply s)

instance ApplyTypeSub TypedExpr where
  apply s e =
    let applyToP :: TypedPats -> TypedPats
        applyToP = fmap $ applyVar (\((v,t)::(T.Text,Type')) -> (v, apply s t))
        applyToT :: Type' -> Type'
        applyToT = apply s
     in overExpr applyToT applyToP (id :: T.Text -> T.Text) e

compSub :: TypeSub -> TypeSub -> TypeSub
l `compSub` r = apply l r `M.union` l

nullSub :: M.Map T.Text a
nullSub = M.empty

newtype HmInfW
  = HMWError TypeErr
  deriving (Eq, Show)

data HmInfS = HmInfS
  { hmsCount       :: Integer
  , hmsRowCount    :: Integer
  , hmsConstraints :: [Constraint]
  } deriving (Eq, Show)

initState :: HmInfS
initState = HmInfS {hmsCount = 0, hmsRowCount = 0, hmsConstraints = []}

type HmInfM = RWST HmEnv (DL.DList HmInfW) HmInfS (Except [TypeErr])

data TypeErr
  = TEOccurs T.Text Type'
  | TERowOccurs T.Text Row'
  | TEUnification Type' Type'
  | TERowUnification Row' Row'
  | TEPatternType ParsePattern' Type'
  | TEPatternLength T.Text Integer
  | TEUnknownDataConstructor T.Text
  deriving (Eq, Show)


mapSubPatBinds :: [(T.Text, Type')] -> [ParsePattern'] -> TypedPats
mapSubPatBinds bt's ps = fmap (subPatVars sub) ps
 where
  sub :: M.Map T.Text (T.Text, Type')
  sub = M.mapWithKey (,) $ M.fromList bt's

-- Substitute variables in patterns with (variable, type) given by s
-- Partial function: All bound variables in the provided patterns must have
-- a key in the map.
subPatVars :: M.Map T.Text (T.Text, Type') -> ParsePattern' -> TypedPat
subPatVars s p = case applyVarMaybe (`M.lookup` s) p of
  Left vs ->
    error
      $  "Expected all variables in pattern to be in substitution:\n"
      <> show vs
  Right x -> x

infer :: HmEnv -> E' -> Either [TypeErr] (Type', TypedExpr)
infer env expr = do
  ((t, te), hmState) <- runHm env (cata doHm expr)
  s                  <- runUnify (hmsConstraints hmState)
  let te' = apply s te
  let t'  = apply s t
  assert (ftv t' == (ftv te' :: S.Set T.Text)) (pure ())
  let fvs = S.toList (ftv t') :: [T.Text]
  return (foldr TAll t' fvs, foldr ETAbs te' fvs)

runHm :: M.Map T.Text Type' -> HmInfM a -> Either [TypeErr] (a, HmInfS)
runHm env x = do
  (r, s, w) <- runIdentity $ runExceptT $ runRWST x env initState
  let es = DL.toList w
  unless (null es) $ Left (fmap (\(HMWError e) -> e) es)
  return (r, s)

runUnify :: [Constraint] -> Either [TypeErr] TypeSub
runUnify cs =
  let res =
        first fst
          . second DL.toList
          . runIdentity
          . runWriterT
          . flip runStateT initSolveSt
          . unificationSolver
          . (nullSub, )
          $ cs
  in  case res of
        (subs, []) -> Right subs
        (_   , es) -> Left es

type HMResult = HmInfM (Type', TypedExpr)

doHm :: ExprF' [ParsePattern'] HMResult -> HMResult

doHm (FV (EAbsF pss e)) = do
  lamTvs <- mapM (const freshTv) pss

  -- Need to check patterns and unify bind type variables with the types
  -- expected in patterns. Also, unify top level bind type variables with
  -- lambda type variables
  bts    <- unifyPatterns (zip lamTvs pss)
  let pss' = fmap (mapSubPatBinds bts) pss

  (t, te) <- inEnv' bts e
  return
    ( foldr (TFun (Nothing :: Maybe T.Text)) t lamTvs
    , EAbsT (zip pss' lamTvs) te
    )

doHm (FV (ELamCaseF pe's)) = do
  let (pss, es) = unzip pe's
  tv    <- freshTv

  -- The binds for each case
  bts's <- mapM (unifyPatterns . (\b -> [(tv, b)])) pss
  let pss' = zipWith mapSubPatBinds bts's pss
  -- The expressions evaluated in the context of the binds
  -- Not sure why the compiler doesn't complain about not handling the empty
  -- list case, but in any case, the parser should make sure pe's is
  -- non-empty and therefore (t:ts) will always match.
  (t : ts, tes) <- unzip <$> forM (zip bts's es) (uncurry inEnv')

  -- All expression types must unify
  mapM_ (unify t) ts

  return (TFun (Nothing :: Maybe T.Text) tv t, ELamCaseT tv (zip pss' tes))

doHm (FV (EAppF e as)) = do
  tv       <- freshTv
  (t1, te) <- e
  as'      <- sequence as
  let t2 = foldr (TFun @T.Text Nothing . fst) tv as'
  unify t1 t2
  return (tv, EApp te (fmap snd as'))

doHm (FV (ETupF es)) = do
  es' <- sequence es
  let t = TTup $ fmap fst es'
  return (t, ETup (fmap snd es'))

doHm (FV (EArrF es)) = do
  es' <- sequence es
  t   <- case es' of
    []          -> flip TArr 0 <$> freshTv
    (t, _) : xs -> do
      forM_ xs (unify t . fst)
      return (TArr t . length $ es')
  return (t, EArr (fmap snd es'))

doHm (FV (ELetF xs e :: ELetF [ParsePattern'] HMResult)) = do
  (t, xs', te) <- foldr f ((\(t, te) -> (t, [], te)) <$> e) xs
  return (t, ELetT xs' te)
 where
  f :: ([ParsePattern'], HmInfM (Type', TypedExpr))
    -> HmInfM (Type', [((TypedPats, Type'), TypedExpr)], TypedExpr)
    -> HmInfM (Type', [((TypedPats, Type'), TypedExpr)], TypedExpr)
  -- let x = e1 in e2
  f (ps, mtte) macc = do
    (xt, e1)    <- mtte

    -- Get generalized types for the binds in the pattern
    bts'        <- unifyPatterns [(xt, ps)]
    unifyResult <- runUnify <$> gets hmsConstraints
    s           <- either throwError return unifyResult
    bts''       <- local (apply s) $ mapM (mapM $ generalize . apply s) bts'

    -- Determine the generalized type of x, substitute pattern binds with
    -- types, and generalize the expression bound to x
    xt'         <- local (apply s) (generalize $ apply s xt)
    let ps' = mapSubPatBinds bts'' ps
    let tvs = fst (schemeVars xt')
    let e1' = foldr ETAbs e1 tvs

    (t', xs', te') <- inEnv' bts'' $ local (apply s) macc

    return (t', ((ps', xt'), e1') : xs', te')

doHm (FV (ERecEmptyF :: ERecEmptyF HMResult)) =
  return (TRec (TRowEmpty :: Row'), ERecEmpty)

doHm (FV (ERecF e)) = do
  (rt, rte) <- e
  rtv       <- freshRowTv
  unify rt $ TRec (TRowVar rtv :: Row')
  return (TRec (TRowVar rtv :: Row'), ERec rte)

doHm (FV (ERecExtendF l e rec)) = do
  (rt, rte) <- rec
  (t , te ) <- e
  rtv       <- freshRowTv
  unify rt $ TRec (TRowVar rtv :: Row')
  return (TRec $ polyRow [(l, t)] rtv, ERecExtend l te rte)

doHm (FV (ERecUpdateF l e rec)) = do
  (rt, rte) <- rec
  (t , te ) <- e
  rtv       <- freshRowTv
  tv        <- freshTv
  unify rt (TRec $ polyRow [(l, tv)] rtv)
  return (TRec $ polyRow [(l, t)] rtv, ERecUpdate l te rte)

doHm (FV (ERecRenameF to from rec)) = do
  (rt, rte) <- rec
  rtv       <- freshRowTv
  tv        <- freshTv
  unify rt (TRec $ polyRow [(from, tv)] rtv)
  return (TRec $ polyRow [(to, tv)] rtv, ERecRename to from rte)

doHm (FV (ERecRemoveF rec l)) = do
  (rt, rte) <- rec
  tv        <- freshTv
  rtv       <- freshRowTv
  unify rt $ TRec (polyRow [(l, tv)] rtv)
  return (TRec (TRowVar rtv :: Row'), ERecRemove rte l)

doHm (FV (ERecSelectF rec l)) = do
  (rt, rte) <- rec
  tv        <- freshTv
  rtv       <- freshRowTv
  unify rt $ TRec (polyRow [(l, tv)] rtv)
  return (tv, ERecSelect rte l)

doHm (FV (ECaseF _ [] :: ECaseF [ParsePattern'] HMResult)) =
  error "The parser should make this impossible"

doHm (FV (ECaseF e (pe : pe's))) = do
  (t, te) <- e
  let (headPats, headExpr)    = pe
  let (tailPats's, tailExprs) = unzip pe's

  -- headExpr and tailExprs must be evaluated in a context with pattern binds
  -- and the patterns must unify with the type of e
  head'bts                  <- unifyPatterns [(t, headPats)]
  (headType, headTypedExpr) <- inEnv' head'bts headExpr

  tail'bts's                <- mapM (unifyPatterns . (: []) . (t, )) tailPats's

  let tailExprsInEnv = zipWith inEnv' tail'bts's tailExprs
  (tailTypes, tailTypedExprs) <- unzip <$> sequence tailExprsInEnv

  -- All cases must unify to the same type
  forM_ tailTypes $ unify headType

  let headPats'   = mapSubPatBinds head'bts headPats
  let tailPats's' = zipWith mapSubPatBinds tail'bts's tailPats's

  return
    ( headType
    , ECase te ((headPats', headTypedExpr) : zip tailPats's' tailTypedExprs)
    )

doHm (FV (EIfF b t e)) = do
  (t1, te1) <- b
  (t2, te2) <- t
  (t3, te3) <- e
  unify t1 tBool
  unify t2 t3
  return (t2, EIf te1 te2 te3)

doHm (FV (EMultiIfF [] _ :: EMultiIfF HMResult)) =
  error "Parser shouldn't allow this case"

doHm (FV (EMultiIfF ((g :# e) : gs) me)) = do
  g'         <- g
  e'         <- e
  (gs', es') <- unzip <$> forM gs (\(h :# f) -> (,) <$> h <*> f)

  -- All guards must be bools
  forM_ (g' : gs') $ unify tBool . fst
  -- All expressions must unify
  forM_ es' $ unify (fst e') . fst

  -- If there's a default, it must also unify
  me' <- sequence me
  forM_ me' $ unify (fst e') . fst

  return
    ( fst e'
    , EMultiIf (zipWith (:#) (fmap snd (g' : gs')) (fmap snd (e' : es')))
               (snd <$> me')
    )

doHm (FV (EVarF n :: EVarF T.Text HMResult)) = do
  env <- ask
  case M.lookup n env of
    Nothing -> error $ "unbound variable \"" <> T.unpack n <> "\""
    Just t  -> do
      (t', insts) <- instantiate t
      let e = case insts of
            [] -> EVar n
            xs -> foldr ETApp (EVar n) xs
      return (t', e)

doHm (FV (EOpF o :: EOpF HMResult)) = return . (, EOp o) . JC.opType $ o

doHm (FV (ELitF l :: ELitF HMResult)) = return . (, ELit l) . JC.litType $ l

doHm _ = error "Unexpected default case hit"

unifyPatterns :: [(Type', [ParsePattern'])] -> HmInfM [(Bind, Type')]
unifyPatterns tps = do
  bindMMap <- forM tps $ \(t, ps) -> flip (`foldrM` MM.empty) ps $ \p m -> do
    (patTy, bindTys) <- patternTypes p
    unify t patTy
    return $ foldr (uncurry MM.insert) m bindTys
  -- If there were duplicate binds they have to unify
  unifyDups . MM.unionsWith (<>) $ bindMMap

unifyDups :: MM.MultiMap Bind Type' -> HmInfM [(Bind, Type')]
unifyDups bs =
  let unifyList1 :: [Type'] -> HmInfM Type'
      unifyList1 (t : ts) = mapM_ (unify t) ts >> return t
      unifyList1 _        = error "Unexpected empty list."
  in  forM (MM.assocs bs) $ \(b, ts) -> (b, ) <$> unifyList1 ts

-- MultiMaps never have keys with no values

inEnv :: (T.Text, Type') -> HmInfM a -> HmInfM a
inEnv (n, s) m = do
  let errFn _new _old =
        error "Shadowed variable. Previous checks should prevent."
  let scope = M.insertWith errFn n s
  local scope m

inEnv' :: [(T.Text, Type')] -> HmInfM a -> HmInfM a
inEnv' bs m = foldr inEnv m bs

unify :: Type' -> Type' -> HmInfM ()
unify t1 t2 = tellC (t1 :# t2)

generalize :: Type' -> HmInfM Type'
generalize t = (`generalize'` t) <$> ask

instantiate :: Type' -> HmInfM (Type', [Type'])
instantiate (TAll v t) = do
  fv        <- freshTv
  (t', fvs) <- instantiate t
  let sub = M.fromList [(v, Left fv)]
  return (apply sub t', fv : fvs)
instantiate t = return (t, [])

unificationSolver :: (TypeSub, [Constraint]) -> SolveM TypeSub
unificationSolver (s, []) = return s
-- Prior to unification records' row types may be in arbitrary order, so we do
-- a sort first. During unification further sorts are unnecessary because
-- substitutions maintain the fields' ordering.
unificationSolver (s, (t1 :# t2) : cs) =
  unifier (sortRecords t1) (sortRecords t2)
    >>= (\s' -> unificationSolver (s' `compSub` s, apply s' cs))

unifier :: Type' -> Type' -> SolveM TypeSub
unifier t1 t2 | t1 == t2 = return nullSub
unifier (TFun (_ :: Maybe T.Text) l r) (TFun (_ :: Maybe T.Text) l' r') =
  unifyMany [l, r] [l', r']

unifier (TVar n) t        = bind n t
unifier t        (TVar n) = bind n t

unifier (TCon (n :: T.Text) ts) (TCon (n' :: T.Text) ts') | n == n' =
  unifyMany ts ts'
unifier (TTup ts) (TTup ts')                          = unifyMany ts ts'
unifier (TArr t i) (TArr t' i') | i == i'             = unifier t t'

unifier (TRec r              ) (TRec r')              = rowUnifier r r'

unifier (TAll (_ :: T.Text) _) _                      = error "Unexpected"
unifier _                      (TAll (_ :: T.Text) _) = error "Unexpected"

unifier t t' = unificationError (TEUnification t t') >> return nullSub

unifyMany :: [Type'] -> [Type'] -> SolveM TypeSub
unifyMany []         []         = return nullSub
unifyMany (t1 : t1s) (t2 : t2s) = do
  s1 <- unifier t1 t2
  s2 <- unifyMany (apply s1 t1s) (apply s1 t2s)
  return $ s2 `compSub` s1

unifyMany _ _ = error "bleh"

rowUnifier :: Row' -> Row' -> SolveM TypeSub

-- rules: uni-varl and uni-varr
-- Just a row type variable on either side, same as bind for things of kind *
rowUnifier (TRowVar v) r           = rowBind v r
rowUnifier r           (TRowVar v) = rowBind v r

-- rule: uni-row
rowUnifier r1 r2
  | ((l, t) : fs, mv) <- decomposeRow r1
  , ((l', t') : fs', mv') <- decomposeRow r2
  , l == l'
  = do
      -- s1 is the null substitution since no rewrite is necessary
    s2 <- unifier t t'
    s3 <- rowUnifier (apply s2 $ mkRow fs mv) (apply s2 $ mkRow fs' mv')
    return (s3 `compSub` s2)

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
rowUnifier r1 r2
  | (sFields, Just sVar) <- decomposeRow r2
  , ((l, tau) : rFields, rVar) <- decomposeRow r1
  , not $ l `S.member` S.fromList (fmap fst sFields)
  = rowUni (l, tau, rFields, rVar) (sFields, sVar)

-- Identical to previous case except the left and right patterns are swapped
rowUnifier r1 r2
  | (sFields, Just sVar) <- decomposeRow r1
  , ((l, tau) : rFields, rVar) <- decomposeRow r2
  , not $ l `S.member` S.fromList (fmap fst sFields)
  = rowUni (l, tau, rFields, rVar) (sFields, sVar)

-- rule: row-head, uni-const, uni-var
rowUnifier r r' | r == r' = return nullSub

rowUnifier r1 r2 = unificationError (TERowUnification r1 r2) >> return nullSub

-- The heavy lifting part of uni-row when rewriting one side is necessary.
rowUni :: (Label, Type', [(Label, Type')], Maybe T.Text)
       -> ([(Label, Type')], T.Text)
       -> SolveM TypeSub
rowUni (l, tau, rFields, rVar) (sFields, sVar) = do
  tau' <- freshUniTv

  -- s' is the tail of s under the substitution s1 which adds the label l and
  -- changes the row type variable
  beta <- freshUniRowTv
  let s' = polyRow sFields beta

  let s1 :: TypeSub
      s1 = M.singleton sVar $ Right (polyRow [(l, tau')] beta)

  s2 <- unifier (apply s1 tau) (apply s1 tau')
  let s2s1 = s2 `compSub` s1

  -- Termination check
  if null rFields && rVar == Just sVar
    then
      unificationError
          (TERowUnification (mkRow ((l, tau) : rFields) rVar)
                            (mkRow sFields (Just sVar))
          )
        >> return nullSub
    else do
      s3 <- rowUnifier (apply s2s1 $ mkRow rFields rVar) (apply s2s1 s')
      return $ s3 `compSub` s2s1

bind :: T.Text -> Type' -> SolveM TypeSub
bind v t
  | TVar n <- t, v == n       = return nullSub
  | v `S.member` ftv t = unificationError (TEOccurs v t) >> return nullSub
  | TAll (_ :: T.Text) _ <- t = error "Unexpected"
  | otherwise                 = return $ M.singleton v (Left t)

rowBind :: T.Text -> Row' -> SolveM TypeSub
rowBind v t
  | VF (TRowVarF n) <- t, v == n = return nullSub
  | v `S.member` ftv t = unificationError (TERowOccurs v t) >> return nullSub
  | otherwise = return $ M.singleton v (Right t)

freshTv' :: MonadState s m => (s -> Integer) -> (s -> s) -> T.Text -> m T.Text
freshTv' prj upd pre = do
  st <- gets prj
  let r = (pre `T.append`) . T.pack . show $ st
  modify upd
  return r

freshUniTv :: SolveM Type'
freshUniTv = TVar
  <$> freshTv' ssNextTv (\s@SolveS {..} -> s { ssNextTv = ssNextTv + 1 }) "b"

freshUniRowTv :: SolveM T.Text
freshUniRowTv = freshTv'
  ssNextRowTv
  (\s@SolveS {..} -> s { ssNextRowTv = ssNextRowTv + 1 })
  "s"

freshTv :: HmInfM Type'
freshTv = TVar
  <$> freshTv' hmsCount (\s@HmInfS {..} -> s { hmsCount = hmsCount + 1 }) "a"

freshRowTv :: HmInfM T.Text
freshRowTv = freshTv' hmsRowCount
                      (\s@HmInfS {..} -> s { hmsRowCount = hmsRowCount + 1 })
                      "r"

tellE :: TypeErr -> HmInfM ()
tellE = tell . DL.singleton . HMWError

tellC :: Constraint -> HmInfM ()
tellC c = modify (\s@HmInfS {..} -> s { hmsConstraints = c : hmsConstraints })

unificationError :: TypeErr -> SolveM ()
unificationError = tell . DL.singleton

class PatternTypes (f :: * -> *) where
  patternTypes' :: f (HmInfM (Type', [(Bind, Type')])) -> HmInfM (Type', [(Bind, Type')])

class PatternRows (f :: * -> *) where
  patternRows' :: f (HmInfM (Maybe Bind, Row')) -> HmInfM (Maybe Bind, Row')

patternTypes :: ParsePattern' -> HmInfM (Type', [(Bind, Type')])
patternTypes = cata patternTypes'

patternRows :: RecTailPat Bind -> HmInfM (Maybe Bind, Row')
patternRows = cata patternRows'

$(mkVariantInstances ''PatternTypes)
$(mkVariantInstances ''PatternRows)

instance PatternTypes (PLitF Literal) where
  patternTypes' (PLitF (LInt _)) = return (tInt, [])

instance PatternTypes PWildF where
  patternTypes' PWildF = (,[]) <$> freshTv

instance PatternTypes PRecEmptyF where
  patternTypes' PRecEmptyF = return (TRec @T.Text @T.Text TRowEmpty, [])

instance PatternTypes (PVarF Bind) where
  patternTypes' (PVarF v) = (\tv -> (tv, [(v, tv)])) <$> freshTv

instance PatternTypes (PAtF Bind) where
  patternTypes' (PAtF v p) = (\(t, bs) -> (t, (v, t):bs)) <$> p

instance PatternTypes (PConF Label) where
  patternTypes' (PConF c ps) = do
    mt <- M.lookup c <$> ask
    case mt of
      Just conTy -> do
        conTy' <- fst <$> instantiate conTy

        when (arity conTy' /= length ps) $
          throwError [TEPatternLength c . length $ ps]

        (ts, bs) <- unzip <$> sequence ps
        mapM_ (uncurry unify) $ zip ts (argTypes conTy')
        return (returnType conTy', concat bs)

      Nothing -> throwError [TEUnknownDataConstructor c]

instance PatternTypes PTupF where
  patternTypes' (PTupF ps) = (\ps' -> (TTup (fmap fst ps'), concatMap snd ps')) <$> sequence ps

instance PatternTypes PArrF where
  patternTypes' (PArrF ps) = do
    ps' <- sequence ps
    case ps' of
      [] -> (\tv -> (TArr tv 0, []) ) <$> freshTv
      xs@((t,_):_) -> mapM_ (unify t . fst) xs >> return (TArr t (length xs), concatMap snd xs)

instance PatternTypes (PRecF Bind Label) where
  patternTypes' (PRecF fs rtail) = do
    (ls, (ts, btss)) <- second unzip . unzip <$> mapM (\(l,m) -> (l,) <$> m) fs
    let bts = concat btss :: [(Bind, Type')]
        lts = zip ls ts   :: [(Label, Type')]
    rtail' <- patternRows rtail
    case rtail' of
      (Just v, r) -> return (TRec $ extendRow r lts, (v, TRec r):bts)
      (_     , r) -> return (TRec $ extendRow r lts, bts)

instance PatternRows (PVarF Bind) where
  patternRows' (PVarF r) = (Just r,) . TRowVar <$> freshRowTv

instance PatternRows PRecEmptyF where
  patternRows' PRecEmptyF = return (Nothing, TRowEmpty)

instance PatternRows PWildF where
  patternRows' PWildF = (Nothing,) . TRowVar <$> freshRowTv
