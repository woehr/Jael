{-# Language NoImplicitPrelude #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
{-# Language RecordWildCards #-}
{-# Language OverloadedStrings #-}

module Jael.ConsGen where

import           Jael.Prelude

import qualified Control.Comonad.Trans.Cofree as C
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import qualified Language.Fixpoint.Types.Config as F
import qualified Language.Fixpoint.Solver as F
import qualified Language.Fixpoint.Types as F

import           Jael.Classes
import qualified Jael.Constants as JC
import           Jael.Types
import           Jael.Util

type Template = QType

-- Use Fixpoint's Reftable?
class Reftable a where
  reft :: a -> F.Reft

instance Reftable Template where
  reft (r :< _) = r

instance Reftable F.Reft where
  reft = id

class Sortable a where
  toSort :: a -> F.Sort

instance Sortable Template where
  toSort t = let t' = cata applyIns t
                 srt = cata mkSort t'
             in  foldr F.FAbs srt (M.elems tvMap)
    where mkSort :: C.CofreeF TypeF F.Reft F.Sort -> F.Sort
          mkSort (_ C.:< TVarF (Token n _)) = F.FVar $ M.findWithDefault errMsg n tvMap
          mkSort (_ C.:< TGenF _ s) = s
          mkSort (_ C.:< TInsF _ s) = s
          mkSort (_ C.:< TFunF _ s1 s2) = F.FFunc s1 s2

          mkSort (_ C.:< TTupF ss)    =
            F.fAppTC (F.symbolFTycon . F.dummyLoc . F.symbol $
                      "Tup" ++ show (length ss)) ss

          mkSort (_ C.:< TConF n ss) =
            if value n == "Int"
              then F.FInt
              else F.fAppTC (F.symbolFTycon . F.dummyLoc . F.symbol . value $ n)
                            ss

          applyIns :: C.CofreeF TypeF F.Reft Template -> Template
          applyIns (r C.:< TInsF su q) =
            assert (r == F.trueReft) subQType (M.fromList su) q
          applyIns q = embed q

          tvMap :: M.Map T.Text Int
          tvMap = M.fromList $ zip (S.toList $ ftv t) [0..]

          errMsg = error "Unbound type variable"

class SortedReftable a where
  toSReft :: a -> F.SortedReft

instance SortedReftable Template where
  toSReft t@(r :< _) = F.RR (toSort t) r

--type Solver a = Config -> FInfo a -> IO (Result (Integer, a))

--fi :: [SubC a]
--   -> [WfC a]
--   -> BindEnv
--   -> SEnv Sort
--   -> SEnv Sort
--   -> Kuts
--   -> [Qualifier]
--   -> M.HashMap BindId a
--   -> Bool
--   -> Bool
--   -> [Triggered Expr]
--   -> GInfo SubC a

solve :: TypedExpr -> IO ()
solve te = do
  let (_, env, cs) = runConsGen te

  let fi = F.fi
             (subcs cs)
             (wfcs cs)
             env
             F.emptySEnv
             F.emptySEnv
             (kuts cs)
             (map (\q -> F.mkQual
                 (F.symbol ("CmpZ" :: String))
                 [(F.symbol ("v"::String), F.FTC $ F.symbolFTycon "a")]
                 (F.PAtom q (F.eVar ("v"::String)) (F.ECon $ F.I 0) )
                 (F.dummyPos "")
               ) [F.Gt, F.Ge, F.Lt, F.Le, F.Eq, F.Ne] ++
             map (\q -> F.mkQual
                 (F.symbol ("Cmp" :: String))
                 [ (F.symbol ("v"::String), F.FTC $ F.symbolFTycon "a")
                 , (F.symbol ("x"::String), F.FTC $ F.symbolFTycon "a")
                 ]
                 (F.PAtom q (F.eVar ("v"::String)) (F.eVar ("x"::String)))
                 (F.dummyPos "")
               ) [F.Gt, F.Ge, F.Lt, F.Le, F.Eq, F.Ne] ++
             map (\q -> F.mkQual
                 (F.symbol ("Bot" :: String))
                 [ (F.symbol ("v"::String), q)
                 ]
                 (F.PAtom F.Eq (F.ECon $ F.I 0) (F.ECon $ F.I 1))
                 (F.dummyPos "")
               )
                 [F.FVar 0, F.FObj "obj", F.FTC $ F.symbolFTycon "a", F.boolSort, F.intSort]
             )
             mempty
             False
             False
             []

  let cfg = F.defConfig
        { F.save = True
--        , F.stats = True
        , F.eliminate = F.None
        , F.elimStats = True
--        , F.parts = True
--        , F.metadata = True
        }

  F.writeFInfo cfg fi "fi.out"

  F.Result res sol <- F.solve cfg fi
  putStrLn $ show sol ++ "\n"
  putStrLn $ show res
  return ()

data CGEnv = CGEnv
  { cgSymMap :: M.Map T.Text Template
  , cgGuards :: [F.Expr]
  , cgLBinds :: F.IBindEnv
  }

emptyEnv :: CGEnv
emptyEnv = CGEnv
  { cgSymMap = M.empty
  , cgGuards = []
  , cgLBinds = F.emptyIBindEnv
  }

data CGCons a = CGCons
  { wfcs  :: [F.WfC a]
  , subcs :: [F.SubC a]
  , kuts  :: F.Kuts
  } deriving (Show)

instance Monoid (CGCons a) where
  mempty = CGCons [] [] mempty
  mappend (CGCons a b c) (CGCons a' b' c') =
    CGCons (a <> a') (b <> b') (c <> c')

data CGState = CGState
  { kIndex   :: Integer
  , cgGBinds :: F.BindEnv
  }

initState :: CGState
initState = CGState
  { kIndex   = 0
  , cgGBinds = F.emptyBindEnv
  }

type CG = RWST CGEnv (CGCons ()) CGState (Except T.Text)

runConsGen :: TypedExpr -> (Template, F.BindEnv, CGCons ())
runConsGen t = case runExcept $ runRWST (consGen t) emptyEnv initState of
  Left e -> error $ T.unpack e
  Right (a, b, c) -> (a, cgGBinds b, c)

consGen :: TypedExpr -> CG Template

consGen ((r :< TInsF vs t) :< e) =
  assert (r == F.trueReft) consGenIns vs (t :< e)

consGen ((r :< TGenF vs t) :< e) = do
  assert (r == F.trueReft) return ()
  t' <- consGen (t :< e)
  return $ if null vs
             then t'
             else r :< TGenF vs t'

consGen (qt :< EVarF (Token n _)) = do
  t <- tmpltOf n
  return $ t `strengthen` F.symbolReft n `strengthen` qt

consGen (qt :< EAbsF n e) = do
  traceM $ "qt of EAbs=" ++ show qt
  tmplt <- liftM (`strengthen` qt) (freshTmplt $ shape qt)
  mkWfC [tmplt]
  r <- case tmplt of
    (_ :< TFunF b t1 t2) -> do
--      traceM $ "consGen for EAbs. n=" ++ show n ++ "  b=" ++ show b
      assert (n == b) return ()
      let absBind = (b, t1)
      tmplt' <- inEnv absBind (consGen e)
      inEnv absBind $ mkSubC [(tmplt', t2)]
      return tmplt
    _ -> error "Abstraction doesn't have function type."
  traceM "Leaving consGen EAbs"
  return r

consGen (qt :< EAppF e1 e2) = do
  tmplt1 <- consGen e1
  tmplt2 <- consGen e2

--  traceM $ "In consGen for EAppF (tmplt1): " ++ show tmplt1
--  traceM $ "In consGen for EAppF (tmplt2): " ++ show tmplt2

  case tmplt1 of
    (_ :< TFunF b t1 t2) -> do
      mkSubC [(tmplt2, t1)]
      return $ pendingSub (b, e2) (t2 `strengthen` qt)
      --return $ pendingSub (b, e2) t2 `strengthen` qt
    _ -> error "First expression in an application must have a function type"

consGen (qt :< EConF (CInt (Token i _))) =
  return $ JC.intConst i `strengthen` qt

consGen (qt :< EConF CAdd) =
  return $ JC.add `strengthen` qt

consGen (qt :< ELetF x e1 e2) = do
  f <- freshTmplt $ shape qt
  f1 <- consGen e1
  inEnv (x, f1) $ do
    f2 <- consGen e2
    mkSubC [(f2 `strengthen` qt, f)]
  mkWfC [f]
  return f

pendingSub :: (Ident, TypedExpr) -> Template -> Template
pendingSub (n, e) = fmap $ (`F.subst1` (F.symbol n, toFExpr e))

toFExpr :: TypedExpr -> F.Expr

toFExpr ((_ :< TGenF vs t) :< e) =
--  foldr (flip F.ETAbs) (toFExpr $ t :< e) (map F.symbol vs)
  toFExpr $ t :< e

toFExpr t'@((_ :< TInsF _ t) :< e) =
  toFExpr $ t :< e
--  trace ("toFExpr for TInsF with type" ++ show t) undefined
--  error "Expected consGen to do instantiations"
--  undefined

toFExpr (_ :< EVarF n) = F.EVar $ F.symbol n

toFExpr (_ :< EConF CUnit) = F.ECon $ F.L "void" (toSort JC.unit)
toFExpr (_ :< EConF (CBool b)) = if b then F.PTrue else F.PFalse
toFExpr (_ :< EConF (CInt (Token i _))) = F.ECon (F.I i)
toFExpr (_ :< ELetF x e1 e2) = toFExpr e2 `F.subst1` (F.symbol x, toFExpr e1)
toFExpr (_ :< EAppF e1 e2) = F.EApp (toFExpr e1) (toFExpr e2)
--toFExpr _ = error "unimplemented toFExpr"

--toFExpr _ = undefined

consGenIns :: [(T.Text, QType)] -> TypedExpr -> CG Template
consGenIns ins te = do
--  traceM $ "consGenIns the typed expr: " ++ show te
  let (as, ts) = unzip ins
  fs <- mapM (freshTmplt . shape) ts
  t <- consGen te

  let (as', qt) = case t of
                    (_ :< TGenF x y) -> (x, y)
                    _ -> error "Must be instantiating something generalizable"

  assert (not . null $ as') $ return ()
  assert (S.fromList as == S.fromList as') $ return ()

  mkWfC fs
  let ret = subQType (M.fromList $ zip as fs) qt
--  traceM $ "conGenIns returning: " ++ show ret
  return ret

subQType :: M.Map T.Text QType -> QType -> QType
subQType s = cata alg
  where alg :: C.CofreeF TypeF F.Reft QType -> QType
        alg t@(r C.:< TVarF n) =
          M.findWithDefault (embed t) (value n) s `strengthen` r
        alg t = embed t

mkWfC :: [Template] -> CG ()
mkWfC tmplts = do
  ws <- liftM concat $ mapM splitWs tmplts
  tell $ CGCons { wfcs = ws, subcs = [], kuts = mempty }

splitWs :: Template -> CG [F.WfC ()]
splitWs = para alg
  where alg :: C.CofreeF TypeF F.Reft (Template, CG [F.WfC ()]) -> (CG [F.WfC ()])

        alg t@(_ C.:< TFunF b (t1, w1) (_, w2)) = do
          w0 <- bsplitW t
          w1' <- w1
          w2' <- inEnv (b, t1) w2
          return $ w0 ++ w1' ++ w2'

        alg t@(_ C.:< TTupF ts) = do
          w0 <- bsplitW t
          ws <- mapM snd ts
          return $ w0 ++ concat ws

        alg t@(_ C.:< TConF _ ts) = do
          w0 <- bsplitW t
          ws <- mapM snd ts
          return $ w0 ++ concat ws

        alg (_ C.:< TInsF _ _) = error "Shouldn't happen because Ins's get applied"

        alg (_ C.:< TGenF _ (_, x)) = x

        alg t@(_ C.:< TVarF _) = bsplitW t

bsplitW :: C.CofreeF TypeF F.Reft (Template, CG [F.WfC ()]) -> (CG [F.WfC ()])
bsplitW (r C.:< t) = do
  let sr = toSReft (r :< fmap fst t)
  bindEnv <- asks cgLBinds
  return $ F.wfC bindEnv sr ()

mkSubC :: [(Template, Template)] -> CG ()
mkSubC tmplts = do
  cs <- liftM concat $ mapM splitSs tmplts
  tell $ CGCons { wfcs = [], subcs = cs, kuts = mempty }

splitSs :: (Template, Template) -> CG [F.SubC ()]

splitSs (f1@(_ :< TFunF b1 t1 t1'), f2@(_ :< TFunF b2 t2 t2')) = do
--  let b1_b2 = (F.symbol b1, F.eVar b2)
--  cs0 <- bsplitS (r1 `F.subst1` b1_b2 :< TFunF b1 t1 t1', f2)

  cs0 <- bsplitS (f1, f2)
  cs1 <- splitSs (t2, t1)

  cs2 <- inEnv (b2, t2) $ do
    let t1'' = fmap (`F.subst1` (F.symbol b1, F.eVar b2)) t1'
    splitSs (t1'', t2')
  return $ cs0 ++ cs1 ++ cs2

splitSs (t1@(_ :< TConF n1 t1s), t2@(_ :< TConF n2 t2s)) = do
  assert (value n1 == value n2) return ()
  cs0 <- bsplitS (t1, t2)
  cs1 <- liftM concat $ mapM splitSs (zip t1s t2s)
  return $ cs0 ++ cs1

splitSs (t1@(_ :< TVarF n1), t2@(_ :< TVarF n2)) = do
  assert (value n1 == value n2) return ()
  bsplitS (t1, t2)

splitSs (x, y) = do
  traceM $ "Unhandled splitSs case with\nt1:\n\t" ++ show x ++ "\nt2:\n\t" ++ show y
  return []

bsplitS :: (Template, Template) -> CG [F.SubC ()]
bsplitS (t1, t2) = do
  env <- asks cgLBinds
  let sr1 = toSReft t1
  let sr2 = toSReft t2
--  traceM $ "bsplitS: sr1=" ++ show sr1 ++ "\n         sr2=" ++ show sr2 ++ "\n"
  return $ F.subC env sr1 sr2 Nothing [0] ()

tmpltOf :: T.Text -> CG Template
tmpltOf n = fromMaybe
              (error "At this point HM inference was successful so we should \
                     \always have what we lookup in our environment")
              <$> (M.lookup n <$> asks cgSymMap)

freshTmplt :: Type -> CG Template
freshTmplt t = cata alg t
  where alg :: TypeF (CG QType) -> CG QType
        alg (TVarF n)  = liftM (:< TVarF n) freshKVar
        alg (TGenF _ _) = error "Can we have gen here?"
        alg (TInsF _ _) = error "Can we have ins here?"
        alg (TFunF b q1 q2) = liftM2 (:<) freshKVar (liftM2 (TFunF b) q1 q2)
        alg (TConF n qs)    = liftM2 (:<) freshKVar (liftM (TConF n) $ sequence qs)
        alg (TTupF qs)      = liftM2 (:<) freshKVar (liftM  TTupF    $ sequence qs)

freshKVar :: CG F.Reft
freshKVar = do
  k <- gets kIndex
  modify (\(s@CGState{..}) -> s{kIndex = k+1})
  let kv = F.intKvar k
  tell $ CGCons [] [] (F.KS $ HS.singleton kv)
  return $ F.reft F.vv_ $ F.PKVar kv (F.Su HM.empty)

strengthen :: (Reftable a, Show a) => QType -> a -> QType
strengthen t@(r :< fq) a =
  trace ("Adding reft " ++ show a ++ "\n\nto " ++ show t ++ "\n\n")
  (r <> reft a) :< fq

inEnv :: (Ident, Template) -> CG a -> CG a
inEnv (t, tmplt) x = do
  let (s, sr) = (F.symbol t, toSReft tmplt)
  be  <- gets cgGBinds
  ibe <- asks cgLBinds
  sbe <- asks cgSymMap
  let (bid, be') = F.insertBindEnv s sr be
  let ibe' = F.insertsIBindEnv [bid] ibe
  let sbe' = M.insert (value t) tmplt sbe
  modify (\(e@CGState{..}) -> e { cgGBinds = be' })
  local  (\(e@CGEnv{..})   -> e { cgLBinds = ibe', cgSymMap = sbe' }) x
