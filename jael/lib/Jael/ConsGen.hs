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

type Template = QScheme

-- Use Fixpoint's Reftable?
class Reftable a where
  reft :: a -> F.Reft

instance Reftable Template where
  reft (Scheme _ _ (r :< _)) = r

instance Reftable F.Reft where
  reft = id

class Sortable a where
  toSort :: a -> F.Sort

instance Sortable Template where
  toSort t = foldr F.FAbs (mkSort t) (M.elems tvMap)

    where mkSort :: Template -> F.Sort
          mkSort (Scheme{..}) =
            cata alg (apply (M.map removeAnn schIns) schType)

          alg :: C.CofreeF TypeF F.Reft F.Sort -> F.Sort
          alg (_ C.:< TVarF (Token n _)) =
            F.FVar $ M.findWithDefault
              (error $ "unbound " ++ show n ++ " in " ++ show t)
              n tvMap
          alg (_ C.:< TFunF _ s1 s2) = F.FFunc s1 s2

          alg (_ C.:< TTupF ss)    =
            F.fAppTC (F.symbolFTycon . F.dummyLoc . F.symbol $
                      "Tup" ++ show (length ss)) ss

          alg (_ C.:< TConF n ss) =
            if value n == "Int"
              then F.FInt
              else F.fAppTC
                     (F.symbolFTycon . F.dummyLoc . F.symbol . value $ n) ss

          tvMap :: M.Map T.Text Int
          tvMap = M.fromList $ zip (S.toList $ schGens t) [0..]

class SortedReftable a where
  toSReft :: a -> F.SortedReft

instance SortedReftable Template where
  toSReft t@(Scheme _ _ (r :< _)) = F.RR (toSort t) r

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
--            []
             (map (\q -> F.mkQual
                 (F.symbol ("CmpZ" :: String))
                 [(F.symbol ("v"::String), F.FTC $ F.symbolFTycon "a")]
                 (F.PAtom q (F.eVar ("v"::String)) (F.ECon $ F.I 0) )
                 (F.dummyPos "")
               ) [F.Gt, F.Ge, F.Lt, F.Le, F.Eq, F.Ne]
          ++ map (\q -> F.mkQual
                 (F.symbol ("Cmp" :: String))
                 [ (F.symbol ("v"::String), F.FTC $ F.symbolFTycon "a")
                 , (F.symbol ("x"::String), F.FTC $ F.symbolFTycon "a")
                 ]
                 (F.PAtom q (F.eVar ("v"::String)) (F.eVar ("x"::String)))
                 (F.dummyPos "")
               ) [F.Gt, F.Ge, F.Lt, F.Le, F.Eq, F.Ne]
--          ++ map (\q -> F.mkQual
--                 (F.symbol ("Cmp2" :: String))
--                 [ (F.symbol ("x"::String), F.FTC $ F.symbolFTycon "a")
--                 , (F.symbol ("v"::String), F.FTC $ F.symbolFTycon "a")
--                 ]
--                 (F.PAtom q (F.eVar ("v"::String)) (F.eVar ("x"::String)))
--                 (F.dummyPos "")
--               ) [F.Gt, F.Ge, F.Lt, F.Le, F.Eq, F.Ne]
--          ++ map (\q -> F.mkQual
--                 (F.symbol ("Bot" :: String))
--                 [ (F.symbol ("v"::String), q)
--                 ]
--                 (F.PAtom F.Eq (F.ECon $ F.I 0) (F.ECon $ F.I 1))
--                 (F.dummyPos "")
--               ) [F.FVar 0, F.FObj "obj", F.FTC $ F.symbolFTycon "a", F.boolSort, F.intSort]
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

  F.Result res sol _ <- F.solve cfg fi
  putStrLn $ show res
  putStrLn $ show sol ++ "\n"
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
runConsGen te = case runExcept $ runRWST (act te) emptyEnv initState of
  Left e -> error $ T.unpack e
  Right (a, b, c) -> (a, cgGBinds b, c)
  where act e = do
          e' <- sequence $ fmap instTmplt e
          consGen e'

consGen :: TypedExpr -> CG Template

consGen (qt :< EVarF (Token n _)) = do
  mkWfC [qt]

  t <- tmpltOf n

  traceM $ "qt:\n\t" ++ show qt ++ "\nt:\n\t" ++ show t
--  e <- asks cgLBinds
--  traceM $ "consGen EVar, looking up " ++ show n ++ "which is:\n\t" ++ show t ++"\nwith environment" ++ show (F.toFix e)
  let t' = t `strengthen` F.symbolReft n
  mkSubC [(t', qt)]
  return t'

consGen (qt :< EAbsF n e) = do
  mkWfC [qt]

  tmplt <- (freshTmplt $ shape qt)
  mkWfC [tmplt]
  mkSubC [(tmplt, qt)]

  r <- case tmpltize tmplt of
    (_, TFunF b t1 t2) -> do
      assert (n == b) return ()
      inEnv (b, t1) $ do
        tmplt' <- consGen e
        mkSubC [(tmplt', t2)]
      return tmplt
    _ -> error "Abstraction doesn't have function type."
  return r

consGen (qt :< EAppF e1 e2) = do
  mkWfC [qt]

  tmplt1 <- consGen e1
  tmplt2 <- consGen e2

  case tmpltize tmplt1 of
    (_, TFunF b t1 t2) -> do
      mkSubC [(tmplt2, t1)]
      mkSubC [(t2, qt)]
      return $ subExpr (b, e2) t2
      --return $ pendingSub (b, e2) t2 `strengthen` qt
    _ -> error "First expression in an application must have a function type"

consGen (qt :< EConF (CInt (Token i _))) = do
  mkWfC [qt]

  let rt = JC.intConst i
  mkSubC [(rt, qt)]
  return rt

consGen (qt :< EConF CAdd) = do
  mkWfC [qt]

  let rt = JC.add
  mkSubC [(rt, qt)]
  return rt

consGen (qt :< ELetF x e1 e2) = do
  mkWfC [qt]

  f <- freshTmplt $ shape qt
  mkWfC [f]
  mkSubC [(f, qt)]

  f1 <- consGen e1
  inEnv (x, f1) $ do
    f2 <- consGen e2
    mkSubC [(f2, f)]

  return f

-- TODO: This needs a better name.
tmpltize :: Template -> (F.Reft, TypeF Template)
tmpltize (Scheme gen ins t) =
  let (r :< ft) = t
  in  (r, fmap (Scheme gen ins) ft)

subExpr :: F.Expression a => (Ident, a) -> Template -> Template
subExpr (n, e) = fmap $ fmap (`F.subst1` (F.symbol n, F.expr e))

instance F.Expression TypedExpr where
  expr (_ :< EVarF n) = F.EVar $ F.symbol n

  expr (_ :< EConF CUnit) = F.ECon $ F.L "void" (toSort JC.unit)
  expr (_ :< EConF (CBool b)) = if b then F.PTrue else F.PFalse
  expr (_ :< EConF (CInt (Token i _))) = F.ECon (F.I i)
  expr (_ :< ELetF x e1 e2) = F.expr e2 `F.subst1` (F.symbol x, F.expr e1)
  expr (_ :< EAppF e1 e2) = F.EApp (F.expr e1) (F.expr e2)

mkWfC :: [Template] -> CG ()
mkWfC tmplts = do
  ws <- liftM concat $ mapM splitWs tmplts
  tell $ CGCons { wfcs = ws, subcs = [], kuts = mempty }

splitWs :: Template -> CG [F.WfC ()]
splitWs (Scheme gen ins qt) = para alg (fmap (Scheme gen ins) . duplicate $ qt)
  where alg :: C.CofreeF TypeF (Template)
               (Cofree TypeF Template, CG [F.WfC ()])
                                   -> (CG [F.WfC ()])

        alg (t C.:< TFunF b (t1 :< _, w1) (_, w2)) = do
          w0 <- bsplitW t
          w1' <- w1
          w2' <- inEnv (b, t1) w2
          return $ w0 ++ w1' ++ w2'

        alg (t C.:< TTupF ts) = do
          w0 <- bsplitW t
          ws <- mapM snd ts
          return $ w0 ++ concat ws

        alg (t C.:< TConF _ ts) = do
          w0 <- bsplitW t
          ws <- mapM snd ts
          return $ w0 ++ concat ws

        alg (t C.:< TVarF _) = bsplitW t

bsplitW :: Template -> CG [F.WfC ()]
bsplitW t = do
  bindEnv <- asks cgLBinds
  return $ F.wfC bindEnv (toSReft t) ()

mkSubC :: [(Template, Template)] -> CG ()
mkSubC tmplts = do
  cs <- liftM concat $ mapM splitSs tmplts
  tell $ CGCons { wfcs = [], subcs = cs, kuts = mempty }

splitSs :: (Template, Template) -> CG [F.SubC ()]
splitSs (f1, f2) = do
  cs0 <- bsplitS (f1, f2)
  cs1 <- splitSs' (tmpltize f1) (tmpltize f2)
  return $ cs0 ++ cs1

splitSs' :: (F.Reft, TypeF Template)
         -> (F.Reft, TypeF Template)
         -> CG [F.SubC ()]

splitSs' f1@(_, TFunF b1 t1 t1') f2@(_, TFunF b2 t2 t2') = do
  traceM $ "splitSs' TFunF:\nf1:\n\t" ++ show f1 ++ "\nf2:\n\t" ++ show f2
--  let b1_b2 = (F.symbol b1, F.eVar b2)
--  cs0 <- bsplitS (r1 `F.subst1` b1_b2 :< TFunF b1 t1 t1', f2)

  cs1 <- splitSs (t2, t1)

  cs2 <- inEnv (b2, t2) $ do
    let t1'' = subExpr (b1, F.eVar b2) t1'
    splitSs (t1'', t2')
  return $ cs1 ++ cs2

splitSs' (_, TConF n1 t1s) (_, TConF n2 t2s) = do
  assert (value n1 == value n2) return ()
  liftM concat $ mapM splitSs (zip t1s t2s)

splitSs' (_, TVarF n1) (_, TVarF n2) = do
  assert (value n1 == value n2) return []

splitSs' x y = do
  traceM $ "Unhandled splitSs case with\nt1:\n\t"
    ++ show x ++ "\nt2:\n\t" ++ show y
  return []

bsplitS :: (Template, Template) -> CG [F.SubC ()]
bsplitS (t1, t2) = do
  env <- asks cgLBinds
  let sr1 = toSReft t1
  let sr2 = toSReft t2
  return $ F.subC env sr1 sr2 Nothing [0] ()

tmpltOf :: T.Text -> CG Template
tmpltOf n = fromMaybe
              (error "At this point HM inference was successful so we should \
                     \always have what we lookup in our environment")
              <$> (M.lookup n <$> asks cgSymMap)

freshTmplt :: TScheme -> CG Template
freshTmplt (Scheme gen ins t) = do
  assert (ins == M.empty) return ()
  liftM (Scheme gen M.empty) (freshQType t)

instTmplt :: Template -> CG Template
instTmplt (Scheme gen ins t) = do
  assert (S.empty == S.intersection gen (S.fromList $ M.keys ins)) return ()
  let ins' = (M.map (freshQType . removeAnn) ins)
  liftM (Scheme gen M.empty) (subQType ins' t)

subQType :: M.Map T.Text (CG QType) -> QType -> CG QType
subQType su = cata alg
  where
    alg :: C.CofreeF TypeF F.Reft (CG QType) -> CG QType
    alg t@(varRef C.:< TVarF n) = do
      let cgQ = M.findWithDefault (return $ varRef :< TVarF n) (value n) su
      (subRef :< qt) <- cgQ
      return (varRef <> subRef :< qt)
    alg qt = do
      (r C.:< t) <- sequence qt
      return $ r :< t

freshQType :: Type -> CG QType
freshQType = cata alg
  where alg :: TypeF (CG QType) -> CG QType
        alg (TVarF n)  = liftM (:< TVarF n) freshKVar
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

strengthen :: Reftable a => Template -> a -> Template
strengthen (Scheme g i (r :< fq)) a = Scheme g i $ (r <> reft a) :< fq

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
