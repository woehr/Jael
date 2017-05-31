{-# Language FlexibleInstances #-}
{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
{-# Language TupleSections #-}
{-# Language TypeSynonymInstances #-}

module Jael.ConsGen where

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
import           Jael.Constants
import           Jael.Types
import           Jael.Util

-- Use Fixpoint's Reftable?
class Reftable a where
  reft :: a -> F.Reft

instance Reftable Template where
  reft (Scheme _ _ (r :< _)) = r

instance Reftable F.Reft where
  reft = id

class Sortable a where
  toSort :: a -> F.Sort

instance Sortable QType where
  toSort t = toSort (Scheme S.empty M.empty $ removeAnn t)

instance Sortable Template where
  toSort (Scheme gs is t) = toSort (Scheme gs (M.map removeAnn is) (removeAnn t))

instance Sortable QScheme where
  toSort (Scheme gs is t) = toSort (Scheme gs (M.map removeAnn is) (removeAnn t))

instance Sortable TScheme where
  toSort t = foldr F.FAbs (mkSort t) (M.elems tvMap)

    where mkSort :: TScheme -> F.Sort
          mkSort Scheme{..} =
            cata alg (apply schIns schType)

          alg :: TypeF F.Sort -> F.Sort
          alg (TVarF (Token n _)) =
            F.FVar $ M.findWithDefault
              (error $ "unbound " ++ show n ++ " in " ++ show t)
              n tvMap
          alg (TFunF _ s1 s2) = F.FFunc s1 s2

          alg (TTupF ss)    =
            F.fAppTC (F.symbolFTycon . F.dummyLoc . F.symbol $
                      "Tup" ++ show (length ss)) ss

          alg (TConF n ss)
            | value n == "Int"
            = F.intSort
            | value n == "Bool"
            = F.boolSort
            | otherwise =F.fAppTC
                (F.symbolFTycon . F.dummyLoc . F.symbol . value $ n) ss

          tvMap :: M.Map T.Text Int
          tvMap = M.fromList $ zip (S.toList $ schGens t) [0..]

instance Sortable Constant where
  toSort = toSort . constantScheme

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

solve :: (HMTypedExpr, MaybeTypedExpr) -> IO (F.FixResult (Integer, ()), F.FixSolution)
solve te = do
  let (_, env, cs, bindMap) = runConsGen te

  let fi = F.fi
             (subcs cs)
             (wfcs cs)
             env
             F.emptySEnv
             F.emptySEnv
             (kuts cs)
             (
--             map (\q -> F.mkQual
--                 (F.symbol ("CmpZ" :: String))
--                 [(F.symbol ("v"::String), F.FTC $ F.symbolFTycon "a")]
--                 (F.PAtom q (F.eVar ("v"::String)) (F.ECon $ F.I 0) )
--                 (F.dummyPos "")
--               ) [F.Gt, F.Ge, F.Lt, F.Le, F.Eq, F.Ne]
--          ++ map (\q -> F.mkQual
--                 (F.symbol ("Cmp" :: String))
--                 [ (F.symbol ("v"::String), F.FTC $ F.symbolFTycon "a")
--                 , (F.symbol ("x"::String), F.FTC $ F.symbolFTycon "a")
--                 ]
--                 (F.PAtom q (F.eVar ("v"::String)) (F.eVar ("x"::String)))
--                 (F.dummyPos "")
--               ) [F.Gt, F.Ge, F.Lt, F.Le, F.Eq, F.Ne]
             map (\q -> F.mkQual
                 (F.symbol ("CmpZ" :: String))
                 [(F.symbol ("v"::String), F.FVar 0)]
                 (F.PAtom q (F.eVar ("v"::String)) (F.ECon $ F.I 0) )
                 (F.dummyPos "")
               ) [F.Gt, F.Ge, F.Lt, F.Le, F.Eq, F.Ne]
          ++ map (\q -> F.mkQual
                 (F.symbol ("Cmp" :: String))
                 [ (F.symbol ("v"::String), F.FVar 0)
                 , (F.symbol ("x"::String), F.FVar 0)
                 ]
                 (F.PAtom q (F.eVar ("v"::String)) (F.eVar ("x"::String)))
                 (F.dummyPos "")
               ) [F.Gt, F.Ge, F.Lt, F.Le, F.Eq, F.Ne]
             )
             bindMap
             False
             False
             []

  let cfg = F.defConfig
        { F.save = True
--        , F.stats = True
--        , F.eliminate = F.None
--        , F.elimStats = True
--        , F.parts = True
--        , F.metadata = True
        }

--  F.writeFInfo cfg fi "fi.out"

  F.Result res sol _ <- F.solve cfg fi
  print res
  putStrLn $ show sol ++ "\n"
  return (res, sol)

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
  , cgBindInfo :: F.BindMap ()
  }

initState :: CGState
initState = CGState
  { kIndex   = 0
  , cgGBinds = F.emptyBindEnv
  , cgBindInfo = HM.empty
  }

type CG = RWST CGEnv (CGCons ()) CGState (Except T.Text)

runConsGen :: (HMTypedExpr, MaybeTypedExpr)
           -> (Template, F.BindEnv, CGCons (), F.BindMap ())
runConsGen (hte, mte) = case runExcept $ runRWST act emptyEnv initState of
  Left e -> error $ T.unpack e
  Right (a, b, c) -> (a, cgGBinds b, c, cgBindInfo b)

  where
    act :: CG Template
    act = consGen te'

    te' :: Cofree ExprF (TScheme, [QType])
    te' = joinExprs (,) hte mte

-- -- Use on the inferred type to get an hm type and a template
-- -- specifying additional constraints which are used in consGen
-- freshFromQScheme :: QScheme -> CG (TScheme, Template)
-- freshFromQScheme qs@(Scheme gs is qt) = do
--   -- should hit one day
--   assert (null is) return ()
--   -- The uninstantiated hm type
--   let ts = Scheme gs (M.map removeAnn is) (removeAnn qt)
--   -- Uninstantiated constraint template
--   ct <- qschemeToTemplate qs
--   -- Instantiated constraint template
--   ct' <- instTmplt ct
--   return (ts, ct')

-- Replace Nothings with a fresh KVar to get an RType
nothingToKvar :: QType -> CG RType
nothingToKvar = sequence . fmap (maybe freshKVar return)

-- Essentially, the function nothingToKvar over a QScheme
-- No instantiation, no well-formedness constraints
-- Use if you need to turn a QScheme into a Template for instantiation
-- (on constants' types or inferred types)
qschemeToTemplate :: QScheme -> CG Template
qschemeToTemplate (Scheme gs is qt) = do
  rtype <- nothingToKvar qt
  -- QType becomes RType, nothings become fresh kvars
  is' <- sequence $ M.map nothingToKvar is
  return $ Scheme gs is' rtype

-- Make well-formedness constraints and substitute instantiations
instTmplt :: Template -> CG Template
instTmplt (Scheme gs is t) = do
  assert (S.empty == S.intersection gs (S.fromList $ M.keys is)) return ()
  mkWfCsFromMap $ M.map (Scheme gs M.empty) is
--  mkWfC [Scheme gs is t]
  return $ Scheme gs M.empty (subRType is t)

-- One day RType should be an instance of TIOps
subRType :: M.Map T.Text RType -> RType -> RType
subRType su = cata alg
  where
    alg :: C.CofreeF TypeF F.Reft RType -> RType
    alg (varRef C.:< TVarF n) = do
      let subRef :< qt = M.findWithDefault (varRef :< TVarF n) (value n) su
      varRef <> subRef :< qt
    alg (r C.:< t) = r :< t

-------------------------------------------------------------------------------

consGenEpilogue :: Template -> [QType] -> CG Template
consGenEpilogue tmplt qs = do
  tmplt' <- instTmplt tmplt
  qs' <- mapM (\q -> qschemeToTemplate $ Scheme (ftv q) M.empty q) qs
  mkSubC $ map (tmplt',) qs'
  return tmplt'

-- consGen takes as an input an expression annotated with a plain
-- *uninstantiated* type and an instantiated template. The first should be used
-- as the type determined by standard Hindley-Milner type inference. The second
-- represents additional constraints on the the particular expression.
consGen :: Cofree ExprF (TScheme, [QType]) -> CG Template
consGen ((_, qs) :< EVarF (Token n _)) = do
  traceM "\nconsGen: Var"

  t <- tmpltOf n

  let t' = t `strengthen` F.symbolReft n

  consGenEpilogue t' qs

consGen ((hmt, qs) :< EAbsF n e) = do
  traceM "\nconsGen: Abs"
  tmplt <- freshTmplt hmt
  traceM $ "freshTmplt: " ++ show tmplt
  mkWfC [tmplt]

  r <- case tmpltize tmplt of
    (_, TFunF b t1 t2) -> do
      assert (n == b) return ()
      inEnv (b, t1) $ do
        tmplt' <- consGen e
        mkSubC [(tmplt', t2)]
      return tmplt
    _ -> error "Abstraction doesn't have function type."

  consGenEpilogue r qs

consGen ((_, qs) :< EAppF e1 e2) = do
  traceM "\nconsGen: App"

  tmplt1 <- consGen e1
  tmplt2 <- consGen e2

  r <- case tmpltize tmplt1 of
    (_, TFunF b t1 t2) -> do
      mkSubC [(tmplt2, t1)]
      return $ subExpr (b, e2) t2
      --return $ pendingSub (b, e2) t2 `strengthen` qt
    _ -> error "First expression in an application must have a function type"

  consGenEpilogue r qs

consGen ((hmt, qs) :< ELetF x e1 e2) = do
  traceM "\nconsGen: Let"

  f <- freshTmplt hmt
  traceM $ "freshTmplt: " ++ show f

  mkWfC [f]

  f1 <- consGen e1
  inEnv (x, f1) $ do
    f2 <- consGen e2
    mkSubC [(f2, f)]

  consGenEpilogue f qs

consGen ((_, qs) :< EConF c) = do
  traceM "\nconsGen: EConF"

  t <- qschemeToTemplate (constantScheme c)
  consGenEpilogue t qs

consGen x = error $ "Unhandled case for:\n" ++ show x

instance (Show a) => F.Expression (Cofree ExprF a) where
  expr (_ :< EVarF n) = F.EVar $ F.symbol n

  expr (_ :< EConF CUnit) = F.ECon $ F.L "void" (toSort CUnit)
  expr (_ :< EConF (CBool b)) = if b then F.PTrue else F.PFalse
  expr (_ :< EConF (CInt (Token i _))) = F.ECon (F.I i)
  expr (_ :< ELetF x e1 e2) = F.expr e2 `F.subst1` (F.symbol x, F.expr e1)
  expr (_ :< EAppF (_ :< EConF CNot) e2) = F.PNot $ F.expr e2
  expr (_ :< EAppF e1 e2) = F.EApp (F.expr e1) (F.expr e2)
  expr x = error $ "Unhandled case for:\n" ++ show x

-------------------------------------------------------------------------------

mkWfC :: [Template] -> CG ()
mkWfC qs = do
  ws <- concat <$> mapM splitWs qs
  tell CGCons { wfcs = ws, subcs = [], kuts = mempty }

-- Helper for generating well-formedness constraints on a map of RTypes
mkWfCsFromMap :: M.Map T.Text Template -> CG ()
mkWfCsFromMap = mkWfC . map snd . M.toList

splitWs :: Template -> CG [F.WfC ()]
splitWs (Scheme gen ins qt) = para alg (fmap (Scheme gen ins) . duplicate $ qt)
  where --alg :: C.CofreeF TypeF QScheme
        --       (Cofree TypeF QScheme, CG [F.WfC ()])
        --                          -> (CG [F.WfC ()])

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

-------------------------------------------------------------------------------

mkSubC :: [(Template, Template)] -> CG ()
mkSubC tmplts = do
  cs <- concat <$> mapM splitSs tmplts
  tell CGCons { wfcs = [], subcs = cs, kuts = mempty }

splitSs :: (Template, Template) -> CG [F.SubC ()]
splitSs (f1, f2) = do
  cs0 <- bsplitS (f1, f2)
  cs1 <- splitSs' (tmpltize f1) (tmpltize f2)
  return $ cs0 ++ cs1

splitSs' :: (F.Reft, TypeF Template)
         -> (F.Reft, TypeF Template)
         -> CG [F.SubC ()]

splitSs' f1@(_, TFunF b1 t1 t1') f2@(_, TFunF b2 t2 t2') = do
--  traceM $ "splitSs' TFunF:\nf1:\n\t" ++ show f1 ++ "\nf2:\n\t" ++ show f2
--  let b1_b2 = (F.symbol b1, F.eVar b2)
--  cs0 <- bsplitS (r1 `F.subst1` b1_b2 :< TFunF b1 t1 t1', f2)

  cs1 <- splitSs (t2, t1)

  cs2 <- inEnv (b2, t2) $ do
    let t1'' = subExpr (b1, F.eVar b2) t1'
    splitSs (t1'', t2')

  return $ cs1 ++ cs2

splitSs' (_, TConF n1 t1s) (_, TConF n2 t2s) = do
  assert (value n1 == value n2) return ()
  concat <$> mapM splitSs (zip t1s t2s)

splitSs' (_, TVarF n1) (_, TVarF n2) =
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

-------------------------------------------------------------------------------

-- TODO: This needs a better name.
tmpltize :: Template -> (F.Reft, TypeF Template)
tmpltize (Scheme gen ins t) =
  let (r :< ft) = t
  in  (r, fmap (Scheme gen ins) ft)

subExpr :: F.Expression a => (Ident, a) -> Template -> Template
subExpr (n, e) = fmap $ fmap (`F.subst1` (F.symbol n, F.expr e))

-------------------------------------------------------------------------------

tmpltOf :: T.Text -> CG Template
tmpltOf n = do
  env <- asks cgSymMap
  return $
    fromMaybe (error $ "At this point HM inference was successful so we should \
                       \always have what we lookup in our environment\n" ++
                       show env
              )
              (M.lookup n env)

-------------------------------------------------------------------------------

-- Create fresh kvars as the refinements of the input scheme. This includes
-- creating fresh kvars for the types in the instantiation map. No
-- well-formedness are created. For that, use instTmplt.
freshTmplt :: TScheme -> CG Template
freshTmplt (Scheme gs is t) =
  Scheme gs <$> sequence (M.map freshRType is) <*> freshRType t

-- Like freshTmplt but works on types instead of schemes
freshRType :: Type -> CG RType
freshRType = sequence . cata (freshKVar :<)

--  liftM (Scheme gs M.empty) (cata alg t)
--   where alg :: TypeF (CG RType) -> CG RType
--         alg (TVarF n)       = liftM  (:< TVarF n) freshKVar
--         alg (TFunF b q1 q2) = liftM2 (:<) freshKVar (liftM2 (TFunF b) q1 q2)
--         alg (TConF n qs)    = liftM2 (:<) freshKVar (liftM  (TConF n) $ sequence qs)
--         alg (TTupF qs)      = liftM2 (:<) freshKVar (liftM   TTupF    $ sequence qs)

-------------------------------------------------------------------------------

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
  modify (\(e@CGState{..}) -> e { cgGBinds = be'
                                , cgBindInfo = HM.insert bid () cgBindInfo }
         )
  local  (\(e@CGEnv{..})   -> e { cgLBinds = ibe', cgSymMap = sbe' }) x
