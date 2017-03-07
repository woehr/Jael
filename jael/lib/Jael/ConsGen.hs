{-# Language NoImplicitPrelude #-}

module Jael.ConsGen where

import           Jael.Prelude

import qualified Control.Comonad.Trans.Cofree as C
import qualified Data.HashMap.Strict as H
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Language.Fixpoint.Types as F

--import           Jael.Constants (intConst, add)
import           Jael.Types hiding (removeAnn)
import           Jael.Util

type Template = QScheme
--type Template = QType

data CGEnv = CGEnv
  { cgTmplts :: M.Map T.Text Template
  , cgGuards :: [F.Expr]
  } deriving (Show)

data CGCons a = CGCons
  { wfs :: [F.WfC a]
  , subs :: [F.SubC a]
  } deriving (Show)

instance Monoid (CGCons a) where
  mempty = CGCons [] []
  mappend (CGCons a b) (CGCons a' b') = CGCons (a <> a') (b <> b')

data CGState = CGState
  { kIndex :: Integer
  } deriving (Show)

initState :: CGState
initState = CGState 0

type CG = RWST CGEnv (CGCons ()) CGState (Except T.Text)

consGen :: TypedExpr -> CG Template

consGen ((r :< TInsF vs t) :< e) =
  assert (r == F.trueReft) $ consGenIns vs (t :< e)

consGen ((r :< TGenF vs t) :< e) =
  assert (r == F.trueReft) $ consGenGen vs (t :< e)

--consGen (qt :< EVarF (Token n _)) = do
--  (Scheme vs t) <- tmpltOf n
--  return $ Scheme vs $ t `strengthen` F.symbolReft n `strengthenQ` qt

--consGen (qt :< EConF (CInt (Token i _))) =
--  return $ (fmap liftQType $ intConst i) `strengthenT` liftQType qt
--consGen (qt :< EConF CAdd) =
--  return $ add `strengthenT` qt

--consGen _  = undefined

consGenGen :: [T.Text] -> TypedExpr -> CG Template
consGenGen vs te = do
  assert (not . null $ vs) $ return ()
  (Scheme vs' t) <- consGen te
  assert (S.fromList vs `S.intersection` S.fromList vs' == S.empty) $ return ()
  return $ Scheme (vs ++ vs') t

consGenIns :: [(T.Text, QType)] -> TypedExpr -> CG Template
consGenIns ins te = do
  let (as, ts) = unzip ins
  fs <- mapM (freshTmplt . shape) ts
  (Scheme as' qt) <- consGen te
  assert (not . null $ as') $ return ()
  assert (S.fromList as == S.fromList as') $ return ()
  tell $ mkWfC fs
  return $ Scheme [] $ subQType (M.fromList $ zip as $ map tmpltType fs) qt

subQType :: M.Map T.Text QType -> QType -> QType
subQType s = cata alg
  where alg :: C.CofreeF TypeF F.Reft QType -> QType
        alg t@(r C.:< TVarF n) =
          M.findWithDefault (embed t) (value n) s `strengthenQ` r
        alg t = embed t

mkWfC :: [Template] -> CGCons ()
mkWfC tmplts =
  let bindEnv = undefined
      tmplts' = concat $ map (\x -> F.wfC bindEnv (srOf x) ()) tmplts
  in  CGCons { wfs = tmplts', subs = [] }
  where srOf :: Template -> F.SortedReft
        srOf = undefined

tmpltType :: Template -> QType
tmpltType (Scheme _ t) = t

tmpltOf :: T.Text -> CG Template
tmpltOf n = fromMaybe
              (error "At this point HM inference was successful so we should \
                     \always have what we lookup in our environment")
              <$> (M.lookup n <$> asks cgTmplts)

--instTmplt :: Template -> (T.Text, QType) -> Template
--instTmplt = undefined

freshTmplt :: Type -> CG Template
freshTmplt t = do
  q <- cata alg t
  return $ Scheme [] q
  where alg :: TypeF (CG QType) -> CG QType
        alg (TVarF _) = error "Can we have vars?"
        alg (TGenF _ _) = error "Can we have gen here?"
        alg (TInsF _ _) = error "Can we have ins here?"
        alg (TFunF q1 q2) = liftM2 (:<) freshKVar (liftM2 TFunF q1 q2)
        alg (TConF n qs) = liftM2 (:<) freshKVar (liftM (TConF n) $ sequence qs)
        alg (TTupF qs) = liftM2 (:<) freshKVar (liftM TTupF $ sequence qs)

freshKVar :: CG F.Reft
freshKVar = do
  k <- gets kIndex
  modify (\(s@CGState{..}) -> s{kIndex = k+1})
  return $ F.reft F.vv_ $ F.PKVar (F.intKvar k) (F.Su H.empty)

strengthenQ :: QType -> F.Reft -> QType
strengthenQ (r :< fq) r' = (r <> r') :< fq

--strengthenTmplt :: Template -> TType -> Template
--strengthenTmplt (Scheme xs t1) t2 = Scheme xs $ strengthenT t1 t2

--strengthenT :: TType -> TType -> TType

--strengthenT (q1s :< TFunF l1 r1) (q2s :< TFunF l2 r2) =
--  q1s <> q2s :< TFunF (strengthenT l1 l2) (strengthenT r1 r2)

--strengthenT (q1s :< TTupF t1s) (q2s :< TTupF t2s) =
--  q1s <> q2s :< TTupF (map (uncurry strengthenT) (zip t1s t2s))

--strengthenT (q1 :< TConF n1 t1) (q2 :< TConF n2 t2) =
--  assert (n1 == n2) $
--  q1 <> q2 :< TConF n1 (map (uncurry strengthenT) (zip t1 t2))

--strengthenT (q1 :< TVarF n1) (q2 :< TVarF n2) =
--  assert (n1 == n2) $ q1 <> q2 :< TVarF n1

--strengthenT _ _ = error "Shapes of types don't match"
