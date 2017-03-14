{-# Language NoImplicitPrelude #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
{-# Language RecordWildCards #-}
{-# Language OverloadedStrings #-}

module Jael.ConsGen where

import           Jael.Prelude

import qualified Control.Comonad.Trans.Cofree as C
import qualified Data.HashMap.Strict as H
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
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

data CGEnv = CGEnv
  { cgConsts :: M.Map T.Text Template
  , cgGuards :: [F.Expr]
  , cgLBinds :: F.IBindEnv
  }

emptyEnv :: M.Map T.Text Template -> CGEnv
emptyEnv c = CGEnv
  { cgConsts = c
  , cgGuards = []
  , cgLBinds = F.emptyIBindEnv
  }

data CGCons a = CGCons
  { wfcs  :: [F.WfC a]
  , subcs :: [F.SubC a]
  } deriving (Show)

instance Monoid (CGCons a) where
  mempty = CGCons [] []
  mappend (CGCons a b) (CGCons a' b') = CGCons (a <> a') (b <> b')

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

consGen :: TypedExpr -> CG Template

consGen ((r :< TInsF vs t) :< e) =
  assert (r == F.trueReft) consGenIns vs (t :< e)

consGen ((r :< TGenF vs t) :< e) = do
  assert (r == F.trueReft) return ()
  t' <- consGen (t :< e)
  return $ r :< TGenF vs t'

consGen (qt :< EVarF (Token n _)) = do
  t <- tmpltOf n
  return $ t `strengthen` F.symbolReft n `strengthen` qt

consGen (qt :< EAbsF n e) = do
  tmplt <- freshTmplt (shape qt)
  mkWfC [tmplt]
  case tmplt of
    (_ :< TFunF b t1 t2) -> do
      assert (n == b) return ()
      let absBind = (F.symbol b, toSReft t1)
      tmplt' <- inEnv absBind (consGen e)
      inEnv absBind $ mkSubC [(tmplt', t2)]
      return tmplt
    _ -> error "Abstraction doesn't have function type."

--consGen (qt :< EConF (CInt (Token i _))) =
--  return $ intConst i `strengthen` qt

consGen (qt :< EConF CAdd) =
  return $ JC.add `strengthen` qt

consGen _  = undefined

consGenIns :: [(T.Text, QType)] -> TypedExpr -> CG Template
consGenIns ins te = do
  let (as, ts) = unzip ins
  fs <- mapM (freshTmplt . shape) ts
  t <- consGen te

  let (as', qt) = case t of
                    (_ :< TGenF x y) -> (x, y)
                    _ -> error "Must be instantiating something generalizable"

  assert (not . null $ as') $ return ()
  assert (S.fromList as == S.fromList as') $ return ()

  mkWfC fs
  return $ subQType (M.fromList $ zip as fs) qt

subQType :: M.Map T.Text QType -> QType -> QType
subQType s = cata alg
  where alg :: C.CofreeF TypeF F.Reft QType -> QType
        alg t@(r C.:< TVarF n) =
          M.findWithDefault (embed t) (value n) s `strengthen` r
        alg t = embed t

mkWfC :: [Template] -> CG ()
mkWfC tmplts = do
  ws <- liftM concat $ mapM splitWs tmplts
--  let ws = concat $ map (\sr -> F.wfC bindEnv sr ()) srefts
  tell $ CGCons { wfcs = ws, subcs = [] }

splitWs :: Template -> CG [F.WfC ()]
splitWs = para alg
  where alg :: C.CofreeF TypeF F.Reft (Template, CG [F.WfC ()]) -> (CG [F.WfC ()])

        alg t@(_ C.:< TFunF b (t1, w1) (_, w2)) = do
          w0 <- bsplitW t
          w1' <- w1
          w2' <- inEnv (F.symbol b, toSReft t1) w2
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
mkSubC cs = undefined

tmpltOf :: T.Text -> CG Template
tmpltOf n = fromMaybe
              (error "At this point HM inference was successful so we should \
                     \always have what we lookup in our environment")
              <$> (M.lookup n <$> asks cgConsts)

freshTmplt :: Type -> CG Template
freshTmplt t = cata alg t
  where alg :: TypeF (CG QType) -> CG QType
        alg (TVarF _) = error "Can we have vars?"
        alg (TGenF _ _) = error "Can we have gen here?"
        alg (TInsF _ _) = error "Can we have ins here?"
        alg (TFunF b q1 q2) = liftM2 (:<) freshKVar (liftM2 (TFunF b) q1 q2)
        alg (TConF n qs)    = liftM2 (:<) freshKVar (liftM (TConF n) $ sequence qs)
        alg (TTupF qs)      = liftM2 (:<) freshKVar (liftM  TTupF    $ sequence qs)

freshKVar :: CG F.Reft
freshKVar = do
  k <- gets kIndex
  modify (\(s@CGState{..}) -> s{kIndex = k+1})
  return $ F.reft F.vv_ $ F.PKVar (F.intKvar k) (F.Su H.empty)

strengthen :: Reftable a => QType -> a -> QType
strengthen (r :< fq) a = (r <> reft a) :< fq

inEnv :: (F.Symbol, F.SortedReft) -> CG a -> CG a
inEnv (s, sr) x = do
  be  <- gets cgGBinds
  ibe <- asks cgLBinds
  let (bid, be') = F.insertBindEnv s sr be
  let ibe' = F.insertsIBindEnv [bid] ibe
  modify (\(e@CGState{..}) -> e { cgGBinds = be' })
  local  (\(e@CGEnv{..})   -> e { cgLBinds = ibe' }) x
