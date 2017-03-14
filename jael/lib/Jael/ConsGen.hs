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
--import           Jael.Constants (intConst, add)
import           Jael.Types hiding (removeAnn)
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
          mkSort (_ C.:< TFunF s1 s2) = F.FFunc s1 s2

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

--consGen (qt :< EConF (CInt (Token i _))) =
--  return $ (fmap liftQType $ intConst i) `strengthenT` liftQType qt
--consGen (qt :< EConF CAdd) =
--  return $ add `strengthenT` qt

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
  bindEnv <- asks cgLBinds
  let srefts = concat $ map splitWs tmplts
  let ws = concat $ map (\sr -> F.wfC bindEnv sr ()) srefts
  tell $ CGCons { wfcs = ws, subcs = [] }

splitWs :: Template -> [F.SortedReft]
splitWs = undefined

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
        alg (TFunF q1 q2) = liftM2 (:<) freshKVar (liftM2 TFunF q1 q2)
        alg (TConF n qs)  = liftM2 (:<) freshKVar (liftM (TConF n) $ sequence qs)
        alg (TTupF qs)    = liftM2 (:<) freshKVar (liftM  TTupF    $ sequence qs)

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
