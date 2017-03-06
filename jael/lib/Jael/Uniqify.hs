{-# Language MultiWayIf #-}
{-# Language NoImplicitPrelude #-}
{-# Language RecordWildCards #-}

module Jael.Uniqify
  ( uniqifyVars
  )
where

import           Jael.Prelude
import qualified Control.Comonad.Trans.Cofree as C
import qualified Data.HashSet as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Language.Fixpoint.Types as F

import           Jael.Types
import           Jael.Util

-- Make program variables and refinement variables unique. Values for each
-- are drawn from two distinct sets of names so program variables can be
-- substituted into refinements without clashing.

data UniqifyS = UniqifyS
  { pvPool :: [T.Text] -- program var pool
  , vvPool :: [T.Text] -- value var pool
  , varMap :: M.Map T.Text T.Text
  , subMap :: M.Map T.Text T.Text
  }

initState :: UniqifyS
initState = UniqifyS
  { pvPool = concat [map (T.pack . take x . repeat) ['a'..'z'] | x <- [1..]]
  , vvPool = [T.pack $ "v" ++ show x | x <- [(0::Integer)..]]
  , varMap = M.empty
  , subMap = M.empty
  }

type UniqifyM = State UniqifyS

uniqifyVars :: TypedExpr -> (M.Map T.Text T.Text, TypedExpr)
uniqifyVars te = let
  (v, s) = runState (doUnique te) initState
  in (subMap s, v)

doUnique :: TypedExpr -> UniqifyM TypedExpr
doUnique = cata uniqifyAlg

uniqifyAlg :: C.CofreeF ExprF QType (UniqifyM TypedExpr) -> (UniqifyM TypedExpr)

uniqifyAlg (t C.:< EAppF e1 e2) = do
  e1' <- e1
  e2' <- e2
  t' <- uniqifyReft t
  return $ t' :< EAppF e1' e2'

uniqifyAlg (t C.:< EAbsF x e) = do
  e' <- e
  t' <- uniqifyReft t
  x' <- subOrFreshPV (value x)
  modify $ \s@(UniqifyS{..}) -> s { varMap = M.delete (value x) varMap }
  return $ t' :< EAbsF x{value = x'} e'

uniqifyAlg (t C.:< ELetF x e1 e2) = do
  e2' <- e2
  x' <- subOrFreshPV (value x)
  modify $ \s@(UniqifyS{..}) -> s { varMap = M.delete (value x) varMap }
  e1' <- e1
  t' <- uniqifyReft t
  return $ t' :< ELetF x{value = x'} e1' e2'


uniqifyAlg (t C.:< EIteF b e1 e2) = do
  e1' <- e1
  e2' <- e2
  b' <- b
  t' <- uniqifyReft t
  return $ t' :< EIteF b' e1' e2'

uniqifyAlg (t C.:< ETupF es) = do
  es' <- sequence es
  t' <- uniqifyReft t
  return $ t' :< ETupF es'

uniqifyAlg (t C.:< EVarF v) = do
  v' <- subVar v
  t' <- uniqifyReft t
  return $ t' :< EVarF v'

uniqifyAlg (t C.:< EConF c) = do
  t' <- uniqifyReft t
  return $ t' :< EConF c

uniqifyReft :: QType -> UniqifyM QType

uniqifyReft = cata uniqifyReftAlg

uniqifyReftAlg :: C.CofreeF TypeF F.Reft (UniqifyM QType) -> (UniqifyM QType)

uniqifyReftAlg (r C.:< (TFunF t1 t2)) = do
  t1' <- t1
  t2' <- t2
  r' <- subReft r
  return $ r' :< TFunF t1' t2'

uniqifyReftAlg (r C.:< TTupF ts) = do
  ts' <- sequence ts
  r' <- subReft r
  return $ r' :< TTupF ts'

uniqifyReftAlg (r C.:< TConF n ts) = do
  ts' <- sequence ts
  r' <- subReft r
  return $ r' :< TConF n ts'

uniqifyReftAlg (r C.:< TVarF x) = liftM (:< TVarF x) (subReft r)

uniqifyReftAlg (r C.:< TInsF vsts t) = do
  let (vs, ts) = unzip vsts
  ts' <- sequence ts
  let vsts' = zip vs ts'
  t' <- t
  r' <- subReft r
  return $ r' :< TInsF vsts' t'

uniqifyReftAlg (r C.:< TGenF vs t) = do
  t' <- t
  r' <- subReft r
  return $ r' :< TGenF vs t'

subReft :: F.Reft -> UniqifyM F.Reft
subReft r@(F.Reft (v, e))
  -- Only uniqify interesting refinements
  | r == F.trueReft =
      return r
  | otherwise = do
      let vs = S.toList $ F.reftFreeVars r
      vs' <- mapM (subOrFreshPV . F.symbolText) vs
      v' <- liftM F.symbol nextVV
      let su = F.mkSubst $ (v, F.eVar v'):(zip vs $ map F.eVar vs')
      return $ F.Reft (v', F.subst su e)

subVar :: Ident -> UniqifyM Ident
subVar (Token n p) = subOrFreshPV n >>= return . flip Token p

nextVV :: UniqifyM T.Text
nextVV = do
  (next:pool') <- gets vvPool
  modify (\s@UniqifyS{..} -> s{vvPool=pool'})
  return next

nextPV :: UniqifyM T.Text
nextPV = do
  (next:pool') <- gets pvPool
  modify (\s@UniqifyS{..} -> s{pvPool=pool'})
  return next

subFor :: T.Text -> UniqifyM (Maybe T.Text)
subFor v = liftM (M.lookup v) $ gets varMap

subOrFreshPV :: T.Text -> UniqifyM T.Text
subOrFreshPV v = do
  ms <- subFor v
  case ms of
    Just s -> return s
    Nothing -> do
      n <- nextPV
      modify (\s@(UniqifyS{..}) -> s{varMap = M.insert v n varMap})
      modify (\s@(UniqifyS{..}) -> s{subMap = M.insert n v subMap})
      return n
