{-# Language MultiWayIf #-}
{-# Language NoImplicitPrelude #-}
{-# Language RecordWildCards #-}
{-# Language TupleSections #-}

module Jael.Uniqify
  ( uniqifyExpr
  , uniqifyQType
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
-- are drawn from distinct sets of names so program variables can be
-- substituted into refinements without clashing.

data UniqifyS = UniqifyS
  { pvPool :: [T.Text]
  , vvPool :: [T.Text]
  , subMap :: M.Map T.Text T.Text
  }

initState :: [T.Text] -> [T.Text] -> UniqifyS
initState pv vv = UniqifyS
  { pvPool = pv
  , vvPool = vv
  , subMap = M.empty
  }

data UniqifyR = UniqifyR
  { binds :: M.Map T.Text T.Text
  }

initEnv :: M.Map T.Text T.Text -> UniqifyR
initEnv = UniqifyR

type UniqifyM = ReaderT UniqifyR (State UniqifyS)

uniqifyExpr :: TypedExpr -> (M.Map T.Text T.Text, TypedExpr)
uniqifyExpr te = let
  pv = concat [map (T.pack . take x . repeat) ['a'..'z'] | x <- [1..]]
  (v, s) = flip runState (initState pv $ error "not used")
         $ flip runReaderT (initEnv M.empty)
         $ cata uniqifyExprAlg te
  in (subMap s, v)

uniqifyQType :: QType -> M.Map T.Text T.Text -> (M.Map T.Text T.Text, QType)
uniqifyQType t m = let
  pv = [T.pack $ "b" ++ show x | x <- [(0::Integer)..]]
  vv = [T.pack $ "v" ++ show x | x <- [(0::Integer)..]]
  (v, s) = flip runState (initState pv vv)
         $ flip runReaderT (initEnv m)
         $ cata uniqifyQTypeAlg t
  in (subMap s, v)

uniqifyExprAlg :: C.CofreeF ExprF QType (UniqifyM TypedExpr)
                                     -> (UniqifyM TypedExpr)

uniqifyExprAlg (t C.:< EAppF e1 e2) = do
  e1' <- e1
  e2' <- e2
  t' <- uniqifyQType' t
  return $ t' :< EAppF e1' e2'

uniqifyExprAlg (t C.:< EAbsF x e) = do
  t' <- uniqifyQType' t
  (x', e') <- inEnv (value x) e
  return $ t' :< EAbsF x{value = x'} e'

uniqifyExprAlg (t C.:< ELetF x e1 e2) = do
  t' <- uniqifyQType' t
  (x', e2') <- inEnv (value x) e2
  e1' <- e1
  return $ t' :< ELetF x{value = x'} e1' e2'

uniqifyExprAlg (t C.:< EIteF b e1 e2) = do
  e1' <- e1
  e2' <- e2
  b' <- b
  t' <- uniqifyQType' t
  return $ t' :< EIteF b' e1' e2'

uniqifyExprAlg (t C.:< ETupF es) = do
  es' <- sequence es
  t' <- uniqifyQType' t
  return $ t' :< ETupF es'

uniqifyExprAlg (t C.:< EVarF v) = do
  -- Vars must be bound so this must succeed
  m <- asks binds
  let v' = M.findWithDefault (error $ "Unboud var: " ++ show v) (value v) m
  t' <- uniqifyQType' t
  return $ t' :< EVarF v{value=v'}

uniqifyExprAlg (t C.:< EConF c) = do
  t' <- uniqifyQType' t
  return $ t' :< EConF c

uniqifyQTypeAlg :: C.CofreeF TypeF F.Reft (UniqifyM QType)
                                       -> (UniqifyM QType)

uniqifyQTypeAlg (r C.:< (TFunF b t1 t2)) = do
    r' <- subReft r
    t1' <- t1
    (b', t2') <- inEnv (value b) t2
    return $ r' :< TFunF b{value=b'} t1' t2'

uniqifyQTypeAlg (r C.:< TTupF ts) = do
  ts' <- sequence ts
  r' <- subReft r
  return $ r' :< TTupF ts'

uniqifyQTypeAlg (r C.:< TConF n ts) = do
  ts' <- sequence ts
  r' <- subReft r
  return $ r' :< TConF n ts'

uniqifyQTypeAlg (r C.:< TVarF x) = liftM (:< TVarF x) (subReft r)

uniqifyQTypeAlg (r C.:< TInsF vsts t) = do
  let (vs, ts) = unzip vsts
  ts' <- sequence ts
  let vsts' = zip vs ts'
  t' <- t
  r' <- subReft r
  return $ r' :< TInsF vsts' t'

uniqifyQTypeAlg (r C.:< TGenF vs t) = do
  t' <- t
  r' <- subReft r
  return $ r' :< TGenF vs t'

inEnv :: T.Text -> UniqifyM a -> UniqifyM (T.Text, a)
inEnv v x = do
  v' <- nextPV
  modify (\s@(UniqifyS{..}) -> s{subMap = M.insert v' v subMap})
  x' <- local (\(r@UniqifyR{..}) -> r{binds=M.insert v v' (M.delete v binds)}) x
  return (v', x')

uniqifyQType' :: QType -> UniqifyM QType
uniqifyQType' t = do
  (m, t') <- liftM (uniqifyQType t) $ asks binds
  modify (\(s@UniqifyS{..}) -> s{ subMap = subMap `M.union` m})
  return t'

subReft :: F.Reft -> UniqifyM F.Reft
subReft r@(F.Reft (v, e))
  -- Only uniqify interesting refinements
  | r == F.trueReft =
      return r
  | otherwise = do
      let vs = S.toList $ F.reftFreeVars r
      vs' <- doLookups $ map F.symbolText vs
      v'  <- liftM F.symbol nextVV
      let su = F.mkSubst $ (v, F.eVar v'):(zip vs $ map F.eVar vs')
      return $ F.Reft (v', F.subst su e)

doLookups :: [T.Text] -> UniqifyM [T.Text]
doLookups = mapM f
  where f v = do
          m <- asks binds
          case M.lookup v m of
            Just v' -> return v'
            Nothing -> do
              next <- nextPV
              modify (\(s@UniqifyS{..}) ->
                        s{subMap = M.insert next v subMap}
                     )
              return next

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
