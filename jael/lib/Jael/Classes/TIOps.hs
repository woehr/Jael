{-# Language NoImplicitPrelude #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}

module Jael.Classes.TIOps where

import           Jael.Prelude
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Jael.Types
import Jael.Util

class TIOps a where
  ftv :: a -> S.Set T.Text
  apply :: M.Map T.Text Type -> a -> a

instance TIOps Type where
  ftv = cata alg
    where alg (TVarF v)     = S.singleton (value v)
          alg (TFunF _ t1 t2) = t1 `S.union` t2
          alg (TTupF ts)    = S.unions ts
          alg (TConF _ ts)  = S.unions ts
          alg (TInsF ss t)  = S.unions $ (t S.\\ S.fromList (map fst ss)) : map snd ss
          alg (TGenF vs t)  = assert (S.fromList vs == t) t

  apply s t@(TVar v)   = M.findWithDefault t (value v) s
  apply s (TIns ss t)  = TIns (map (second $ apply s) ss) $
                           apply (s `M.difference` M.fromList ss) t
  apply s (TGen _ t)   = let t' = apply s t in TGen (S.toList $ ftv t') t'
  -- Uninteresting cases
  apply s (TFun b t1 t2) = TFun b (apply s t1) (apply s t2)
  apply s (TTup ts)    = TTup $ map (apply s) ts
  apply s (TCon n ts)  = TCon n $ map (apply s) ts
  apply _ _ = error "Why does ghc complain about unmatched patterns?"

instance TIOps (Type, Type) where
  ftv (t1, t2) = S.union (ftv t1) (ftv t2)
  apply s ts = join bimap (apply s) ts

instance TIOps QType where
  ftv = ftv . shape

  apply s t = let t' = apply s (shape t)
              in  case addReftsTo t (noQual t') of
                    Left e -> error $ show e
                    Right x -> x

instance TIOps a => TIOps [a] where
  ftv = S.unions . map ftv
  apply s = map (apply s)

instance TIOps HMTypedExpr where
  ftv = freeVars . removeAnn
  apply s = fmap (apply s)

instance TIOps TScheme where
  ftv (Scheme vs t) = ftv t S.\\ S.fromList vs
  apply s (Scheme vs t) = Scheme vs $ apply (foldr M.delete s vs) t

instance TIOps (M.Map T.Text TScheme) where
  ftv = S.unions . map ftv. M.elems
  apply s = M.map (apply s)
