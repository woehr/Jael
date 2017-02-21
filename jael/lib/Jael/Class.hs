{-# Language NoImplicitPrelude #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}

module Jael.Class where

import           Jael.Prelude
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import qualified Language.Fixpoint.Types as F

import Jael.Types
import Jael.Util

class TIOps a where
  ftv :: a -> S.Set T.Text
  apply :: M.Map T.Text Type -> a -> a

instance TIOps Type where
  ftv = cata alg
    where alg (TVarF t)      = S.singleton (value t)
          alg (TFunF t1 t2)  = t1 `S.union` t2
          alg (TTupF ts)     = S.unions ts
          alg (TNamedF _ ts) = S.unions ts
          alg _              = S.empty

  apply s = cata alg
    where alg :: TypeF Type -> Type
          alg t@(TVarF v) = M.findWithDefault (embed t) (value v) s
          alg t = embed t

instance TIOps (Type, Type) where
  ftv (t1, t2) = S.union (ftv t1) (ftv t2)
  apply s ts = join bimap (apply s) ts

instance TIOps QType where
  ftv = ftv . removeAnn

-- Both apply implementations work

  apply s = iterCofree f
    where f :: [F.Reft] -> TypeF QType -> QType
          f a (TVarF n) = flip setAnn a $ noQual (apply s $ Fix $ TVarF n)
          f a q = a :< q

instance TIOps a => TIOps [a] where
  ftv = S.unions . map ftv
  apply s = map (apply s)

--  apply s = cata alg
--    where alg :: C.CofreeF TypeF (Maybe Qual) QType -> QType
--          alg (a C.:< (TVarF n)) =
--            flip setAnn a $ noQual (apply s $ Fix $ TVarF n)
--          alg x = embed x

--------------- Instances for various kinds of exprs ---------------

instance TIOps HMTypedExpr where
  ftv = freeVars . removeAnn
  apply s = fmap (apply s)

