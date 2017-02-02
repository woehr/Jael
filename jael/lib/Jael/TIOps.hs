{-# Language FlexibleInstances #-}
{-# Language NoImplicitPrelude #-}

module Jael.TIOps where

import           BasePrelude

import           Control.Comonad.Cofree
import           Data.Functor.Foldable
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Jael.Expr
import Jael.Type
import Jael.Util
import Jael.Util.Ann

class TIOps a where
  ftv :: a -> S.Set T.Text
  apply :: M.Map T.Text Type -> a -> a

--------------- Instances for various kinds of types ---------------

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

instance TIOps a => TIOps [a] where
  ftv = S.unions . map ftv
  apply s = map (apply s)

instance TIOps (Type, Type) where
  ftv (t1, t2) = S.union (ftv t1) (ftv t2)
  apply s ts = join bimap (apply s) ts

instance TIOps QType where
  ftv = ftv . removeAnn

-- Both apply implementations work

  apply s = iterCofree f
    where f :: Maybe Qual -> TypeF QType -> QType
          f a (TVarF n) = flip setAnn a $ noQual (apply s $ Fix $ TVarF n)
          f a q = a :< q

--  apply s = cata alg
--    where alg :: C.CofreeF TypeF (Maybe Qual) QType -> QType
--          alg (a C.:< (TVarF n)) =
--            flip setAnn a $ noQual (apply s $ Fix $ TVarF n)
--          alg x = embed x

--------------- Instances for various kinds of exprs ---------------

instance TIOps TypedExpr where
  ftv = freeVars . removeAnn
  apply s = fmap (apply s)
