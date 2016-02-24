module Jael.Type where

import qualified Data.Functor.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Language.Fixpoint.Types as L

class TIOps a where
  ftv :: a -> S.Set T.Text
  apply :: M.Map T.Text Type -> a -> a

data BaseType = BTUnit
              | BTBool
              | BTInt
              deriving (Eq, Show)

data ValVar = VV T.Text
            | VVFresh
            deriving (Eq, Show)

-- Expr from liquid-fixpoint is also the data type for predicates
data Type = TBase BaseType
          | TQual ValVar BaseType L.Expr
          | TFun Type Type
          | TVar T.Text
          deriving (Eq, Show)

data TypeF a = TBaseF BaseType
             | TQualF ValVar BaseType L.Expr
             | TFunF a a
             | TVarF T.Text
             deriving (Eq, Functor, Show)

type instance F.Base Type = TypeF

instance F.Foldable Type where
  project (TBase x)     = TBaseF x
  project (TQual x y z) = TQualF x y z
  project (TFun  x y  ) = TFunF  x y
  project (TVar  x)     = TVarF  x

instance F.Unfoldable Type where
  embed (TBaseF x)     = TBase x
  embed (TQualF x y z) = TQual x y z
  embed (TFunF  x y)   = TFun  x y
  embed (TVarF  x)     = TVar  x

instance TIOps Type where
  ftv = F.cata alg
    where alg (TVarF t)      = S.singleton t
          alg (TFunF t1 t2)  = t1 `S.union` t2
          alg _                 = S.empty

  apply s = F.cata alg
    where alg t@(TVarF v) = M.findWithDefault (F.embed t) v s
          alg t = F.embed t

arityOf :: Type -> Integer
arityOf (TFun _ x) = 1 + arityOf x
arityOf _ = 0
