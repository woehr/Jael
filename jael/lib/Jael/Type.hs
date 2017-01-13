{-# Language DeriveFunctor #-}
{-# Language TypeFamilies #-}

module Jael.Type where

import Prelude ()
import BasePrelude hiding (TVar)
import qualified Data.Functor.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Language.Fixpoint.Types as L
import Jael.Util

class TIOps a where
  ftv :: a -> S.Set T.Text
  apply :: M.Map T.Text Type -> a -> a

data BaseType = BTUnit
              | BTBool
              | BTInt
              | BTBit
              | BTBuffer Type
              deriving (Eq, Show)

data ValVar = VV T.Text
            | VVFresh
            deriving (Eq, Show)

-- Add refinements regarding the size of Type so termination checks on
-- recursive functions can be performed.
{-@ autosize Type @-}
data Type = TBase BaseType
          | TFun Type Type
          | TVar Ident
          | TTup [Type]
          | TNamed T.Text [Type]
          deriving (Eq, Show)

data TypeF a = TBaseF BaseType
             | TFunF a a
             | TVarF Ident
             | TTupF [a]
             | TNamedF T.Text [a]
             deriving (Eq, Functor, Show)

type instance F.Base Type = TypeF

instance F.Recursive Type where
  project (TBase  x)     = TBaseF  x
  project (TFun   x y  ) = TFunF   x y
  project (TVar   x)     = TVarF   x
  project (TTup   x)     = TTupF   x
  project (TNamed x y)   = TNamedF x y

instance F.Corecursive Type where
  embed (TBaseF  x)     = TBase  x
  embed (TFunF   x y)   = TFun   x y
  embed (TVarF   x)     = TVar   x
  embed (TTupF   x)     = TTup   x
  embed (TNamedF x y)   = TNamed x y

instance TIOps Type where
  ftv = F.cata alg
    where alg (TVarF t)      = S.singleton (value t)
          alg (TFunF t1 t2)  = t1 `S.union` t2
          alg (TTupF ts)     = S.unions ts
          alg (TNamedF _ ts) = S.unions ts
          alg _              = S.empty

  apply s = F.cata alg
    where alg t@(TVarF v) = M.findWithDefault (F.embed t) (value v) s
          alg t = F.embed t

arityOf :: Type -> Integer
arityOf (TFun _ x) = 1 + arityOf x
arityOf _ = 0

-- Expr from liquid-fixpoint is the data type for predicates
data QType = QTValVar ValVar L.Expr
           | QTTypedValVar ValVar QType L.Expr
           | QTNothing L.Expr
           deriving (Eq, Show)
