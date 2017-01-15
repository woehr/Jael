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

data Builtin = BTUnit
             | BTBool
             | BTInt
             | BTBit
             | BTBuffer QType
             deriving (Eq, Show)

type VV = T.Text
type ValVar = Maybe VV

-- Add info about the size of Unqualified so termination checks on
-- recursive functions can be performed.
{-@ autosize Type @-}
data Type = TBuiltin Builtin
          | TFun Type Type
          | TVar Ident
          | TTup [Type]
          | TNamed Ident [Type]
          deriving (Eq, Show)

data TypeF a = TBuiltinF Builtin
             | TFunF a a
             | TVarF Ident
             | TTupF [a]
             | TNamedF Ident [a]
             deriving (Eq, Functor, Show)

type instance F.Base Type = TypeF

instance F.Recursive Type where
  project (TBuiltin x) = TBuiltinF x
  project (TFun x y)   = TFunF x y
  project (TVar x)     = TVarF x
  project (TTup x)     = TTupF x
  project (TNamed x y) = TNamedF x y

instance F.Corecursive Type where
  embed (TBuiltinF x) = TBuiltin x
  embed (TFunF x y)   = TFun x y
  embed (TVarF x)     = TVar x
  embed (TTupF x)     = TTup x
  embed (TNamedF x y) = TNamed x y

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

data Qual = Qual VV L.Expr
          deriving (Eq, Show)

arityOf :: Type -> Integer
arityOf (TFun _ x) = 1 + arityOf x
arityOf _ = 0

data QType = QType (Ann (Maybe Qual) TypeF QType)
  deriving (Eq, Show)

data QTypeF a = QTypeF (Ann (Maybe Qual) TypeF a)
  deriving (Eq, Functor, Show)

type instance F.Base QType = QTypeF

instance F.Recursive QType where
  project (QType (Ann a t)) = QTypeF (Ann a t)

instance F.Corecursive QType where
  embed (QTypeF (Ann a t)) = QType (Ann a t)
