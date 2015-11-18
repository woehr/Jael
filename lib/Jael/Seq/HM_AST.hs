module Jael.Seq.HM_AST where

import Data.Functor.Foldable as F
import Jael.Seq.HM_Types
import Jael.Seq.Literal
import Jael.Seq.Prm

data Ex = EVar Text
        | EPrm Prm
        | ELit Literal
        | EApp Ex Ex
        | EAbs Text Ex
        | ELet Text Ex Ex
        deriving (Eq, Show)

data ExF a = EVarF Text
           | EPrmF Prm
           | ELitF Literal
           | EAppF a a
           | EAbsF Text a
           | ELetF Text a a
           deriving (Eq, Functor, Show)

type instance Base Ex = ExF

instance F.Foldable Ex where
  project (EVar x)     = EVarF x
  project (EPrm x)     = EPrmF x
  project (ELit x)     = ELitF x
  project (EApp x y)   = EAppF x y
  project (EAbs x y)   = EAbsF x y
  project (ELet x y z) = ELetF x y z

instance F.Unfoldable Ex where
  embed (EVarF x)     = EVar x
  embed (EPrmF x)     = EPrm x
  embed (ELitF x)     = ELit x
  embed (EAppF x y)   = EApp x y
  embed (EAbsF x y)   = EAbs x y
  embed (ELetF x y z) = ELet x y z

-- Annotate type f a with something of type x
data Ann x f a = Ann { ann :: x, unAnn :: f a }
  deriving (Show, Functor)

data TypedEx = TypedEx (Ann Ty ExF TypedEx)
  deriving (Show)

data TypedExF a = TypedExF (Ann Ty ExF a)
  deriving (Show, Functor)

type instance Base TypedEx = TypedExF

instance F.Foldable TypedEx where
  project (TypedEx Ann {ann=t, unAnn=e}) = TypedExF Ann {ann=t, unAnn=e}

instance F.Unfoldable TypedEx where
  embed (TypedExF Ann {ann=t, unAnn=e}) = TypedEx Ann {ann=t, unAnn=e}

instance TyOps TypedEx where
  ftv (TypedEx (Ann {ann=t})) = ftv t
  apply s = cata alg
    where alg (TypedExF Ann {ann=t, unAnn=e}) = TypedEx Ann {ann=apply s t, unAnn=e}

instance SeqTypable TypedEx where
  tyOf (TypedEx Ann {ann=t}) = t

mkTyped :: Ty -> ExF TypedEx -> TypedEx
mkTyped t e = TypedEx Ann {ann=t, unAnn=e}

