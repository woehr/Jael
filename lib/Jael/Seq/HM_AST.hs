module Jael.Seq.HM_AST where

import Data.Functor.Foldable as F
import Jael.Seq.Literal
import Jael.Seq.Prm
import Jael.Seq.Types
import Jael.Util

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

data HMTypedEx = HMTypedEx (Ann HMTy ExF HMTypedEx)
  deriving (Show)

data HMTypedExF a = HMTypedExF (Ann HMTy ExF a)
  deriving (Show, Functor)

type instance Base HMTypedEx = HMTypedExF

instance F.Foldable HMTypedEx where
  project (HMTypedEx Ann {ann=t, unAnn=e}) = HMTypedExF Ann {ann=t, unAnn=e}

instance F.Unfoldable HMTypedEx where
  embed (HMTypedExF Ann {ann=t, unAnn=e}) = HMTypedEx Ann {ann=t, unAnn=e}

instance TIOps HMTypedEx where
  ftv (HMTypedEx (Ann {ann=t})) = ftv t
  apply s = cata alg
    where alg (HMTypedExF Ann {ann=t, unAnn=e}) = HMTypedEx Ann {ann=apply s t, unAnn=e}

instance HMTypable HMTypedEx where
  hmTyOf (HMTypedEx Ann {ann=t}) = t

mkTyped :: HMTy -> ExF HMTypedEx -> HMTypedEx
mkTyped t e = HMTypedEx Ann {ann=t, unAnn=e}

