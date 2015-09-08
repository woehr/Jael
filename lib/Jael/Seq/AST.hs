{-# Language DeriveFunctor, NoImplicitPrelude, TypeFamilies #-}
module Jael.Seq.AST where

import ClassyPrelude hiding (Foldable)
import Data.Functor.Foldable
import Jael.Seq.Types

data Ex = EVar Text
        | EUnit
        | EInt Integer
        | EBool Bool
        | EApp Ex Ex
        | EAbs Text Ex
        | ELet Text Ex Ex
          deriving (Show)

data ExF a = EVarF Text
           | EUnitF
           | EIntF Integer
           | EBoolF Bool
           | EAppF a a
           | EAbsF Text a
           | ELetF Text a a
             deriving (Eq, Functor, Show)

type instance Base Ex = ExF

instance Foldable Ex where
  project (EVar x)     = EVarF x
  project (EUnit)      = EUnitF
  project (EInt x)     = EIntF x
  project (EBool x)    = EBoolF x
  project (EApp x y)   = EAppF x y
  project (EAbs x y)   = EAbsF x y
  project (ELet x y z) = ELetF x y z

instance Unfoldable Ex where
  embed (EVarF x)     = EVar x
  embed (EUnitF)      = EUnit
  embed (EIntF x)     = EInt x
  embed (EBoolF x)    = EBool x
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

instance Foldable TypedEx where
  project (TypedEx (Ann { ann = t, unAnn = e })) = TypedExF (Ann { ann = t, unAnn = e})

instance Unfoldable TypedEx where
  embed (TypedExF (Ann { ann = t, unAnn = e })) = TypedEx (Ann {ann = t, unAnn = e})

instance TyOps TypedEx where
  ftv (TypedEx (Ann { ann = t })) = ftv t
  apply s = cata alg
    where alg (TypedExF (Ann {ann = t, unAnn = e})) = TypedEx (Ann {ann = apply s t, unAnn = e})

tyOf :: TypedEx -> Ty
tyOf (TypedEx (Ann { ann = t })) = t

mkTyped :: Ty -> ExF TypedEx -> TypedEx
mkTyped t e = TypedEx $ Ann {ann = t, unAnn = e}

