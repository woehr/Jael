{-# Language DeriveFunctor #-}
{-# Language FlexibleInstances #-}
{-# Language NoImplicitPrelude #-}
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}
{-# Language TemplateHaskell #-}
{-# Language TypeSynonymInstances #-}

module Jael.Types.Type where

import           BasePrelude hiding ((<>), (<$>), (<+>), empty)

import           Control.Comonad.Cofree
import qualified Control.Comonad.Trans.Cofree as C
import           Data.Eq.Deriving (deriveEq1)
import           Data.Functor.Foldable
import qualified Data.Text as T
import           Text.Show.Deriving (deriveShow1)
import           Text.PrettyPrint.Leijen.Text

import qualified Language.Fixpoint.Types as F

import           Jael.Pretty
import           Jael.Types.Ann
import           Jael.Util

data Builtin = BTUnit
             | BTBool
             | BTInt
             | BTBits
             | BTBuffer QType
             deriving (Eq, Show)

data TypeF a = TBuiltinF Builtin
             | TFunF a a
             | TVarF Ident
             | TTupF [a]
             | TNamedF Ident [a]
             deriving (Eq, Functor, Show)

type Type = Fix TypeF
type QType = Ann TypeF [F.Reft]

data Scheme a = Scheme [T.Text] a
  deriving (Eq, Show)

type TScheme = Scheme Type
type QScheme = Scheme QType

pattern TBuiltin :: Builtin -> Type
pattern TBuiltin a = Fix (TBuiltinF a)

pattern TFun :: Type -> Type -> Type
pattern TFun a b = Fix (TFunF a b)

pattern TVar :: Ident -> Type
pattern TVar a = Fix (TVarF a)

pattern TTup :: [Type] -> Type
pattern TTup as = Fix (TTupF as)

pattern TNamed :: Ident -> [Type] -> Type
pattern TNamed a bs = Fix (TNamedF a bs)


instance Pretty Type where
  pretty = cata ppTypeAlg

ppTypeAlg :: TypeF Doc -> Doc
ppTypeAlg (TBuiltinF b) = case b of
  BTBits -> text "Bits"
  BTBool -> text "Bool"
  BTInt  -> text "Int"
  BTUnit -> text "Void"
  BTBuffer t -> text "Buffer" <> parens (pretty t)

ppTypeAlg (TNamedF (Token n _) ts) =
  textStrict n <>
  if length ts == 0 then empty else tupled ts

ppTypeAlg (TVarF (Token n _)) = textStrict n
ppTypeAlg (TTupF ts) = tupled ts
ppTypeAlg (TFunF t1 t2) = t1 <+> text "->" <+> t2

instance Pretty QType where
  pretty = cata alg
    where
      alg :: C.CofreeF TypeF [F.Reft] Doc -> Doc
      alg ([] C.:< t) = ppTypeAlg t
      alg (rs C.:< t) =
        foldr (\(F.Reft (n,e)) acc -> acc <+> colon <+> braces
                (textStrict (F.symbolText n) <+> text "|" <+> fpPretty e))
              (ppTypeAlg t)
              rs

noQual :: Type -> QType
noQual = cata alg
  where
    alg :: TypeF QType -> QType
    alg x = [] :< x

shape :: QType -> Type
shape = removeAnn

arityOf :: Type -> Integer
arityOf (Fix (TFunF _ x)) = 1 + arityOf x
arityOf _ = 0

$(deriveEq1   ''TypeF)
$(deriveShow1 ''TypeF)
