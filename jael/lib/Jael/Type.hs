{-# Language DeriveFunctor #-}
{-# Language FlexibleInstances #-}
{-# Language NoImplicitPrelude #-}
{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}
{-# Language TypeSynonymInstances #-}

module Jael.Type where

import           BasePrelude hiding ((<>), (<$>), (<+>), empty)

import           Control.Comonad.Cofree
import qualified Control.Comonad.Trans.Cofree as C
import qualified Data.Text as T
import           Data.Eq.Deriving (deriveEq1)
import           Data.Functor.Foldable
import           Text.Show.Deriving (deriveShow1)
import           Text.PrettyPrint.Leijen.Text

import qualified Language.Fixpoint.Types as L

import           Jael.Util
import           Jael.Util.Ann
import           Jael.Pretty

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
ppTypeAlg (TFunF t1 t2) =
  t1 <$> text "->" <+> t2

data VV = VV T.Text
        | NoVV
        deriving (Eq, Show)

renderVV :: Int -> VV -> Doc
renderVV _ (VV t) = textStrict t
renderVV i NoVV   = textStrict $ T.replicate i "\\"

data Qual = Qual VV L.Expr
          deriving (Eq, Show)

type QType = Ann TypeF (Maybe Qual)

instance Pretty QType where
  pretty = cata alg
    where
      alg :: C.CofreeF TypeF (Maybe Qual) Doc -> Doc
      alg ((Just (Qual vv e)) C.:< t) =
        braces (renderVV 0 vv <+> text ":" <+>
                ppTypeAlg t <+> text "|" <+>
                fpPretty e)
      alg (Nothing  C.:< t) = ppTypeAlg t

noQual :: Type -> QType
noQual = cata alg
  where
    alg :: TypeF QType -> QType
    alg x = Nothing :< x

arityOf :: Type -> Integer
arityOf (Fix (TFunF _ x)) = 1 + arityOf x
arityOf _ = 0

$(deriveEq1   ''TypeF)
$(deriveShow1 ''TypeF)
