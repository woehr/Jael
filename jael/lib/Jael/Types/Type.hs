{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
{-# Language FlexibleInstances #-}
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}
{-# Language TemplateHaskell #-}
{-# Language TypeSynonymInstances #-}
{-# Language DeriveDataTypeable #-}
{-# Language StandaloneDeriving #-}
{-# Language RankNTypes #-}

module Jael.Types.Type where

import           Prelude hiding ((<>), (<$>), (<+>), empty)

import qualified Control.Comonad.Trans.Cofree as C
import           Data.Eq.Deriving (deriveEq1)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Text.Show.Deriving (deriveShow1)
import           Text.PrettyPrint.Leijen.Text

import qualified Language.Fixpoint.Types as F

import           Jael.Pretty
import           Jael.Types.Ann
import           Jael.Util

data TypeF a = TFunF Ident a a
             | TVarF Ident
             | TTupF [a]
             | TConF Ident [a]
             deriving (Data, Eq, Foldable, Functor, Show, Traversable)

type Type  = Fix TypeF
type QType = Cofree TypeF (Maybe F.Reft)
type RType = Cofree TypeF F.Reft

deriving instance Data QType

data Scheme a = Scheme
  { schGens :: S.Set T.Text
  , schIns  :: M.Map T.Text a
  , schType :: a
  } deriving (Data, Eq, Functor, Show)

type TScheme = Scheme Type
type QScheme = Scheme QType
type Template = Scheme RType


pattern TUnitF :: TypeF a
pattern TUnitF = TConF "Unit" []

pattern TUnit :: Type
pattern TUnit = Fix (TConF "Unit" [])

pattern TBoolF :: TypeF a
pattern TBoolF = TConF "Bool" []

pattern TBool :: Type
pattern TBool = Fix (TConF "Bool" [])

pattern TIntF :: TypeF a
pattern TIntF = TConF "Int" []

pattern TInt :: Type
pattern TInt = Fix (TConF "Int" [])

pattern TBitsF :: TypeF a
pattern TBitsF = TConF "Bits" []

pattern TBits :: Type
pattern TBits = Fix (TConF "Bits" [])

pattern TBufferF :: forall t. t -> TypeF t
pattern TBufferF a = TConF "Buffer" [a]

pattern TBuffer :: Type -> Type
pattern TBuffer a = Fix (TConF "Buffer" [a])

pattern TFun :: Ident -> Type -> Type -> Type
pattern TFun a b c = Fix (TFunF a b c)

pattern TVar :: Ident -> Type
pattern TVar a = Fix (TVarF a)

pattern TTup :: [Type] -> Type
pattern TTup as = Fix (TTupF as)

pattern TCon :: Ident -> [Type] -> Type
pattern TCon a bs = Fix (TConF a bs)

--pattern TIns :: [(T.Text, Type)] -> Type -> Type
--pattern TIns subs t = Fix (TInsF subs t)

--pattern TGen :: [T.Text] -> Type -> Type
--pattern TGen as t = Fix (TGenF as t)

instance Pretty Type where
  pretty = cata ppTypeAlg

ppTypeAlg :: TypeF Doc -> Doc

--ppTypeAlg (TInsF ss t) = textStrict "ins" <> list (map (\(x,y)->tupled $ [textStrict x, y]) ss) <+> t
--ppTypeAlg (TGenF vs t) = textStrict "gen" <> list (map textStrict vs) <+> t

ppTypeAlg (TConF (Token n _) ts) =
  textStrict n <>
  if null ts then empty else tupled ts

ppTypeAlg (TVarF (Token n _)) = textStrict n
ppTypeAlg (TTupF ts) = tupled ts
ppTypeAlg (TFunF b t1 t2) =
  textStrict (value b) <+> colon <+> t1 <+> text "->" <+> t2

instance Pretty QType where
  pretty = cata alg
    where
      alg :: C.CofreeF TypeF (Maybe F.Reft) Doc -> Doc
      alg (Just (F.Reft (n, e)) C.:< t) =
        braces $
          (textStrict (F.symbolText n) <+>
           colon <+>
           ppTypeAlg t <+> (if e == F.PTrue
                            then empty
                            else "|" <+> fpPretty e
                           )
          )
      alg (Nothing C.:< t) = alg (Just F.trueReft C.:< t)

instance Pretty QScheme where
  pretty s = pretty (schType s)

noQual :: Type -> QType
noQual = cata (Nothing :<)
--noQual (TFun b t1 t2) = F.trueReft :< TFunF b (contra t1) (co t2)
--
--  where contra :: Type -> QType
--        contra (TFun c u1 u2) = F.falseReft :< TFunF c (co u1) (contra u2)
--        contra x = cata (F.falseReft :<) x
--
--        co :: Type -> QType
--        co (TFun c v1 v2) = F.trueReft :< TFunF c (contra v1) (co v2)
--        co x = cata (F.trueReft :<) x
--
--noQual x = cata (F.trueReft :<) x

shape :: QScheme -> TScheme
shape (Scheme a b t) = Scheme a (M.map removeAnn b) (removeAnn t)

arityOf :: Type -> Integer
arityOf (Fix (TFunF _ _ x)) = 1 + arityOf x
arityOf _ = 0

$(deriveEq1   ''TypeF)
$(deriveShow1 ''TypeF)
