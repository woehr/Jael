{-# Language DeriveFunctor #-}
{-# Language DeriveTraversable #-}
{-# Language FlexibleInstances #-}
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}
{-# Language TemplateHaskell #-}
{-# Language TypeSynonymInstances #-}

module Jael.New.Type where

import qualified Data.Text as T
import           Text.PrettyPrint.ANSI.Leijen as P
import           Data.Eq.Deriving (deriveEq1)
import           Text.Show.Deriving (deriveShow1)

data TypeF a = TAllF [T.Text] a
             | TFunF a a
             | TConF T.Text [a]
             | TTupF [a]
             | TRecF [(T.Text, a)]
             | TArrF a Integer
             | TVarF T.Text
             deriving (Eq, Foldable, Functor, Show, Traversable)

type Type = Fix TypeF

$(deriveEq1   ''TypeF)
$(deriveShow1 ''TypeF)

pattern TAll :: [T.Text] -> Type -> Type
pattern TAll a b = Fix (TAllF a b)

pattern TFun :: Type -> Type -> Type
pattern TFun a b = Fix (TFunF a b)

pattern TCon :: T.Text -> [Type] -> Type
pattern TCon a b = Fix (TConF a b)

pattern TTup :: [Type] -> Type
pattern TTup xs = Fix (TTupF xs)

pattern TRec :: [(T.Text, Type)] -> Type
pattern TRec xs = Fix (TRecF xs)

pattern TArr :: Type -> Integer -> Type
pattern TArr t n = Fix (TArrF t n)

pattern TVar :: T.Text -> Type
pattern TVar a = Fix (TVarF a)

pattern TBool :: Type
pattern TBool = Fix (TConF "Bool" [])

pattern TInt :: Type
pattern TInt = Fix (TConF "Int" [])

class Prec a where
  prec :: a -> Integer

instance Prec (TypeF a) where
  prec (TAllF _ _) = 0
  prec (TFunF _ _) = 1
  prec (TConF _ _) = 2
  prec (TRecF _)   = 2
  prec (TTupF _)   = 2
  prec (TArrF _ _) = 2
  prec (TVarF _)   = 2

instance Prec Type where
  prec (Fix x) = prec x

braced :: [Doc] -> Doc
braced = encloseSep lbrace rbrace comma

checkPrec :: (Prec a, Prec b, Pretty b) => (Integer -> Integer -> Bool) -> a -> b -> Doc
checkPrec f x y =
  let p = pretty y
  in if prec x `f` prec y then parens p else p

precGt :: (Prec a, Prec b, Pretty b) => a -> b -> Doc
precGt = checkPrec (>)

precGe :: (Prec a, Prec b, Pretty b) => a -> b -> Doc
precGe = checkPrec (>=)

instance (Prec a, Pretty a) => Pretty (TypeF a) where
  pretty x@(TAllF as t) =
          text "forall"
    P.<+> hsep (map (string . T.unpack) as)
    P.<>  dot
    P.<+> precGt x t

  pretty x@(TFunF a b) =
          precGe x a
    P.<+> string "->"
    P.<+> precGt x b

  pretty (TConF n xs) =
    let xs' = if null xs then P.empty else tupled $ map pretty xs
    in  pretty (T.unpack n) P.<> xs'

  pretty (TTupF xs) = tupled $ map pretty xs

  pretty r@(TRecF xs) = braced $ flip map xs
    (\(l, t) -> pretty (T.unpack l) P.<+> colon P.<+> precGt r t)

  pretty (TArrF t n) = P.brackets $ pretty t P.<> semi P.<+> pretty n

  pretty (TVarF x) = string (T.unpack x)

instance P.Pretty Type where
  pretty (Fix x) = P.pretty x
