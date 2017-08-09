{-# Language DeriveFunctor #-}
{-# Language FlexibleInstances #-}
{-# Language PatternSynonyms #-}
{-# Language TemplateHaskell #-}
{-# Language TypeSynonymInstances #-}

module Jael.New.Types where

import qualified Data.Text as T
import           Text.PrettyPrint.ANSI.Leijen as P
import           Data.Eq.Deriving (deriveEq1)
import           Text.Show.Deriving (deriveShow1)

braced :: [Doc] -> Doc
braced = encloseSep lbrace rbrace comma

data TypeF a = TAllF [T.Text] a
             | TFunF a a
             | TConF (Maybe T.Text) [a]
             | TRecF [(T.Text, a)]
             | TVarF T.Text
             deriving (Eq, Functor, Show)

type Type = Fix TypeF

$(deriveEq1   ''TypeF)
$(deriveShow1 ''TypeF)

pattern TAll :: [T.Text] -> Type -> Type
pattern TAll a b = Fix (TAllF a b)

pattern TFun :: Type -> Type -> Type
pattern TFun a b = Fix (TFunF a b)

pattern TCon :: (Maybe T.Text) -> [Type] -> Type
pattern TCon a b = Fix (TConF a b)

pattern TRec :: [(T.Text, Type)] -> Type
pattern TRec xs = Fix (TRecF xs)

pattern TVar :: T.Text -> Type
pattern TVar a = Fix (TVarF a)

class Prec a where
  prec :: a -> Integer

instance Prec (TypeF a) where
  prec (TAllF _ _) = 0
  prec (TFunF _ _) = 1
  prec (TRecF _)   = 2
  prec (TConF _ _) = 2
  prec (TVarF _)   = 2

instance Prec Type where
  prec (Fix x) = prec x

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

  pretty (TConF n as) =
    let as' = if null as then P.empty else tupled $ map pretty as
    in case n of
         Just n' ->      pretty (T.unpack n')
                    P.<> as'

         Nothing ->      as'

  pretty r@(TRecF xs) = braced $ flip map xs
    (\(l, t) -> pretty (T.unpack l) P.<+> colon P.<+> precGt r t)

  pretty (TVarF x) = string (T.unpack x)

instance P.Pretty Type where
  pretty (Fix x) = P.pretty x
