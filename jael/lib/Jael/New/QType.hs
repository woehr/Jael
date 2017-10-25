{-# Language DeriveFunctor #-}
{-# Language PatternSynonyms #-}
{-# Language RankNTypes #-}

module Jael.New.QType
where

import qualified Data.Text as T
import qualified Control.Comonad.Trans.Cofree as C
import Jael.New.Type

type Refinement e = Maybe (T.Text, e)
type QTypeF e = C.CofreeF TypeF (Refinement e)
type QType e = Fix (QTypeF e)

unQType :: QTypeF r f -> TypeF f
unQType (_ C.:< t) = t

pattern UQAllF :: [T.Text] -> a -> QTypeF e a
pattern UQAllF a b = Nothing C.:< TAllF a b

pattern UQFunF :: a -> a -> QTypeF e a
pattern UQFunF a b = Nothing C.:< TFunF a b

pattern UQConF :: T.Text -> [a] -> QTypeF e a
pattern UQConF a b = Nothing C.:< TConF a b

pattern UQTupF :: [a] -> QTypeF e a
pattern UQTupF xs = Nothing C.:< TTupF xs

pattern UQRecF :: [(T.Text, a)] -> QTypeF e a
pattern UQRecF xs = Nothing C.:< TRecF xs

pattern UQArrF :: a -> Integer -> QTypeF e a
pattern UQArrF t n = Nothing C.:< TArrF t n

pattern UQVarF :: T.Text -> QTypeF e a
pattern UQVarF a = Nothing C.:< TVarF a

pattern UQAll :: [T.Text] -> QType e -> QType e
pattern UQAll a b = Fix (Nothing C.:< TAllF a b)

pattern UQFun :: QType e -> QType e -> QType e
pattern UQFun a b = Fix (Nothing C.:< TFunF a b)

pattern UQCon :: T.Text -> [QType e] -> QType e
pattern UQCon a b = Fix (Nothing C.:< TConF a b)

pattern UQTup :: [QType e] -> QType e
pattern UQTup xs = Fix (Nothing C.:< TTupF xs)

pattern UQRec :: [(T.Text, QType e)] -> QType e
pattern UQRec xs = Fix (Nothing C.:< TRecF xs)

pattern UQArr :: QType e -> Integer -> QType e
pattern UQArr t n = Fix (Nothing C.:< TArrF t n)

pattern UQVar :: T.Text -> QType e
pattern UQVar a = Fix (Nothing C.:< TVarF a)

pattern QAll :: (T.Text, a) -> [T.Text] -> QType a -> QType a
pattern QAll q a b = Fix (Just q C.:< TAllF a b)

pattern QFun :: (T.Text, a) -> QType a -> QType a -> QType a
pattern QFun q a b = Fix (Just q C.:< TFunF a b)

pattern QCon :: (T.Text, a) -> T.Text -> [QType a] -> QType a
pattern QCon q a b = Fix (Just q C.:< TConF a b)

pattern QTup :: (T.Text, a) -> [QType a] -> QType a
pattern QTup q xs = Fix (Just q C.:< TTupF xs)

pattern QRec :: (T.Text, a) -> [(T.Text, QType a)] -> QType a
pattern QRec q xs = Fix (Just q C.:< TRecF xs)

pattern QArr :: (T.Text, a) -> QType a -> Integer -> QType a
pattern QArr q t n = Fix (Just q C.:< TArrF t n)

pattern QVar :: (T.Text, a) -> T.Text -> QType a
pattern QVar q a = Fix (Just q C.:< TVarF a)
