{-# Language PatternSynonyms #-}
{-# Language RankNTypes #-}

module Jael.New.QType where

import qualified Data.Text as T
import qualified Control.Comonad.Trans.Cofree as C
import Jael.New.Type

type Refinement e = Maybe (T.Text, e)
type QTypeF v e = C.CofreeF (TypeF v) (Refinement e)
type QType v e = Fix (QTypeF v e)

unQType :: QTypeF v e f -> TypeF v f
unQType (_ C.:< t) = t

pattern UQAllF :: [v] -> a -> QTypeF v e a
pattern UQAllF a b = Nothing C.:< TAllF a b

pattern UQFunF :: Maybe v -> a -> a -> QTypeF v e a
pattern UQFunF n a b = Nothing C.:< TFunF n a b

pattern UQFunF' :: a -> a -> QTypeF v e a
pattern UQFunF' a b = Nothing C.:< TFunF Nothing a b

pattern UQConF :: T.Text -> [a] -> QTypeF v e a
pattern UQConF a b = Nothing C.:< TConF a b

pattern UQTupF :: [a] -> QTypeF v e a
pattern UQTupF xs = Nothing C.:< TTupF xs

pattern UQRecF :: Row' v a -> QTypeF v e a
pattern UQRecF r = Nothing C.:< TRecF r

pattern UQArrF :: a -> Integer -> QTypeF v e a
pattern UQArrF t n = Nothing C.:< TArrF t n

pattern UQVarF :: v -> QTypeF v e a
pattern UQVarF a = Nothing C.:< TVarF a

pattern UQAll :: [v] -> QType v e -> QType v e
pattern UQAll a b = Fix (Nothing C.:< TAllF a b)

pattern UQFun :: Maybe v -> QType v e -> QType v e -> QType v e
pattern UQFun n a b = Fix (Nothing C.:< TFunF n a b)

pattern UQFun' :: QType v e -> QType v e -> QType v e
pattern UQFun' a b = Fix (Nothing C.:< TFunF Nothing a b)

pattern UQCon :: T.Text -> [QType v e] -> QType v e
pattern UQCon a b = Fix (Nothing C.:< TConF a b)

pattern UQTup :: [QType v e] -> QType v e
pattern UQTup xs = Fix (Nothing C.:< TTupF xs)

pattern UQRec :: Row' v (QType v e) -> QType v e
pattern UQRec r = Fix (Nothing C.:< TRecF r)

pattern UQArr :: QType v e -> Integer -> QType v e
pattern UQArr t n = Fix (Nothing C.:< TArrF t n)

pattern UQVar :: v -> QType v e
pattern UQVar a = Fix (Nothing C.:< TVarF a)

pattern QAll :: (T.Text, e) -> [v] -> QType v e -> QType v e
pattern QAll q a b = Fix (Just q C.:< TAllF a b)

--pattern QFun :: (T.Text, e) -> Maybe v -> QType v e -> QType v e -> QType v e
--pattern QFun q n a b = Fix (Just q C.:< TFunF n a b)

--pattern QFun' :: (T.Text, e) -> QType v e -> QType v e -> QType v e
--pattern QFun' q a b = Fix (Just q C.:< TFunF Nothing a b)

pattern QCon :: (T.Text, e) -> T.Text -> [QType v e] -> QType v e
pattern QCon q a b = Fix (Just q C.:< TConF a b)

pattern QTup :: (T.Text, e) -> [QType v e] -> QType v e
pattern QTup q xs = Fix (Just q C.:< TTupF xs)

pattern QRec :: (T.Text, e) -> Row' v (QType v e) -> QType v e
pattern QRec q r = Fix (Just q C.:< TRecF r)

pattern QArr :: (T.Text, e) -> QType v e -> Integer -> QType v e
pattern QArr q t n = Fix (Just q C.:< TArrF t n)

pattern QVar :: (T.Text, e) -> v -> QType v e
pattern QVar q a = Fix (Just q C.:< TVarF a)
