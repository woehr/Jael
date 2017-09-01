{-# Language PatternSynonyms #-}

module Jael.New.QType
where

import qualified Data.Text as T
import Jael.New.Types

type QType e   = Cofree TypeF (Maybe (T.Text, e))

pattern UQAll :: [T.Text] -> QType a -> QType a
pattern UQAll a b = Nothing :< TAllF a b

pattern UQFun :: QType a -> QType a -> QType a
pattern UQFun a b = Nothing :< TFunF a b

pattern UQCon :: T.Text -> [QType a] -> QType a
pattern UQCon a b = Nothing :< TConF a b

pattern UQTup :: [QType a] -> QType a
pattern UQTup xs = Nothing :< TTupF xs

pattern UQRec :: [(T.Text, QType a)] -> QType a
pattern UQRec xs = Nothing :< TRecF xs

pattern UQArr :: QType a -> Integer -> QType a
pattern UQArr t n = Nothing :< TArrF t n

pattern UQVar :: T.Text -> QType a
pattern UQVar a = Nothing :< TVarF a

pattern QAll :: (T.Text, a) -> [T.Text] -> QType a -> QType a
pattern QAll q a b = Just q :< TAllF a b

pattern QFun :: (T.Text, a) -> QType a -> QType a -> QType a
pattern QFun q a b = Just q :< TFunF a b

pattern QCon :: (T.Text, a) -> T.Text -> [QType a] -> QType a
pattern QCon q a b = Just q :< TConF a b

pattern QTup :: (T.Text, a) -> [QType a] -> QType a
pattern QTup q xs = Just q :< TTupF xs

pattern QRec :: (T.Text, a) -> [(T.Text, QType a)] -> QType a
pattern QRec q xs = Just q :< TRecF xs

pattern QArr :: (T.Text, a) -> QType a -> Integer -> QType a
pattern QArr q t n = Just q :< TArrF t n

pattern QVar :: (T.Text, a) -> T.Text -> QType a
pattern QVar q a = Just q :< TVarF a
