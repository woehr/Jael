{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Jael.QType where

import qualified Control.Comonad.Trans.Cofree as C
import qualified Data.Text                    as T

import Haskus.Utils.Variant

import Jael.Prelude

type Refinement e = (T.Text, e)
type MRefinement e = Maybe (Refinement e)
type QTypeF t e = C.CofreeF t (MRefinement e)
type QType t e = Fix (QTypeF t e)
--type QType' e = QType T.Text e

--mapQType :: (v -> v') -> (e -> e') -> QTypeF v e f -> QTypeF v' e' f
--mapQType f g (mr C.:< t) = fmap (second g) mr C.:< (unfix . snd . vOpApply (Just . f)) t

--mapQTypeV :: (v -> v') -> QTypeF v e f -> QTypeF v' e f
--mapQTypeV f = mapQType f id

--mapQTypeE :: (e -> e') -> QTypeF v e f -> QTypeF v e' f
--mapQTypeE f = mapQType id f

unQType :: QTypeF t e f -> t f
unQType (_ C.:< t) = t

unrefined :: (Functor t) => QType t e -> Fix t
unrefined = hoistFix unQType

pattern QualType :: ( q ~ QType t e
                    , t ~ VariantF xs
                    , Popable (f q) (ApplyAll q xs)
                    ) => Refinement e -> f q -> q
pattern QualType r f = Fix (Just r C.:< FV f)

pattern QualTypeF :: ( q ~ QType t e
                     , t ~ VariantF xs
                     , Popable (f q) (ApplyAll q xs)
                     ) => Refinement e -> f q -> QTypeF t e q
pattern QualTypeF r f = Just r C.:< FV f

pattern UnqualType :: ( q ~ QType t e
                      , t ~ VariantF xs
                      , Popable (f q) (ApplyAll q xs)
                      ) => f q -> q
pattern UnqualType f = Fix (Nothing C.:< FV f)

pattern UnqualTypeF :: ( q ~ QType t e
                       , t ~ VariantF xs
                       , Popable (f q) (ApplyAll q xs)
                       ) => f q -> QTypeF t e q
pattern UnqualTypeF f = Nothing C.:< FV f

--pattern UQAllF :: v -> a -> QTypeF () e a
--pattern UQAllF x y = UnqualTypeF (TAllF x y)

--pattern UQFunF :: Maybe v -> a -> a -> QTypeF v e a
--pattern UQFunF n a b = Nothing C.:< FV (TFunF n a b)
--
--pattern UQFunF' :: a -> a -> QTypeF v e a
--pattern UQFunF' a b = Nothing C.:< FV (TFunF Nothing a b)
--
--pattern UQConF :: T.Text -> [a] -> QTypeF v e a
--pattern UQConF a b = Nothing C.:< FV (TConF a b)
--
--pattern UQTupF :: [a] -> QTypeF v e a
--pattern UQTupF xs = Nothing C.:< FV (TTupF xs)
--
--pattern UQRecF :: Row T.Text (Type v T.Text) -> QTypeF v e a
--pattern UQRecF r = Nothing C.:< FV (TRecF r)
--
--pattern UQArrF :: a -> Integer -> QTypeF v e a
--pattern UQArrF t n = Nothing C.:< FV (TArrF t n)
--
--pattern UQVarF :: v -> QTypeF v e a
--pattern UQVarF a = Nothing C.:< FV (TVarF a)
--
--pattern UQAll :: v -> QType v e -> QType v e
--pattern UQAll a b = Fix (Nothing C.:< FV (TAllF a b))
--
--pattern UQFun :: Maybe v -> QType v e -> QType v e -> QType v e
--pattern UQFun n a b = Fix (Nothing C.:< FV (TFunF n a b))
--
--pattern UQFun' :: QType v e -> QType v e -> QType v e
--pattern UQFun' a b = Fix (Nothing C.:< FV (TFunF Nothing a b))
--
--pattern UQCon :: T.Text -> [QType v e] -> QType v e
--pattern UQCon a b = Fix (Nothing C.:< FV (TConF a b))
--
--pattern UQTup :: [QType v e] -> QType v e
--pattern UQTup xs = Fix (Nothing C.:< FV (TTupF xs))
--
--pattern UQRec :: Row v (QType v e) -> QType v e
--pattern UQRec r = Fix (Nothing C.:< FV (TRecF r))
--
--pattern UQArr :: QType v e -> Integer -> QType v e
--pattern UQArr t n = Fix (Nothing C.:< FV (TArrF t n))
--
--pattern UQVar :: v -> QType v e
--pattern UQVar a = Fix (Nothing C.:< FV (TVarF a))
--
--pattern QAll :: (T.Text, e) -> v -> QType v e -> QType v e
--pattern QAll q a b = Fix (Just q C.:< FV (TAllF a b))
--
--pattern QFun :: (T.Text, e) -> Maybe v -> QType v e -> QType v e -> QType v e
--pattern QFun q n a b = Fix (Just q C.:< FV (TFunF n a b))
--
--pattern QFun' :: (T.Text, e) -> QType v e -> QType v e -> QType v e
--pattern QFun' q a b = Fix (Just q C.:< FV (TFunF Nothing a b))
--
--pattern QCon :: (T.Text, e) -> T.Text -> [QType v e] -> QType v e
--pattern QCon q a b = Fix (Just q C.:< FV (TConF a b))
--
--pattern QTup :: (T.Text, e) -> [QType v e] -> QType v e
--pattern QTup q xs = Fix (Just q C.:< FV (TTupF xs))
--
--pattern QRec :: (T.Text, e) -> Row v (QType v e) -> QType v e
--pattern QRec q r = Fix (Just q C.:< FV (TRecF r))
--
--pattern QArr :: (T.Text, e) -> QType v e -> Integer -> QType v e
--pattern QArr q t n = Fix (Just q C.:< FV (TArrF t n))
--
--pattern QVar :: forall v e. (T.Text, e) -> v -> QType v e
--pattern QVar q a = Fix (Just q C.:< (FV (TVarF a :: TVarF v (QType v e)) :: TypeF v T.Text (QType v e)) :: _)
