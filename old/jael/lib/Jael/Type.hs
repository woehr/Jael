{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Jael.Type where

import qualified Data.Map      as M
import qualified Data.Set      as S
import qualified Data.Text     as T
import qualified Data.TreeDiff

import Jael.Classes
import Jael.Prelude
import Jael.TH


data TRowEmptyF r = TRowEmptyF
$(mkConBoilerplate ''TRowEmptyF)

data TRowVarF v r = TRowVarF v
$(mkConBoilerplate ''TRowVarF)

data TRowExtF b t r = TRowExtF (b, t) r
$(mkConBoilerplate ''TRowExtF)

pattern TRowEmpty :: forall xs. (TRowEmptyF :<: xs) => EADT xs
pattern TRowEmpty = VF TRowEmptyF

pattern TRowVar :: forall v xs. (TRowVarF v :<: xs) => v -> EADT xs
pattern TRowVar v = VF (TRowVarF v)

pattern TRowExt :: forall b t xs. (TRowExtF b t :<: xs) => (b, t) -> EADT xs -> EADT xs
pattern TRowExt x r = VF (TRowExtF x r)

class OverRowT (f :: * -> *) a b ys where
  overRowT' :: (a -> b) -> f (EADT ys) -> EADT ys

$(mkVariantInstances ''OverRowT)

instance {-# Overlappable #-} (f :<: ys) => OverRowT f a b ys where
  overRowT' _ = VF

instance (TRowExtF b t' :<: ys) => OverRowT (TRowExtF b t) t t' ys where
  overRowT' f (TRowExtF (b, t) x) = VF (TRowExtF (b, f t) x)

overRowT :: (Functor (VariantF xs), OverRowT (VariantF xs) a b ys)
         => (a -> b) -> EADT xs -> EADT ys
overRowT f = cata (overRowT' f)

type RowCs v b t = '[TRowEmptyF, TRowVarF v, TRowExtF b t]

type RowF  v b t = VariantF (RowCs v b t)
type Row   v b t = EADT (RowCs v b t)

data TAllF v t = TAllF v t
$(mkConBoilerplate ''TAllF)

data TFunF b t = TFunF (Maybe b) t t
$(mkConBoilerplate ''TFunF)

data TConF b t = TConF b [t]
$(mkConBoilerplate ''TConF)

data TTupF t = TTupF [t]
$(mkConBoilerplate ''TTupF)

data TArrF t = TArrF t Integer
$(mkConBoilerplate ''TArrF)

data TVarF v t = TVarF v
$(mkConBoilerplate ''TVarF)

data TRecF v b t = TRecF (Row v b t)
  deriving (Eq, Generic, Show, Typeable)

instance Functor (TRecF v b) where
  fmap f (TRecF r) = TRecF (overRowT f r)

instance Foldable (TRecF v b) where
  foldr f acc (TRecF (TRowExt (_::b, t) r)) = foldr f (f t acc) (TRecF r)
  foldr _ acc (TRecF _)                     = acc

instance Traversable (TRecF v b) where
  traverse _f _as = error "Traversable for TRecF unimplemented"

instance forall v b. (Eq b, Eq v) => Eq1 (TRecF v b) where
  liftEq f (TRecF (TRowExt (b::b, t) r)) (TRecF (TRowExt (b'::b, t') r')) =
    b == b' && f t t' && liftEq f (TRecF r) (TRecF r')
  liftEq _ (TRecF (TRowVar (v1::v))) (TRecF (TRowVar (v2::v))) = v1 == v2
  liftEq _ (TRecF TRowEmpty) (TRecF TRowEmpty) = True
  liftEq _ _ _ = False

class RowLiftShowsPrec (f :: * -> *) a where
  rowLiftShowsPrec' :: (a -> ShowS) -> f ShowS -> ShowS

$(mkVariantInstances ''RowLiftShowsPrec)

instance RowLiftShowsPrec TRowEmptyF a where
  rowLiftShowsPrec' _ TRowEmptyF x = "(Fix " <> show (TRowEmptyF @()) <> x <> ")"

instance (Show v) => RowLiftShowsPrec (TRowVarF v) a where
  rowLiftShowsPrec' _ (TRowVarF v) x = "(Fix " <> show (TRowVarF @v @() v) <> x <> ")"

instance (Show b) => RowLiftShowsPrec (TRowExtF b t) t where
  rowLiftShowsPrec' f (TRowExtF (b, t) s) x =
    "(Fix (TRowExtF (" <> show b <> ", " <> f t "" <> ") " <> s x <> "))"

rowLiftShowsPrec :: (Show b, Show v) => (t -> ShowS) -> Row v b t -> ShowS
rowLiftShowsPrec f = cata (rowLiftShowsPrec' f)

instance (Show b, Show v) => Show1 (TRecF v b) where
  liftShowsPrec f _ i (TRecF r) = (\x -> "(TRecF " <> x <> ")") . rowLiftShowsPrec (f i) r

instance ( Data.TreeDiff.ToExpr b
         , Data.TreeDiff.ToExpr t
         , Data.TreeDiff.ToExpr v
         , Data.TreeDiff.ToExpr (Row v b t)
         ) => Data.TreeDiff.ToExpr (TRecF v b t)

--data TVariF b t = TVariF (Row b t)

type TypeCs v b = '[TAllF v, TFunF b, TConF b, TTupF, TArrF, TVarF v, TRecF v b]

type TypeF v b = VariantF (TypeCs v b)
type Type  v b = EADT (TypeCs v b)

type Type'F     = TypeF T.Text T.Text
type Type'      = Type  T.Text T.Text

type Row'F      = RowF T.Text T.Text Type'
type Row'       = Row  T.Text T.Text Type'

pattern TAll :: forall v xs. (TAllF v :<: xs) => v -> EADT xs -> EADT xs
pattern TAll v t  =  VF (TAllF v t)

pattern TFun :: forall b xs. (TFunF b :<: xs) => Maybe b -> EADT xs -> EADT xs -> EADT xs
pattern TFun mb t t' = VF (TFunF mb t t')

pattern TCon :: forall b xs. (TConF b :<: xs) => b -> [EADT xs] -> EADT xs
pattern TCon b ts  = VF (TConF b ts)

pattern TTup :: forall xs. (TTupF :<: xs) => [EADT xs] -> EADT xs
pattern TTup ts  = VF (TTupF ts)

pattern TArr :: forall xs. (TArrF :<: xs) => EADT xs -> Integer -> EADT xs
pattern TArr t i = VF (TArrF t i)

pattern TVar :: forall v xs. (TVarF v :<: xs) => v -> EADT xs
pattern TVar v = VF (TVarF v)

pattern TRec :: forall v b t xs. (t ~ EADT xs, TRecF v b :<: xs) => Row v b t -> EADT xs
pattern TRec r = VF (TRecF r)

--pattern TVari :: forall b t xs. (TVariF b :<: xs, t ~ EADT xs) => Row b t -> EADT xs
--pattern TVari r = VF (TVariF r)


instance (TAllF v' :<: ys) => VarsApply (TAllF v) v v' ys where
  varApp f (TAllF v t) = TAll (f v) t
  varAppMaybe f (TAllF v t) = maybe (Left [v]) (\x -> TAll x <$> t) (f v)

instance (TVarF v' :<: ys) => VarsApply (TVarF v) v v' ys where
  varApp f (TVarF v) = TVar (f v)
  varAppMaybe f (TVarF v) = maybe (Left [v]) (Right . TVar) (f v)

instance (TRowVarF v' :<: ys) => VarsApply (TRowVarF v) v v' ys where
  varApp f (TRowVarF v) = TRowVar (f v)
  varAppMaybe f (TRowVarF v) = maybe (Left [v]) (Right . TRowVar) (f v)


instance VarsFree (TVarF v) v where
  freeVars' (TVarF v) = S.singleton v

instance (Ord v) => VarsFree (TRecF v b) v where
  freeVars' (TRecF r) = freeVars r

instance VarsFree (TRowVarF v) v where
  freeVars' (TRowVarF v) = S.singleton v

instance (Ord v, LiftToVarsFree t v) => VarsFree (TRowExtF b t) v where
  freeVars' (TRowExtF (_, t) r) = ftv t `S.union` r

instance (Ord v) => VarsFree (TAllF v) v where
  freeVars' (TAllF v t) = S.delete v t

subTVar :: (TVarF v :<: ys, Functor (VariantF ys), SubbableADT (VariantF ys) ys)
        => (v -> Maybe (EADT ys)) -> EADT ys -> EADT ys
subTVar f = subAdt $ \case
    TVar x -> f x
    _ -> Nothing

subRVar :: (TRowVarF v :<: ys, Functor (VariantF ys), SubbableADT (VariantF ys) ys)
        => (v -> Maybe (EADT ys)) -> EADT ys -> EADT ys
subRVar f = subAdt $ \case
  TRowVar x -> f x
  _ -> Nothing

tBool :: Type'
tBool = TCon @T.Text "Bool" []

tInt :: Type'
tInt = TCon @T.Text "Int" []

mkRow :: forall v b t. [(b, t)] -> Maybe v -> Row v b t
mkRow fs (Just v) = polyRow fs v
mkRow fs Nothing  = monoRow fs

extendRow :: forall v b t. Row v b t -> [(b, t)] -> Row v b t
extendRow r = foldr TRowExt r

monoRow :: forall v b t. [(b, t)] -> Row v b t
monoRow = foldr TRowExt TRowEmpty

polyRow :: forall v b t. [(b, t)] -> v -> Row v b t
polyRow rs r = foldr TRowExt (TRowVar r) rs

decomposeRow :: forall v b t. Row v b t -> ([(b, t)], Maybe v)
decomposeRow (TRowExt r x) = first (r:) (decomposeRow x)
decomposeRow (TRowVar v)   = ([], Just v)
decomposeRow _             = ([], Nothing)

-- Sort according to the first element of the tuple while making sure elements
-- with the same name stay in the same relative order.
sortRow :: (Ord b) => Row v b t -> Row v b t
sortRow r =
  let (fs, mv) = decomposeRow r
  in mkRow (sortFst fs) mv

sortRecords :: forall v b. (Ord b) => Type v b -> Type v b
sortRecords = cata alg
  where
  alg :: TypeF v b (Type v b) -> Type v b
  alg (FV (TRecF r)) = TRec @v @b (sortRow r)
  alg x              = Fix x

generalize' :: M.Map T.Text Type' -> Type' -> Type'
generalize' env t
  | vs <- S.toList $ ftv t S.\\ ftv env
  , not (null vs)
  = foldr (TAll @T.Text) t vs
  | otherwise
  = t

arity :: forall v b. Type v b -> Integer
arity t = go t 0 where
  go (TFun (_::Maybe b) _ t2) acc = go t2 (acc + 1)
  go (TAll (_::v) t') acc         = go t' acc
  go _ acc                        = acc

argTypes :: forall v b. Type v b -> [Type v b]
argTypes t = go t [] where
  go (TFun (_::Maybe b) t1 t2) acc = t1 : go t2 acc
  go (TAll (_::v)  t') acc         = go t' acc
  go _ acc                         = acc

returnType :: forall v b. Type v b -> Type v b
returnType (TFun (_::Maybe b) _ t2) = returnType t2
returnType (TAll (_::v) t)          = returnType t
returnType t                        = t

schemeVars :: Type v t -> ([v], Type v t)
schemeVars (TAll v t) = first (v:) (schemeVars t)
schemeVars t          = ([], t)

mkSub :: Type' -> Type' -> Maybe (M.Map T.Text T.Text, M.Map T.Text T.Text)
mkSub = mkSub' (M.empty, M.empty) where
  mkSub' :: (M.Map T.Text T.Text, M.Map T.Text T.Text)
         -> Type' -> Type'
         -> Maybe (M.Map T.Text T.Text, M.Map T.Text T.Text)
  mkSub' sub (TAll (_::T.Text) v) (TAll (_::T.Text) v') = mkSub' sub v v'

  mkSub' sub (TVar a) (TVar b) =
    case M.lookup a (fst sub) of
      Just b' | b == b'   -> Just sub
              | otherwise -> Nothing
      _                   -> Just $ first (M.insert a b) sub

  mkSub' sub (TCon (n::T.Text) as) (TCon (m::T.Text) bs)
    | n == m && length as == length bs
    = foldM mkSubFold sub (zip as bs)

  mkSub' sub (TTup as) (TTup bs)
    | length as == length bs
    = foldM mkSubFold sub (zip as bs)
    | otherwise
    = Nothing

  mkSub' sub (TArr a i) (TArr a' i')
    | i == i'   = mkSub' sub a a'
    | otherwise = Nothing

  -- Record types can have duplicate labels so when checking for
  -- equivalence the first label in one type must always match to the first
  -- matching label in the other
  -- A core assumption in this function is that MultiMap retains the order
  -- of values when constructing the maps from fs and fs'
  mkSub' sub (TRec (r :: Row')) (TRec r')
    | (fs , mv ) <- decomposeRow (sortRow r )
    , (fs', mv') <- decomposeRow (sortRow r')
    , fmap fst fs == fmap fst fs'
    = do
      sub' <- foldM mkSubFold sub $ zip (fmap snd fs) (fmap snd fs')
      case (mv, mv') of
        (Nothing, Nothing) -> Just sub'
        (Just x, Just x')  -> Just $ second (M.insert x x') sub'
        _                  -> Nothing

    | otherwise
    = Nothing

  mkSub' sub (TFun (_::Maybe T.Text) a b) (TFun (_::Maybe T.Text) a' b') =
    mkSub' sub a a' >>= \sub' -> mkSub' sub' b b'

  mkSub' sub _ _ = Just sub

  mkSubFold :: (M.Map T.Text T.Text, M.Map T.Text T.Text)
            -> (Type', Type')
            -> Maybe (M.Map T.Text T.Text, M.Map T.Text T.Text)
  mkSubFold acc (a, b) = mkSub' acc a b

--alphaEq :: Type T.Text -> Type T.Text -> Bool
--alphaEq t u = case mkSub t u of
--  Nothing -> False
--  Just (s1, s2) ->
--    let t' = rapply (fmap (Row [] . Just) s2) $
--              apply (fmap TVar s1) $ sortRecords t
--        u' = sortRecords u
--     in t' == u'
