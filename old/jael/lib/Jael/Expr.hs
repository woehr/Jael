{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Jael.Expr where

import qualified Data.Set      as S
import qualified Data.Text     as T
import qualified Data.TreeDiff

import Jael.Classes
import Jael.Prelude
import Jael.TH

type Bind = T.Text
type Constructor = T.Text
type Label = T.Text

falseConst, trueConst :: IsString a => a
falseConst = "false"
trueConst = "true"

data IntFormat = BinInt | OctInt | HexInt | DecInt
                 deriving (Data, Eq, Generic, Show, Typeable)

instance Data.TreeDiff.ToExpr IntFormat

data JInt = JInt
  { intFormat :: IntFormat
  , intValue  :: Integer
  , intLength :: Integer
  } deriving (Data, Eq, Generic, Show, Typeable)

instance Data.TreeDiff.ToExpr JInt

defaultInt :: Integer -> JInt
defaultInt i =
  JInt
    { intFormat = DecInt
    , intValue = i
    , intLength = if i >= 0 then length (show i) else length (show i) - 1
    }

negateJInt :: JInt -> JInt
negateJInt j@JInt{..} = j{intValue = -intValue}

newtype Literal
  = LInt JInt
  deriving (Data, Eq, Generic, Typeable)

instance Data.TreeDiff.ToExpr Literal

instance Show Literal where
  show (LInt j) = show j

data Operator
  = OpNot
  | OpIff
  | OpImp
  | OpOr
  | OpAnd
  | OpEq
  | OpNe
  | OpGt
  | OpGe
  | OpLt
  | OpLe
  | OpAdd
  | OpSub
  | OpTimes
  | OpDiv
  | OpMod
  deriving (Data, Eq, Generic, Show, Typeable)

instance Data.TreeDiff.ToExpr Operator

data Primitive
  = PrimArrEmpty
  | PrimArrSet
  | PrimArrUnset
  | PrimArrAt
  | PrimImposs
  | PrimUnimpl
  deriving (Data, Eq, Generic, Show, Typeable)

instance Data.TreeDiff.ToExpr Primitive

prims :: [(T.Text, Primitive)]
prims = [ ("arrEmpty", PrimArrEmpty)
        , ("arrSet", PrimArrSet)
        , ("arrUnset", PrimArrUnset)
        , ("arrAt", PrimArrAt)
        , ("impossible", PrimImposs)
        , ("unimplemented", PrimUnimpl)
        ]

data ETAbsF v e = ETAbsF v e
$(mkConBoilerplate ''ETAbsF)

data ETAppF t e = ETAppF t e
$(mkConBoilerplate ''ETAppF)

data EAbsF p e = EAbsF [p] e
$(mkConBoilerplate ''EAbsF)

data EAbsTF t p e = EAbsTF [(p, t)] e
$(mkConBoilerplate ''EAbsTF)

data ELamCaseF p e = ELamCaseF [(p, e)]
$(mkConBoilerplate ''ELamCaseF)

data ELamCaseTF t p e = ELamCaseTF t [(p, e)]
$(mkConBoilerplate ''ELamCaseTF)

data EAppF e = EAppF e [e]
$(mkConBoilerplate ''EAppF)

data ETupF e = ETupF   [e]
$(mkConBoilerplate ''ETupF )

data EArrF e = EArrF   [e]
$(mkConBoilerplate ''EArrF )

data ELetF p e = ELetF [(p, e)] e
$(mkConBoilerplate ''ELetF)

data ELetTF t p e = ELetTF [((p, t), e)] e
$(mkConBoilerplate ''ELetTF)

data ERecF e = ERecF e
$(mkConBoilerplate ''ERecF)

data ERecEmptyF e = ERecEmptyF
$(mkConBoilerplate ''ERecEmptyF)

data ERecUpdateF e = ERecUpdateF Label e e
$(mkConBoilerplate ''ERecUpdateF)

data ERecExtendF e = ERecExtendF Label e e
$(mkConBoilerplate ''ERecExtendF)

data ERecRenameF e = ERecRenameF Label Label e
$(mkConBoilerplate ''ERecRenameF)

data ERecRemoveF e = ERecRemoveF e Label
$(mkConBoilerplate ''ERecRemoveF)

data ERecSelectF e = ERecSelectF e Label
$(mkConBoilerplate ''ERecSelectF)

data ECaseF p e = ECaseF e [(p, e)]
$(mkConBoilerplate ''ECaseF)

data EIfF e = EIfF e e e
$(mkConBoilerplate ''EIfF)

data EMultiIfF e = EMultiIfF [Pair e] (Maybe e)
$(mkConBoilerplate ''EMultiIfF)

data EVarF v e = EVarF v
$(mkConBoilerplate ''EVarF)

data ELitF e = ELitF Literal
$(mkConBoilerplate ''ELitF)

data EPrimF e = EPrimF Primitive
$(mkConBoilerplate ''EPrimF)

data EOpF e = EOpF Operator
$(mkConBoilerplate ''EOpF)

type ExprCs p v =
  '[ EAbsF p, ELamCaseF p, EAppF, ETupF, EArrF, ELetF p, ERecF, ERecEmptyF
   , ERecUpdateF, ERecExtendF, ERecRenameF, ERecRemoveF, ERecSelectF, ECaseF p
   , EIfF, EMultiIfF, EVarF v, ELitF, EOpF
   ]

type ExprTCs t p v =
  '[ ETAbsF v, ETAppF t, EAbsTF t p, ELamCaseTF t p, EAppF, ETupF, EArrF
   , ELetTF t p, ERecF, ERecEmptyF, ERecUpdateF, ERecExtendF, ERecRenameF
   , ERecRemoveF, ERecSelectF, ECaseF p, EIfF, EMultiIfF, EVarF v
   , ELitF, EOpF
   ]

type ExprF p v = VariantF (ExprCs p v)
type Expr p v = EADT (ExprCs p v)
type ExprF' p = ExprF p T.Text
type Expr' p = Expr p T.Text

type ExprTF t p v = VariantF (ExprTCs t p v)
type ExprT t p v = EADT (ExprTCs t p v)
type ExprTF' t p = ExprTF t p T.Text
type ExprT' t p = ExprT t p T.Text

pattern ETAbs :: (ETAbsF v :<: ys) => v -> EADT ys -> EADT ys
pattern ETAbs v e = VF (ETAbsF v e)

pattern ETApp :: (ETAppF t :<: ys) => t -> EADT ys -> EADT ys
pattern ETApp t e = VF (ETAppF t e)

pattern EAbs :: (EAbsF p :<: ys) => [p] -> EADT ys -> EADT ys
pattern EAbs ps e = VF (EAbsF ps e)

pattern EAbsT :: (EAbsTF t p :<: ys) => [(p, t)] -> EADT ys -> EADT ys
pattern EAbsT ps e = VF (EAbsTF ps e)

pattern ELamCase :: (ELamCaseF p :<: ys) => [(p, EADT ys)] -> EADT ys
pattern ELamCase alts = VF (ELamCaseF alts)

pattern ELamCaseT :: (ELamCaseTF t p :<: ys) => t -> [(p, EADT ys)] -> EADT ys
pattern ELamCaseT t alts = VF (ELamCaseTF t alts)

pattern EApp :: (EAppF :<: ys) => EADT ys -> [EADT ys] -> EADT ys
pattern EApp f as = VF (EAppF f as)

pattern ETup :: (ETupF :<: ys) => [EADT ys] -> EADT ys
pattern ETup es = VF (ETupF es)

pattern EArr :: (EArrF :<: ys) => [EADT ys] -> EADT ys
pattern EArr es = VF (EArrF es)

pattern ERec :: (ERecF :<: ys) => EADT ys -> EADT ys
pattern ERec e = VF (ERecF e)

pattern ERecEmpty :: (ERecEmptyF :<: ys) => EADT ys
pattern ERecEmpty = VF ERecEmptyF

pattern ERecExtend :: (ERecExtendF :<: ys) => Label -> EADT ys -> EADT ys -> EADT ys
pattern ERecExtend l e r = VF (ERecExtendF l e r)

pattern ERecUpdate :: (ERecUpdateF :<: ys) => Label -> EADT ys -> EADT ys -> EADT ys
pattern ERecUpdate l e r = VF (ERecUpdateF l e r)

pattern ERecRename :: (ERecRenameF :<: ys) => Label -> Label -> EADT ys -> EADT ys
pattern ERecRename to from e = VF (ERecRenameF to from e)

pattern ERecRemove :: (ERecRemoveF :<: ys) => EADT ys -> Label -> EADT ys
pattern ERecRemove e l = VF (ERecRemoveF e l)

pattern ERecSelect :: (ERecSelectF :<: ys) => EADT ys -> Label -> EADT ys
pattern ERecSelect e l = VF (ERecSelectF e l)

pattern ECase :: (ECaseF p :<: ys) => EADT ys -> [(p, EADT ys)] -> EADT ys
pattern ECase e ps = VF (ECaseF e ps)

pattern EIf :: (EIfF :<: ys) => EADT ys -> EADT ys -> EADT ys -> EADT ys
pattern EIf b t e = VF (EIfF b t e)

pattern EMultiIf :: (EMultiIfF :<: ys) => [Pair (EADT ys)] -> Maybe (EADT ys) -> EADT ys
pattern EMultiIf xs me = VF (EMultiIfF xs me)

pattern ELet :: (ELetF p :<: ys) => [(p, EADT ys)] -> EADT ys -> EADT ys
pattern ELet es e = VF (ELetF es e)

pattern ELetT :: (ELetTF t p :<: ys) => [((p, t), EADT ys)] -> EADT ys -> EADT ys
pattern ELetT es e = VF (ELetTF es e)

pattern EVar :: (EVarF v :<: ys) => v -> EADT ys
pattern EVar v = VF (EVarF v)

pattern EPrim :: (EPrimF :<: ys) => Primitive -> EADT ys
pattern EPrim p = VF (EPrimF p)

pattern ELit :: (ELitF :<: ys) => Literal -> EADT ys
pattern ELit l = VF (ELitF l)

pattern EOp :: (EOpF :<: ys) => Operator -> EADT ys
pattern EOp o = VF (EOpF o)

eInt :: (ELitF :<: ys) => JInt -> EADT ys
eInt i = ELit (LInt i)

eFalse :: (EVarF T.Text :<: ys) => EADT ys
eFalse = EVar @T.Text falseConst

eTrue :: (EVarF T.Text :<: ys) => EADT ys
eTrue = EVar @T.Text trueConst

instance (Ord v) => VarsFree (ETAbsF v) v where
  freeVars' (ETAbsF v e) = e S.\\ S.singleton v

instance (Ord v, LiftToVarsFree t v) => VarsFree (ETAppF t) v where
  freeVars' (ETAppF t e) = ftv t `S.union` e

instance (Ord v, LiftToVarsFree t v) => VarsFree (EAbsTF t p) v where
  freeVars' (EAbsTF ps e) = S.unions $ e : fmap (ftv . snd) ps

instance (Ord v, LiftToVarsFree t v) => VarsFree (ELamCaseTF t p) v where
  freeVars' (ELamCaseTF t cs) = S.unions $ ftv t : fmap snd cs

instance (Ord v, LiftToVarsFree t v) => VarsFree (ELetTF t p) v where
  freeVars' (ELetTF ptes e) = S.unions $ e : fmap snd ptes <> fmap (ftv . snd . fst) ptes

class OverExpr (f :: * -> *) t t' p p' v v' ys where
  overExpr' :: (t -> t') -> (p -> p') -> (v -> v') -> f (EADT ys) -> EADT ys

$(mkVariantInstances ''OverExpr)

instance {-# Overlappable #-} (f :<: ys) => OverExpr f t t' p p' v v' ys where
  overExpr' _ _ _ = VF

instance (ETAbsF v' :<: ys) => OverExpr (ETAbsF v) t t' p p' v v' ys where
  overExpr' _ _ h (ETAbsF v e) = ETAbs (h v) e

instance (ETAppF t' :<: ys) => OverExpr (ETAppF t) t t' p p' v v' ys where
  overExpr' f _ _ (ETAppF t e) = ETApp (f t) e

instance (EVarF v' :<: ys) => OverExpr (EVarF v) t t' p p' v v' ys where
  overExpr' _ _ h (EVarF v) = EVar (h v)

instance (ECaseF p' :<: ys) => OverExpr (ECaseF p) t t' p p' v v' ys where
  overExpr' _ g _ (ECaseF e cs) = ECase e (fmap (first g) cs)

instance (ELetTF t' p' :<: ys) => OverExpr (ELetTF t p) t t' p p' v v' ys where
  overExpr' f g _ (ELetTF ptes e) = ELetT (fmap (first $ bimap g f) ptes) e

instance (EAbsTF t' p' :<: ys) => OverExpr (EAbsTF t p) t t' p p' v v' ys where
  overExpr' f g _ (EAbsTF pts e) = EAbsT (fmap (bimap g f) pts) e

instance (ELamCaseTF t' p' :<: ys) => OverExpr (ELamCaseTF t p) t t' p p' v v' ys where
  overExpr' f g _ (ELamCaseTF t pes) = ELamCaseT (f t) (fmap (first g) pes)

instance (ELetF p' :<: ys) => OverExpr (ELetF p) t t' p p' v v' ys where
  overExpr' _ g _ (ELetF pes e) = ELet (fmap (first g) pes) e

instance (EAbsF p' :<: ys) => OverExpr (EAbsF p) t t' p p' v v' ys where
  overExpr' _ g _ (EAbsF pes e) = EAbs (fmap g pes) e

instance (ELamCaseF p' :<: ys) => OverExpr (ELamCaseF p) t t' p p' v v' ys where
  overExpr' _ g _ (ELamCaseF pes) = ELamCase (fmap (first g) pes)

overExpr :: (Functor (VariantF xs), OverExpr (VariantF xs) t t' p p' v v' ys)
         => (t -> t') -> (p -> p') -> (v -> v') -> EADT xs -> EADT ys
overExpr f g h = cata (overExpr' f g h)
