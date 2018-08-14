{-# Language NoImplicitPrelude #-}
{-# Language UndecidableInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language ScopedTypeVariables #-}
{-# Language PatternSynonyms #-}
{-# Language StandaloneDeriving #-}
{-# Language DeriveTraversable #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language TemplateHaskell #-}
{-# Language KindSignatures #-}
{-# Language DataKinds #-}
{-# Language TypeOperators #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}

module Jael.Liquid where

import qualified Data.TreeDiff

import Jael.Prelude
import Jael.Expr
import Jael.TH
import Jael.Matching

data EIntF e = EIntF Integer
$(mkConBoilerplate ''EIntF)

pattern EInt :: (EIntF :<: ys) => Integer -> EADT ys
pattern EInt i = VF (EIntF i)

type CoreExprCs t v = '[ ETAbsF v, ETAppF t, EAbsTF t v, EAppF, ELetTF t v , EIfF, EVarF v, EIntF, EPrimF ]

type CoreExpr t v = EADT (CoreExprCs t v)

-- cc :: [[ConsInfo t]] -> (PatternMatrix -> Integer) -> ClauseMatrix -> Either Err DecisionTree
-- selectLeftColumn :: PatternMatrix -> Integer
-- type ClauseMatrix = [(PatternVec, Action)]
-- type PatternVec = [P]
-- type Action = Integer
-- data DecisionTree = Fail | Leaf Action | Switch CaseList

-- ETAbsF v, ETAppF t, EAbsTF t v, EAppF, ELetTF t v, EIfF, EVarF v, EIntF, EPrimF m

simplifyExpr :: ExprT t P v -> CoreExpr t v
simplifyExpr = cata simplifyExpr'

class SimplifyExpr (f :: * -> *) xs where
  simplifyExpr' :: f (EADT xs) -> EADT xs

$(mkVariantInstances ''SimplifyExpr)

--------------------------------- Annotations ---------------------------------

instance (ETAbsF v :<: xs) => SimplifyExpr (ETAbsF v) xs where
  simplifyExpr' (ETAbsF v e) = ETAbs v e

instance (ETAppF t :<: xs) => SimplifyExpr (ETAppF t) xs where
  simplifyExpr' (ETAppF t e) = ETApp t e

-------------------------- Abstractions/Application --------------------------

instance SimplifyExpr (EAbsTF t P) xs where
  simplifyExpr' (EAbsTF _t _p) = undefined

instance SimplifyExpr (ELamCaseTF t P) xs where
  simplifyExpr' (ELamCaseTF _t _p) = undefined

instance SimplifyExpr (ELetTF t P) xs where
  simplifyExpr' (ELetTF _t _p) = undefined

instance SimplifyExpr EAppF xs where
  simplifyExpr' (EAppF _f _e) = undefined

-------------------------------- Conditionals --------------------------------

instance SimplifyExpr (ECaseF P) xs where
  simplifyExpr' (ECaseF _e _pes) = undefined

instance SimplifyExpr EIfF xs where
  simplifyExpr' (EIfF _b _t _e) = undefined

instance SimplifyExpr EMultiIfF xs where
  simplifyExpr' (EMultiIfF _pairs _mdef) = undefined

--------------------------------- Base cases ---------------------------------

instance (EVarF v :<: xs) => SimplifyExpr (EVarF v) xs where
  simplifyExpr' (EVarF v) = EVar v

instance (EIntF :<: xs) => SimplifyExpr ELitF xs where
  simplifyExpr' (ELitF (LInt i)) = EInt (intValue i)

instance SimplifyExpr EPrimF xs where
  simplifyExpr' (EPrimF _m) = undefined

------------------------------- Data Structures -------------------------------

instance SimplifyExpr ETupF xs where
  simplifyExpr' (ETupF _es) = undefined

instance SimplifyExpr EArrF xs where
  simplifyExpr' (EArrF _es) = undefined

------------------------------ Record Operations ------------------------------

instance SimplifyExpr ERecF xs where
  simplifyExpr' (ERecF _e) = undefined

instance SimplifyExpr ERecEmptyF xs where
  simplifyExpr' ERecEmptyF = undefined

instance SimplifyExpr ERecExtendF xs where
  simplifyExpr' (ERecExtendF _l _e _r) = undefined

instance SimplifyExpr ERecUpdateF xs where
  simplifyExpr' (ERecUpdateF _l _e _r) = undefined

instance SimplifyExpr ERecRenameF xs where
  simplifyExpr' (ERecRenameF _new _old _r) = undefined

instance SimplifyExpr ERecRemoveF xs where
  simplifyExpr' (ERecRemoveF _e _l) = undefined

instance SimplifyExpr ERecSelectF xs where
  simplifyExpr' (ERecSelectF _e _l) = undefined
