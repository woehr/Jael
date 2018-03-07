{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module Jael.New.ExprToCore where

import           Jael.New.Core
import           Jael.New.Expr
import           Jael.New.HMInfer
import           Jael.New.QType
import           Jael.New.Type

import qualified Control.Comonad.Trans.Cofree as C
import qualified Data.Map                     as M
import qualified Data.Text                    as T

type Tag = Integer

type CoreExpr = Cofree (CoreF Integer Type T.Text) (Maybe TypedExpr)
type CoreQType = QType Integer CoreExpr

type CoreExpr' =
  Cofree (CoreF Integer CoreQType T.Text) (Maybe TypedExpr, CoreQType)

--CTAbsF v c
--CTAppF c t
--CAbsF  v t c

pattern CApp :: CoreExpr -> CoreExpr -> CoreExpr
pattern CApp f a = Nothing :< CAppF f a

--CConF  d [c]
--CLetF  v t c c
--CLetRecF [RecBind v t c]
--CSwitchF
-- { cswitchScrutinee :: c
-- , cswitchAlts      :: [(SwitchAlt, c)]
-- , cswitchDefault   :: c
-- }

pattern CPrim :: PrimOp -> CoreExpr
pattern CPrim op = Nothing :< CPrimF op

pattern CLit :: Literal -> CoreExpr
pattern CLit l = Nothing :< CLitF l

pattern CVar :: Integer -> CoreExpr
pattern CVar i = Nothing :< CVarF i

data ToCoreS = ToCoreS
  { tcsFreshVar :: Integer
  , tcsNames    :: M.Map Integer T.Text
  } deriving (Eq, Show)

type ToCoreM = StateT ToCoreS Identity

initState :: ToCoreS
initState = ToCoreS { tcsFreshVar = 0, tcsNames = M.empty }

exprToCore :: TypedExpr -> CoreExpr
exprToCore = cata alg where
  alg :: C.CofreeF (ExprF Type TypedPat T.Text) s CoreExpr -> CoreExpr
  alg (_ C.:< ETAbsF _vs _e)         = undefined
  alg (_ C.:< ETAppF _ts _e)         = undefined
  alg (_ C.:< EAbsF _pts _e)         = undefined
  alg (_ C.:< ELamCaseF _t _pes)     = undefined

  alg (_ C.:< EAppF _e _es)          = undefined
  alg (_ C.:< ETupF _es)             = undefined
  alg (_ C.:< EArrF _es)             = undefined

  alg (_ C.:< ELetF _ptes _e)        = undefined

  alg (_ C.:< ERecF)                 = undefined
  alg (_ C.:< ERecUpdateF _l _e _r)  = undefined
  alg (_ C.:< ERecExtendF _l _e _r)  = undefined
  alg (_ C.:< ERecRenameF _l' _l _e) = undefined
  alg (_ C.:< ERecRemoveF _e _l)     = undefined
  alg (_ C.:< ERecSelectF _e _l)     = undefined

  alg (_ C.:< ECaseF _e _pes)        = undefined
  alg (_ C.:< EIfF _b _t _e)         = undefined
  alg (_ C.:< EMultiIfF _gs _me)     = undefined

  alg (_ C.:< EVarF _v)              = undefined
  alg (_ C.:< ELitF _l)              = undefined
  alg (_ C.:< EPrimF _p)             = undefined

falseAlt, trueAlt :: CaseAlt a
falseAlt = CAltData "false" []
trueAlt  = CAltData "true"  []

coreIf :: CoreExpr -> CoreExpr -> CoreExpr -> CoreExpr
coreIf b fls tr =
  Nothing :< CCaseF
    { ccaseScrutinee = b
    , ccaseAlts      = [(trueAlt, tr)]
    , ccaseDefault   = fls
    }

coreBinApp :: Primitive -> CoreExpr -> CoreExpr -> CoreExpr
coreBinApp op lft = CApp (CApp (CPrim $ PrimOp op) lft)

coreEq :: CoreExpr -> CoreExpr -> CoreExpr
coreEq = coreBinApp OpEq
