{-# Language NoImplicitPrelude #-}

module Jael.Types.Template where

import           Jael.Prelude

import qualified Language.Fixpoint.Types as F

import           Jael.Types.Ann
import           Jael.Types.Type

type PendingSubs = [(F.Symbol, F.Expr)]

data TmpltReft = TmpltReft F.Reft
               | TmpltKVar Integer PendingSubs
               | TmpltConj TmpltReft TmpltReft
               deriving (Show)

type TType = Ann TypeF TmpltReft
type Template = Scheme TType

type TemplateExpr = Ann TypeF Template
