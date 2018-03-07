{-# Language DeriveFunctor #-}
{-# Language DeriveTraversable #-}
{-# Language TemplateHaskell #-}

module Jael.New.Core where

--import qualified Control.Comonad.Trans.Cofree as C
--import qualified Data.Bimap as M
import qualified Data.Text as T

import Jael.New.Expr
--import Jael.New.Type
--import Jael.New.Parser

data PrimOp = PrimOp Primitive
            | Impossible
            | Assert T.Text
            | Error T.Text
  deriving (Eq, Show)

data RecBind v t c = RecBind v t c
  deriving (Eq, Foldable, Functor, Show, Traversable)

$(deriveEq1   ''RecBind)
$(deriveShow1 ''RecBind)

newtype CoreLit = CLitInt Integer
  deriving (Eq, Show)

data CaseAlt v
  = CAltData T.Text [v]
  | CAltLit Literal
  deriving (Eq, Functor, Show)

-- CoreF v t c
-- v := variables (program and type)
-- t := types
-- d := data constructors
data CoreF v t d c
  = CTAbsF v c
  | CTAppF c t
  | CAbsF  v t c
  | CAppF  c c
  | CLetF  v t c c
  | CLetRecF [RecBind v t c]
  | CCaseF
     { ccaseScrutinee :: c
     , ccaseAlts      :: [(CaseAlt v, c)]
     , ccaseDefault   :: c
     }
  | CConF  d [c]
  | CPrimF PrimOp
  | CLitF Literal
  | CVarF v
  deriving (Eq, Foldable, Functor, Show, Traversable)

$(deriveEq1   ''CoreF)
$(deriveShow1 ''CoreF)
