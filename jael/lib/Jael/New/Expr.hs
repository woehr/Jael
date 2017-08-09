{-# Language DeriveFunctor #-}
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}
{-# Language TemplateHaskell #-}

module Jael.New.Expr
where

import qualified Data.Text as T
import           Data.Eq.Deriving (deriveEq1)
import           Text.Show.Deriving (deriveShow1)

import Jael.New.Types

data Constant = CInt Integer
              | CChar Char
              deriving (Eq, Show)

data BuiltinFunc = BFIff
                 | BFImp
                 | BFOr
                 | BFAnd
                 | BFEq
                 | BFNe
                 | BFGt
                 | BFGe
                 | BFLt
                 | BFLe
                 | BFAdd
                 | BFSub
                 | BFTimes
                 | BFDiv
                 | BFMod
                 | BFCat
                 | BFNot
                 deriving (Eq, Show)

symbolOf :: BuiltinFunc -> String
symbolOf BFIff   = "<->"
symbolOf BFImp   = "-->"
symbolOf BFOr    = "||"
symbolOf BFAnd   = "&&"
symbolOf BFEq    = "=="
symbolOf BFNe    = "!="
symbolOf BFGt    = ">"
symbolOf BFGe    = ">="
symbolOf BFLt    = "<"
symbolOf BFLe    = "<="
symbolOf BFAdd   = "+"
symbolOf BFSub   = "-"
symbolOf BFTimes = "*"
symbolOf BFDiv   = "/"
symbolOf BFMod   = "%"
symbolOf BFCat   = "#"
symbolOf BFNot   = "!"

data PatternF p = PPatF T.Text [p]    -- Constructed with sub-patterns
                | POrF [p]            -- Or pattern
                | PRecF [(T.Text, p)] -- Record pattern
                | PArrF [p]           -- Array with sub-patterns
                | PBindF T.Text       -- Placeholder variable
                | PConstF Constant    -- Ints, Chars
                | PWildF              -- Single wildcard : _
                | PMultiWildF         -- Multi wildcard  : ...
                deriving (Eq, Functor, Show)

type Pattern = Fix PatternF

$(deriveEq1   ''PatternF)
$(deriveShow1 ''PatternF)

data ExprF e = EAbsF T.Text (Maybe Type) e
             | EAppF e e
             | ETupF [e]
             | ERecF [(T.Text, e)]
             | ECaseF e [(Pattern, e)]
             | EIfF e e e
             | ELetF [(Pattern, e)] e
             | EVarF T.Text
             | EConstF Constant
             | EBuiltinF BuiltinFunc
             deriving (Eq, Functor, Show)

type Expr = Fix ExprF

$(deriveEq1   ''ExprF)
$(deriveShow1 ''ExprF)

data CoreF c = CAbsF T.Text Type c
             | CLetF T.Text Type c c
             | CVarF T.Text
             deriving (Eq, Functor, Show)

type Core = Fix CoreF

$(deriveEq1   ''CoreF)
$(deriveShow1 ''CoreF)

-- Patterns for PatternF
pattern PPat :: T.Text -> [Pattern] -> Pattern
pattern PPat x ps = Fix (PPatF x ps)

pattern POr :: [Pattern] -> Pattern
pattern POr ps = Fix (POrF ps)

pattern PRec :: [(T.Text, Pattern)] -> Pattern
pattern PRec ps = Fix (PRecF ps)

pattern PArr :: [Pattern] -> Pattern
pattern PArr ps = Fix (PArrF ps)

pattern PBind :: T.Text -> Pattern
pattern PBind x = Fix (PBindF x)

pattern PConst :: Constant -> Pattern
pattern PConst x = Fix (PConstF x)

pattern PWild :: Pattern
pattern PWild = Fix PWildF

pattern PMultiWild :: Pattern
pattern PMultiWild = Fix PMultiWildF

-- Patterns for ExprF

pattern EAbs :: T.Text -> Maybe Type -> Expr -> Expr
pattern EAbs a b c = Fix (EAbsF a b c)

pattern EApp :: Expr -> Expr -> Expr
pattern EApp e1 e2 = Fix (EAppF e1 e2)

pattern ETup :: [Expr] -> Expr
pattern ETup es = Fix (ETupF es)

pattern ERec :: [(T.Text, Expr)] -> Expr
pattern ERec es = Fix (ERecF es)

pattern ECase :: Expr -> [(Pattern, Expr)] -> Expr
pattern ECase e ps = Fix (ECaseF e ps)

pattern EIf :: Expr -> Expr -> Expr -> Expr
pattern EIf b t e = Fix (EIfF b t e)

pattern ELet :: [(Pattern, Expr)] -> Expr -> Expr
pattern ELet es e = Fix (ELetF es e)

pattern EVar :: T.Text -> Expr
pattern EVar x = Fix (EVarF x)

pattern EConst :: Constant -> Expr
pattern EConst x = Fix (EConstF x)

pattern EBuiltin :: BuiltinFunc -> Expr
pattern EBuiltin x = Fix (EBuiltinF x)

pattern EInt :: Integer -> Expr
pattern EInt x = EConst (CInt x)

pattern EChar :: Char -> Expr
pattern EChar x = EConst (CChar x)

-- Patterns for CoreF

pattern CAbs :: T.Text -> Type -> Core -> Core
pattern CAbs v t c = Fix (CAbsF v t c)

pattern CVar :: T.Text -> Core
pattern CVar v = Fix (CVarF v)
