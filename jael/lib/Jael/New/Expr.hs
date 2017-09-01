{-# Language DeriveFunctor #-}
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}
{-# Language TemplateHaskell #-}
{-# Language RankNTypes #-}

module Jael.New.Expr
where

import qualified Data.Text as T

import Jael.New.Types
import Jael.New.QType

data IntFormat = BinInt | OctInt | HexInt | DecInt
               deriving (Eq, Show)

data JInt = JInt
          { intFormat :: IntFormat
          , intValue  :: Integer
          , intLength :: Integer
          } deriving (Eq, Show)

data Constant = CInt JInt
              | CChar Char
              deriving (Eq, Show)

data UnaryOp = OpNot
             deriving (Eq, Show)

data BinOp = OpIff
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
           deriving (Eq, Show)

data SizeSpec = BitSize  Integer
              | ByteSize Integer
              | KiloSize Integer
              | MegaSize Integer
              | SizedSum [SizeSpec]
              | SizeOf T.Text
              deriving (Eq, Show)

data PatternF p = PPatF T.Text [p]    -- Constructed with sub-patterns
                | PTupF [p]           -- tuples
                | POrF [p]            -- Or pattern
                | PRecF [(T.Text, p)] -- Record pattern
                | PArrF [p]           -- Array with sub-patterns
                | PBindF T.Text (Maybe p)
                | PConstF Constant    -- Ints, Chars
                | PWildF              -- Single wildcard : _
                | PMultiWildF         -- Multi wildcard  : ...
                deriving (Eq, Functor, Show)

type Pattern = Fix PatternF

$(deriveEq1   ''PatternF)
$(deriveShow1 ''PatternF)

data CaseAlt e = CaseAlt Pattern (Maybe (QType e)) e
  deriving (Eq, Functor, Show)

$(deriveEq1   ''CaseAlt)
$(deriveShow1 ''CaseAlt)

data LetBind e = LetBind Pattern (Maybe (QType e)) e
  deriving (Eq, Functor, Show)

$(deriveEq1   ''LetBind)
$(deriveShow1 ''LetBind)

data AbsBind e = AbsBind Pattern (Maybe (QType e))
  deriving (Eq, Functor, Show)

$(deriveEq1   ''AbsBind)
$(deriveShow1 ''AbsBind)

data Guarded e = Guarded e e
  deriving (Eq, Functor, Show)

$(deriveEq1   ''Guarded)
$(deriveShow1 ''Guarded)

type Label     = T.Text

data ExprF e = EAbsF [AbsBind e] e
             | ELamCaseF [CaseAlt e]

             | EAppF e [e]
             | ETupF   [e]

--             | ETAbsF T.Text e
--             | ETAppF e Type
--             | ETAnnF e (QType e)

             | ELetF [LetBind e] e

             | ERecF    [(Label, e)]   -- Record construction
             | ERecUpF  [(Label, e)] e -- Record update
             | ERecExtF e e           -- Extend second record with fields of first
             | ERecResF e Label       -- Remove label from record
             | ERecSelF e Label       -- Select label from record

             | ECaseF e [CaseAlt e]
             | EIfF e e e
             | EMultiIfF [Guarded e] (Maybe e)

             | EVarF T.Text
             | EConstF Constant
             | EUnaryOpF UnaryOp
             | EBinOpF BinOp
             deriving (Eq, Functor, Show)

type Expr = Fix ExprF

$(deriveEq1   ''ExprF)
$(deriveShow1 ''ExprF)

data CoreF c = CAbsF T.Text Type c
             | CLetF T.Text Type c c
             | CAppF c c
             | CCseF
             | CVarF T.Text
             | CCstF
             deriving (Eq, Functor, Show)

type Core = Fix CoreF

$(deriveEq1   ''CoreF)
$(deriveShow1 ''CoreF)

-- Patterns for PatternF
pattern PPat :: T.Text -> [Pattern] -> Pattern
pattern PPat x ps = Fix (PPatF x ps)

pattern PTup :: [Pattern] -> Pattern
pattern PTup xs = Fix (PTupF xs)

pattern POr :: [Pattern] -> Pattern
pattern POr ps = Fix (POrF ps)

pattern PRec :: [(T.Text, Pattern)] -> Pattern
pattern PRec ps = Fix (PRecF ps)

pattern PArr :: [Pattern] -> Pattern
pattern PArr ps = Fix (PArrF ps)

pattern PBind :: T.Text -> Maybe Pattern -> Pattern
pattern PBind b mp = Fix (PBindF b mp)

pattern PConst :: Constant -> Pattern
pattern PConst x = Fix (PConstF x)

pattern PWild :: Pattern
pattern PWild = Fix PWildF

pattern PMultiWild :: Pattern
pattern PMultiWild = Fix PMultiWildF

-- Patterns for ExprF
pattern EAbs :: [AbsBind Expr] -> Expr -> Expr -- Pattern -> QType Expr -> Expr -> Expr
pattern EAbs bs e = Fix (EAbsF bs e)

pattern ELamCase :: [CaseAlt Expr] -> Expr -- [(Pattern, QType Expr, Expr)] -> Expr
pattern ELamCase alts = Fix (ELamCaseF alts)

pattern EApp :: Expr -> [Expr] -> Expr
pattern EApp f as = Fix (EAppF f as)

pattern ETup :: [Expr] -> Expr
pattern ETup es = Fix (ETupF es)

pattern ERec :: [(T.Text, Expr)] -> Expr
pattern ERec ls = Fix (ERecF ls)

pattern ERecUp :: [(Label, Expr)] -> Expr -> Expr
pattern ERecUp ls r = Fix (ERecUpF ls r)

pattern ERecExt :: Expr -> Expr -> Expr
pattern ERecExt r1 r2 = Fix (ERecExtF r1 r2)

pattern ERecRes :: Expr -> Label -> Expr
pattern ERecRes e l = Fix (ERecResF e l)

pattern ERecSel :: Expr -> Label -> Expr
pattern ERecSel e l = Fix (ERecSelF e l)

pattern ECase :: Expr -> [CaseAlt Expr] -> Expr -- [(Pattern, Expr)] -> Expr
pattern ECase e ps = Fix (ECaseF e ps)

pattern EIf :: Expr -> Expr -> Expr -> Expr
pattern EIf b t e = Fix (EIfF b t e)

pattern EMultiIf :: [Guarded Expr] -> Maybe Expr -> Expr
pattern EMultiIf xs me = Fix (EMultiIfF xs me)

pattern ELet :: [LetBind Expr] -> Expr -> Expr -- [(Pattern, Expr)] -> Expr -> Expr
pattern ELet es e = Fix (ELetF es e)

pattern EVar :: T.Text -> Expr
pattern EVar x = Fix (EVarF x)

pattern EConst :: Constant -> Expr
pattern EConst x = Fix (EConstF x)

pattern EUnaryOp :: UnaryOp -> Expr
pattern EUnaryOp x = Fix (EUnaryOpF x)

pattern EBinOp :: BinOp -> Expr
pattern EBinOp x = Fix (EBinOpF x)

pattern EInt :: JInt -> Expr
pattern EInt x = EConst (CInt x)

pattern EChar :: Char -> Expr
pattern EChar x = EConst (CChar x)

class HasSymbol a where
  symbolOf :: a -> String
  exprConstructor :: forall b. a -> ExprF b

instance HasSymbol UnaryOp where
  symbolOf OpNot = "#"
  exprConstructor = EUnaryOpF

instance HasSymbol BinOp where
  symbolOf OpIff   = "<->"
  symbolOf OpImp   = "-->"
  symbolOf OpOr    = "||"
  symbolOf OpAnd   = "&&"
  symbolOf OpEq    = "=="
  symbolOf OpNe    = "!="
  symbolOf OpGt    = ">"
  symbolOf OpGe    = ">="
  symbolOf OpLt    = "<"
  symbolOf OpLe    = "<="
  symbolOf OpAdd   = "+"
  symbolOf OpSub   = "-"
  symbolOf OpTimes = "*"
  symbolOf OpDiv   = "/"
  symbolOf OpMod   = "%"

  exprConstructor = EBinOpF
