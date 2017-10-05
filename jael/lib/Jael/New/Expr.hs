{-# Language DeriveFunctor #-}
{-# Language DeriveTraversable #-}
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}
{-# Language TemplateHaskell #-}
{-# Language TupleSections #-}
{-# Language RankNTypes #-}

module Jael.New.Expr where

import qualified Data.Text as T

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
                deriving (Eq, Foldable, Functor, Show, Traversable)

type Pattern = Fix PatternF

$(deriveEq1   ''PatternF)
$(deriveShow1 ''PatternF)

data Guarded e = Guarded e e
  deriving (Eq, Foldable, Functor, Show, Traversable)

$(deriveEq1   ''Guarded)
$(deriveShow1 ''Guarded)

type Label     = T.Text

data ExprF t p e = ETAbsF T.Text e
                 | ETAppF e t
                 | EAbsF [p] e
                 | ELamCaseF [(p, e)]

                 | EAppF e [e]
                 | ETupF   [e]

                 | ELetF [(p, e)] e

                 | ERecF    [(Label, e)]   -- Record construction
                 | ERecUpF  [(Label, e)] e -- Record update
                 | ERecExtF e e -- Extend second record with fields of first
                 | ERecResF e Label -- Remove label from record
                 | ERecSelF e Label -- Select label from record

                 | ECaseF e [(p, e)]
                 | EIfF e e e
                 | EMultiIfF [Guarded e] (Maybe e)

                 | EVarF T.Text
                 | EConstF Constant
                 | EUnaryOpF UnaryOp
                 | EBinOpF BinOp
                 deriving (Eq, Foldable, Functor, Show, Traversable)

type Expr t p = Fix (ExprF t p)

$(deriveEq1   ''ExprF)
$(deriveShow1 ''ExprF)

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
pattern ETAbs :: T.Text -> Expr t p -> Expr t p
pattern ETAbs t e = Fix (ETAbsF t e)

pattern ETApp :: Expr t p -> t -> Expr t p
pattern ETApp e t = Fix (ETAppF e t)

pattern EAbs :: [p] -> Expr t p -> Expr t p
pattern EAbs bs e = Fix (EAbsF bs e)

pattern ELamCase :: [(p, Expr t p)] -> Expr t p
pattern ELamCase alts = Fix (ELamCaseF alts)

pattern EApp :: Expr t p -> [Expr t p] -> Expr t p
pattern EApp f as = Fix (EAppF f as)

pattern ETup :: [Expr t p] -> Expr t p
pattern ETup es = Fix (ETupF es)

pattern ERec :: [(T.Text, Expr t p)] -> Expr t p
pattern ERec ls = Fix (ERecF ls)

pattern ERecUp :: [(Label, Expr t p)] -> Expr t p -> Expr t p
pattern ERecUp ls r = Fix (ERecUpF ls r)

pattern ERecExt :: Expr t p -> Expr t p -> Expr t p
pattern ERecExt r1 r2 = Fix (ERecExtF r1 r2)

pattern ERecRes :: Expr t p -> Label -> Expr t p
pattern ERecRes e l = Fix (ERecResF e l)

pattern ERecSel :: Expr t p -> Label -> Expr t p
pattern ERecSel e l = Fix (ERecSelF e l)

pattern ECase :: Expr t p -> [(p, Expr t p)] -> Expr t p
pattern ECase e ps = Fix (ECaseF e ps)

pattern EIf :: Expr t p -> Expr t p -> Expr t p -> Expr t p
pattern EIf b t e = Fix (EIfF b t e)

pattern EMultiIf :: [Guarded (Expr t p)] -> Maybe (Expr t p) -> Expr t p
pattern EMultiIf xs me = Fix (EMultiIfF xs me)

pattern ELet :: [(p, Expr t p)] -> Expr t p -> Expr t p
pattern ELet es e = Fix (ELetF es e)

pattern EVar :: T.Text -> Expr t p
pattern EVar x = Fix (EVarF x)

pattern EConst :: Constant -> Expr t p
pattern EConst x = Fix (EConstF x)

pattern EUnaryOp :: UnaryOp -> Expr t p
pattern EUnaryOp x = Fix (EUnaryOpF x)

pattern EBinOp :: BinOp -> Expr t p
pattern EBinOp x = Fix (EBinOpF x)

pattern EInt :: JInt -> Expr t p
pattern EInt x = EConst (CInt x)

pattern EChar :: Char -> Expr t p
pattern EChar x = EConst (CChar x)

class HasSymbol a where
  symbolOf :: a -> String
  exprConstructor :: forall t p b. a -> ExprF t p b

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

exprMap :: (t -> t') -> (p -> p') -> Fix (ExprF t p) -> Fix (ExprF t' p')
exprMap f g = cata alg where
  alg (ETAbsF t e) = ETAbs t e
  alg (ETAppF e t) = ETApp e (f t)
  alg (EAbsF ps e) = EAbs (map g ps) e
  alg (ELamCaseF alts) = ELamCase $ map (first g) alts
  alg (EAppF e es) = EApp e es
  alg (ETupF es) = ETup es
  alg (ELetF es e) = ELet (map (first g) es) e
  alg (ERecF fs) = ERec fs
  alg (ERecUpF fs e) = ERecUp fs e
  alg (ERecExtF top bot) = ERecExt top bot
  alg (ERecResF e l) = ERecRes e l
  alg (ERecSelF e l) = ERecSel e l
  alg (ECaseF e alts) = ECase e $ map (first g) alts
  alg (EIfF b t e) = EIf b t e
  alg (EMultiIfF gs me) = EMultiIf gs me
  alg (EVarF t) = EVar t
  alg (EConstF c) = EConst c
  alg (EUnaryOpF o) = EUnaryOp o
  alg (EBinOpF o) = EBinOp o
