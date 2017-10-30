{-# Language DeriveFunctor #-}
{-# Language DeriveTraversable #-}
{-# Language LambdaCase #-}
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

data ExprF t p v e = ETAbsF v e
                   | ETAppF e t
                   | EAbsF [p] [(v, t)] e
                   | ELamCaseF [(p, e)]

                   | EAppF e [e]
                   | ETupF   [e]
                   | EArrF   [e]

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

type Expr t p s = Fix (ExprF t p s)

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
pattern ETAbs :: s -> Expr t p s -> Expr t p s
pattern ETAbs t e = Fix (ETAbsF t e)

pattern ETApp :: Expr t p s -> t -> Expr t p s
pattern ETApp e t = Fix (ETAppF e t)

pattern EAbs :: [p] -> [(s, t)] -> Expr t p s -> Expr t p s
pattern EAbs ps bs e = Fix (EAbsF ps bs e)

pattern ELamCase :: [(p, Expr t p s)] -> Expr t p s
pattern ELamCase alts = Fix (ELamCaseF alts)

pattern EApp :: Expr t p s -> [Expr t p s] -> Expr t p s
pattern EApp f as = Fix (EAppF f as)

pattern ETup :: [Expr t p s] -> Expr t p s
pattern ETup es = Fix (ETupF es)

pattern ERec :: [(T.Text, Expr t p s)] -> Expr t p s
pattern ERec ls = Fix (ERecF ls)

pattern ERecUp :: [(Label, Expr t p s)] -> Expr t p s -> Expr t p s
pattern ERecUp ls r = Fix (ERecUpF ls r)

pattern ERecExt :: Expr t p s -> Expr t p s -> Expr t p s
pattern ERecExt r1 r2 = Fix (ERecExtF r1 r2)

pattern ERecRes :: Expr t p s -> Label -> Expr t p s
pattern ERecRes e l = Fix (ERecResF e l)

pattern ERecSel :: Expr t p s -> Label -> Expr t p s
pattern ERecSel e l = Fix (ERecSelF e l)

pattern ECase :: Expr t p s -> [(p, Expr t p s)] -> Expr t p s
pattern ECase e ps = Fix (ECaseF e ps)

pattern EIf :: Expr t p s -> Expr t p s -> Expr t p s -> Expr t p s
pattern EIf b t e = Fix (EIfF b t e)

pattern EMultiIf :: [Guarded (Expr t p s)] -> Maybe (Expr t p s) -> Expr t p s
pattern EMultiIf xs me = Fix (EMultiIfF xs me)

pattern ELet :: [(p, Expr t p s)] -> Expr t p s -> Expr t p s
pattern ELet es e = Fix (ELetF es e)

pattern EVar :: T.Text -> Expr t p s
pattern EVar x = Fix (EVarF x)

pattern EConst :: Constant -> Expr t p s
pattern EConst x = Fix (EConstF x)

pattern EUnaryOp :: UnaryOp -> Expr t p s
pattern EUnaryOp x = Fix (EUnaryOpF x)

pattern EBinOp :: BinOp -> Expr t p s
pattern EBinOp x = Fix (EBinOpF x)

pattern EInt :: JInt -> Expr t p s
pattern EInt x = EConst (CInt x)

pattern EChar :: Char -> Expr t p s
pattern EChar x = EConst (CChar x)

class HasSymbol a where
  symbolOf :: a -> String
  exprConstructor :: forall t p s b. a -> ExprF t p s b

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

mapExpr :: (t -> t') -> (p -> p') -> ExprF t p s f -> ExprF t' p' s f
mapExpr f g = \case
  (ETAbsF t e)       -> ETAbsF t e
  (ETAppF e t)       -> ETAppF e (f t)
  (EAbsF ps bs e)    -> EAbsF (map g ps) (map (second f) bs) e
  (ELamCaseF alts)   -> ELamCaseF $ map (first g) alts
  (EAppF e es)       -> EAppF e es
  (ETupF es)         -> ETupF es
  (EArrF es)         -> EArrF es
  (ELetF es e)       -> ELetF (map (first g) es) e
  (ERecF fs)         -> ERecF fs
  (ERecUpF fs e)     -> ERecUpF fs e
  (ERecExtF top bot) -> ERecExtF top bot
  (ERecResF e l)     -> ERecResF e l
  (ERecSelF e l)     -> ERecSelF e l
  (ECaseF e alts)    -> ECaseF e $ map (first g) alts
  (EIfF b t e)       -> EIfF b t e
  (EMultiIfF gs me)  -> EMultiIfF gs me
  (EVarF t)          -> EVarF t
  (EConstF c)        -> EConstF c
  (EUnaryOpF o)      -> EUnaryOpF o
  (EBinOpF o)        -> EBinOpF o

mapExprT :: (t -> t') -> ExprF t p s f -> ExprF t' p s f
mapExprT = flip mapExpr id

mapExprP :: (p -> p') -> ExprF t p s f -> ExprF t p' s f
mapExprP = mapExpr id
