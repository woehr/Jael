{-# Language DeriveDataTypeable #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveTraversable #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}
{-# Language RecordWildCards #-}
{-# Language TemplateHaskell #-}
{-# Language TupleSections #-}
{-# Language RankNTypes #-}

module Jael.New.Expr where

import qualified Data.Text as T

type Bind = T.Text
type Constructor = T.Text
type Label = T.Text

data IntFormat = BinInt | OctInt | HexInt | DecInt
                 deriving (Data, Eq, Show, Typeable)

data JInt = JInt
  { intFormat :: IntFormat
  , intValue  :: Integer
  , intLength :: Integer
  } deriving (Data, Eq, Show, Typeable)

data Constant
  = CInt JInt
  | OpNot
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
  deriving (Data, Eq, Typeable)

instance Show Constant where
  show (CInt j) = show j
  show OpNot   = "!"
  show OpIff   = "<->"
  show OpImp   = "-->"
  show OpOr    = "||"
  show OpAnd   = "&&"
  show OpEq    = "=="
  show OpNe    = "!="
  show OpGt    = ">"
  show OpGe    = ">="
  show OpLt    = "<"
  show OpLe    = "<="
  show OpAdd   = "+"
  show OpSub   = "-"
  show OpTimes = "*"
  show OpDiv   = "/"
  show OpMod   = "%"

data SizeSpec
  = BitSize  Integer
  | ByteSize Integer
  | KiloSize Integer
  | MegaSize Integer
  | SizedSum [SizeSpec]
  | SizeOf T.Text
  deriving (Eq, Show)

data RecTailPat b
  = TailBind b
  | TailWild
  | TailEmpt
  deriving (Data, Eq, Functor, Show, Typeable)

data PatternF b p
  = PPatF Constructor [p]    -- Constructed with sub-patterns
  | PTupF [p]           -- tuples
  | POrF [p]            -- Or pattern
  | PRecF [(Label, p)] (RecTailPat b) -- Record pattern
  | PArrF [p]           -- Array with sub-patterns
  | PBindF b (Maybe p)
  | PConstF Constant    -- Ints, Chars
  | PWildF              -- Single wildcard : _
  deriving (Data, Eq, Foldable, Functor, Show, Traversable, Typeable)

type Pattern = Fix (PatternF T.Text)

$(deriveEq1   ''PatternF)
$(deriveShow1 ''PatternF)

mapPatB :: (b -> b') -> PatternF b p -> PatternF b' p
mapPatB _ (PPatF c ps)    = PPatF c ps
mapPatB _ (PTupF ps)      = PTupF ps
mapPatB _ (POrF ps)       = POrF ps
mapPatB f (PRecF lp's mb) = PRecF lp's (f <$> mb)
mapPatB _ (PArrF ps)      = PArrF ps
mapPatB f (PBindF b mp)   = PBindF (f b) mp 
mapPatB _ (PConstF c)     = PConstF c
mapPatB _  PWildF         = PWildF

data Guarded e = Guarded e e
  deriving (Data, Eq, Foldable, Functor, Show, Traversable, Typeable)

$(deriveEq1   ''Guarded)
$(deriveShow1 ''Guarded)

data ExprF t p v e
  = ETAbsF [v] e
  | ETAppF [t] e
  | EAbsF [(p, t)] e
  | ELamCaseF t [(p, e)]

  | EAppF e [e]
  | ETupF   [e]
  | EArrF   [e]

  | ELetF [((p, t), e)] e

  | ERecF -- Empty record
  | ERecUpdateF Label e e -- Record update
  | ERecExtendF Label e e -- Extend second record with fields of first
  | ERecRenameF Label Label e -- Rename second label to first
  | ERecRemoveF e Label -- Remove label from record
  | ERecSelectF e Label -- Select label from record

  | ECaseF e [(p, e)]
  | EIfF e e e
  | EMultiIfF [Guarded e] (Maybe e)

  | EVarF T.Text
  | EConstF Constant
  deriving (Data, Eq, Foldable, Functor, Show, Traversable, Typeable)

type Expr t p v = Fix (ExprF t p v)

$(deriveEq1   ''ExprF)
$(deriveShow1 ''ExprF)

-- Patterns for Fix (PatternF T.Text)
pattern PPat :: Constructor -> [Pattern] -> Pattern
pattern PPat x ps = Fix (PPatF x ps)

pattern PTup :: [Pattern] -> Pattern
pattern PTup xs = Fix (PTupF xs)

pattern POr :: [Pattern] -> Pattern
pattern POr ps = Fix (POrF ps)

pattern PRec :: [(Label, Pattern)] -> RecTailPat T.Text -> Pattern
pattern PRec ps mb = Fix (PRecF ps mb)

pattern PArr :: [Pattern] -> Pattern
pattern PArr ps = Fix (PArrF ps)

pattern PBind :: Bind -> Maybe Pattern -> Pattern
pattern PBind b mp = Fix (PBindF b mp)

pattern PConst :: Constant -> Pattern
pattern PConst x = Fix (PConstF x)

pattern PWild :: Pattern
pattern PWild = Fix PWildF

-- Patterns for Fix (ExprF t p v)
pattern ETAbs :: [v] -> Expr t p v -> Expr t p v
pattern ETAbs vs e = Fix (ETAbsF vs e)

pattern ETApp :: [t] -> Expr t p v -> Expr t p v
pattern ETApp t e = Fix (ETAppF t e)

pattern EAbs :: [(p, t)] -> Expr t p v -> Expr t p v
pattern EAbs ps e = Fix (EAbsF ps e)

pattern ELamCase :: t -> [(p, Expr t p v)] -> Expr t p v
pattern ELamCase t alts = Fix (ELamCaseF t alts)

pattern EApp :: Expr t p v -> [Expr t p v] -> Expr t p v
pattern EApp f as = Fix (EAppF f as)

pattern ETup :: [Expr t p v] -> Expr t p v
pattern ETup es = Fix (ETupF es)

pattern EArr :: [Expr t p v] -> Expr t p v
pattern EArr es = Fix (EArrF es)

pattern ERec :: Expr t p v
pattern ERec = Fix ERecF

pattern ERecExtend :: Label -> Expr t p v -> Expr t p v -> Expr t p v
pattern ERecExtend l e r = Fix (ERecExtendF l e r)

pattern ERecUpdate :: Label -> Expr t p v -> Expr t p v -> Expr t p v
pattern ERecUpdate l e r = Fix (ERecUpdateF l e r)

pattern ERecRename :: Label -> Label -> Expr t p v -> Expr t p v
pattern ERecRename to from e = Fix (ERecRenameF to from e)

pattern ERecRemove :: Expr t p v -> Label -> Expr t p v
pattern ERecRemove e l = Fix (ERecRemoveF e l)

pattern ERecSelect :: Expr t p v -> Label -> Expr t p v
pattern ERecSelect e l = Fix (ERecSelectF e l)

pattern ECase :: Expr t p v -> [(p, Expr t p v)] -> Expr t p v
pattern ECase e ps = Fix (ECaseF e ps)

pattern EIf :: Expr t p v -> Expr t p v -> Expr t p v -> Expr t p v
pattern EIf b t e = Fix (EIfF b t e)

pattern EMultiIf :: [Guarded (Expr t p v)] -> Maybe (Expr t p v) -> Expr t p v
pattern EMultiIf xs me = Fix (EMultiIfF xs me)

pattern ELet :: [((p, t), Expr t p v)] -> Expr t p v -> Expr t p v
pattern ELet es e = Fix (ELetF es e)

pattern EVar :: T.Text -> Expr t p v
pattern EVar x = Fix (EVarF x)

pattern EConst :: Constant -> Expr t p v
pattern EConst x = Fix (EConstF x)

pattern EInt :: JInt -> Expr t p v
pattern EInt x = EConst (CInt x)

pattern EFalse :: Expr t p v
pattern EFalse = EVar "false"

pattern ETrue :: Expr t p v
pattern ETrue = EVar "true"

mapExpr :: (t -> t') -> (p -> p') -> ExprF t p v f -> ExprF t' p' v f
mapExpr f g = \case
  (ETAbsF vs e)        -> ETAbsF vs e
  (ETAppF ts e)        -> ETAppF (map f ts) e
  (EAbsF pt's e)       -> EAbsF (map (bimap g f) pt's) e
  (ELamCaseF t alts)   -> ELamCaseF (f t) $ map (first g) alts
  (EAppF e es)         -> EAppF e es
  (ETupF es)           -> ETupF es
  (EArrF es)           -> EArrF es
  (ELetF pte's e)      -> ELetF (map (first $ bimap g f) pte's) e
  ERecF                -> ERecF
  (ERecExtendF l e r)  -> ERecExtendF l e r
  (ERecUpdateF l e r)  -> ERecUpdateF l e r
  (ERecRenameF l l' r) -> ERecRenameF l l' r
  (ERecRemoveF e l)    -> ERecRemoveF e l
  (ERecSelectF e l)    -> ERecSelectF e l
  (ECaseF e alts)      -> ECaseF e $ map (first g) alts
  (EIfF b t e)         -> EIfF b t e
  (EMultiIfF gs me)    -> EMultiIfF gs me
  (EVarF t)            -> EVarF t
  (EConstF c)          -> EConstF c

mapExprT :: (t -> t') -> ExprF t p v f -> ExprF t' p v f
mapExprT = flip mapExpr id

mapExprP :: (p -> p') -> ExprF t p v f -> ExprF t p' v f
mapExprP = mapExpr id
