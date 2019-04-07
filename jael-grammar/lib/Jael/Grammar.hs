{-# Language TypeFamilies #-}

module Jael.Grammar
  ( module Jael.Grammar
  , module Jael.Grammar.Abs
  , module Jael.Grammar.ErrM
  , module Jael.Grammar.Lex
  , module Jael.Grammar.Par
  ) where

import Jael.Grammar.Abs
import Jael.Grammar.ErrM
import Jael.Grammar.Lex
import Jael.Grammar.Par

data RecordOp = RecExtend LIdent Expr
              | RecRename LIdent LIdent
              | RecUpdate LIdent Expr
              deriving (Eq, Show)

type family Element g

type instance Element [a] = a
type instance Element LSpc1LIdent = LIdent
type instance Element LCom1LIdent = LIdent
type instance Element LCom1RowType = (LIdent, RefinedType)
type instance Element LCom1RefType = RefinedType
type instance Element LCom2RefType = RefinedType
type instance Element LCom1Pattern = Pattern
type instance Element LCom2Pattern = Pattern
type instance Element LBar2Pattern = Pattern
type instance Element LCom1RecPat = (LIdent, Pattern)
type instance Element LSemi1Case = (Pattern, Expr)
type instance Element LSemi1Let = (Pattern, Expr)
type instance Element LSemi1Multi = (Expr, Expr)
--type instance LSemi1Guard = (Pattern, Expr, Expr)
type instance Element LCom1Expr = Expr
type instance Element LCom2Expr = Expr
type instance Element LCom1RecExpr = RecordOp
type instance Element LCom1AnyType = AnyType
type instance Element LCom1SChoice = (LIdent, Session)
type instance Element LCom1ChanExp = ChanExp
type instance Element LCom1PChoice = (LIdent, Proc)
type instance Element LCom1RecProc = (Pattern, ChanExp)
type instance Element LBar2ParProc = Proc
type instance Element LBar1DataCon = DataCon
type instance Element LTop = Top

class ToList mono where
  otoList :: mono -> [Element mono]

instance ToList [a] where
  otoList = id

instance ToList LSpc1LIdent where
  otoList (LSpc1LIdentNil l)    = [l]
  otoList (LSpc1LIdentCon l xs) = l: otoList xs

instance ToList LCom1LIdent where
  otoList (LCom1LIdentNil l)    = [l]
  otoList (LCom1LIdentCon l xs) = l: otoList xs

instance ToList LCom1RowType where
  otoList (LCom1RowTypeNil l b)    = [(l, b)]
  otoList (LCom1RowTypeCon l b xs) = (l, b): otoList xs

instance ToList LCom1RefType where
  otoList (LCom1RefTypeNil r)    = [r]
  otoList (LCom1RefTypeCon r xs) = r: otoList xs

instance ToList LCom2RefType where
  otoList (LCom2RefTypeNil r1 r2) = [r1, r2]
  otoList (LCom2RefTypeCon r xs)  = r: otoList xs

instance ToList LCom1Pattern where
  otoList (LCom1PatternNil p)    = [p]
  otoList (LCom1PatternCon p xs) = p: otoList xs

instance ToList LCom2Pattern where
  otoList (LCom2PatternNil p1 p2) = [p1, p2]
  otoList (LCom2PatternCon p xs)  = p: otoList xs

instance ToList LBar2Pattern where
  otoList (LBar2PatternNil p1 p2) = [p1, p2]
  otoList (LBar2PatternCon p xs)  = p: otoList xs

instance ToList LCom1RecPat where
  otoList (LCom1RecPatNil l p)    = [(l, p)]
  otoList (LCom1RecPatCon l p xs) = (l, p): otoList xs

instance ToList LSemi1Case where
  otoList (LSemi1CaseNil p e)    = [(p, e)]
  otoList (LSemi1CaseCon p e xs) = (p, e): otoList xs

instance ToList LSemi1Let where
  otoList (LSemi1LetNil p e)    = [(p, e)]
  otoList (LSemi1LetCon p e xs) = (p, e): otoList xs

instance ToList LSemi1Multi where
  otoList (LSemi1MultiNil e1 e2)    = [(e1, e2)]
  otoList (LSemi1MultiCon e1 e2 xs) = (e1, e2): otoList xs

instance ToList LCom1Expr where
  otoList (LCom1ExprNil e)    = [e]
  otoList (LCom1ExprCon e xs) = e: otoList xs

instance ToList LCom2Expr where
  otoList (LCom2ExprNil e1 e2) = [e1, e2]
  otoList (LCom2ExprCon e xs)  = e: otoList xs

instance ToList LCom1RecExpr where
  otoList (LCom1RecExtendNil l e)      = [RecExtend l e]
  otoList (LCom1RecExtendCon l e xs)   = RecExtend l e: otoList xs
  otoList (LCom1RecRenameNil l1 l2)    = [RecRename l1 l2]
  otoList (LCom1RecRenameCon l1 l2 xs) = RecRename l1 l2: otoList xs
  otoList (LCom1RecUpdateNil l e)      = [RecUpdate l e]
  otoList (LCom1RecUpdateCon l e xs)   = RecUpdate l e: otoList xs

instance ToList LCom1AnyType where
  otoList (LCom1AnyTypeNil t)    = [t]
  otoList (LCom1AnyTypeCon t xs) = t: otoList xs

instance ToList LCom1SChoice where
  otoList (LCom1SChoiceNil l s)    = [(l, s)]
  otoList (LCom1SChoiceCon l s xs) = (l, s): otoList xs

instance ToList LCom1ChanExp where
  otoList (LCom1ChanExpNil ce)    = [ce]
  otoList (LCom1ChanExpCon ce xs) = ce: otoList xs

instance ToList LCom1PChoice where
  otoList (LCom1PChoiceNil l p)    = [(l, p)]
  otoList (LCom1PChoiceCon l p xs) = (l, p): otoList xs

instance ToList LCom1RecProc where
  otoList (LCom1RecProcNil p ce)    = [(p, ce)]
  otoList (LCom1RecProcCon p ce xs) = (p, ce): otoList xs

instance ToList LBar2ParProc where
  otoList (LBar2ParProcNil p1 p2) = [p1, p2]
  otoList (LBar2ParProcCon p xs)  = p: otoList xs

instance ToList LBar1DataCon where
  otoList (LBar1DataConNil dc)    = [dc]
  otoList (LBar1DataConCon dc xs) = dc: otoList xs

instance ToList LTop where
  otoList (LTopCon x xs) = x: otoList xs
  otoList (LTopNil x)    = [x]

parseTo :: ([Token] -> Err a) -> (a -> b) -> String -> Either String b
parseTo p f x = case p (tokens x) of
  Ok  a -> Right (f a)
  Bad e -> Left e
