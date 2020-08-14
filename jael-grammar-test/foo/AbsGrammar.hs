-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.

module AbsGrammar where

newtype UIdent = UIdent String
  deriving (Eq, Ord, Show, Read)

newtype LIdent = LIdent String
  deriving (Eq, Ord, Show, Read)

newtype NegChan = NegChan String
  deriving (Eq, Ord, Show, Read)

newtype ReftIdent = ReftIdent String
  deriving (Eq, Ord, Show, Read)

newtype DecInt = DecInt String
  deriving (Eq, Ord, Show, Read)

newtype HexInt = HexInt String
  deriving (Eq, Ord, Show, Read)

newtype OctInt = OctInt String
  deriving (Eq, Ord, Show, Read)

newtype BinInt = BinInt String
  deriving (Eq, Ord, Show, Read)

data Label = LabelLIdent LIdent | LabelDecInt DecInt
  deriving (Eq, Ord, Show, Read)

data AnyInt
    = AnyIntDec DecInt
    | AnyIntHex HexInt
    | AnyIntOct OctInt
    | AnyIntBin BinInt
  deriving (Eq, Ord, Show, Read)

data LSpc1LIdent
    = LSpc1LIdentCon LIdent LSpc1LIdent | LSpc1LIdentNil LIdent
  deriving (Eq, Ord, Show, Read)

data LCom1LIdent
    = LCom1LIdentCon LIdent LCom1LIdent | LCom1LIdentNil LIdent
  deriving (Eq, Ord, Show, Read)

data BaseType
    = BTypeFun RefinedType RefinedType
    | BTypeFunDep LIdent RefinedType RefinedType
    | BTypeVar LIdent
    | BTypeRecEmp
    | BTypeRecPoly LIdent
    | BTypeRec LCom1RowType
    | BTypeRecExt LCom1RowType LIdent
    | BTypeTup LCom2RefType
    | BTypeArr RefinedType AnyInt
    | BTypeAlias Alias
  deriving (Eq, Ord, Show, Read)

data LCom1RowType
    = LCom1RowTypeCon LIdent RefinedType LCom1RowType
    | LCom1RowTypeNil LIdent RefinedType
  deriving (Eq, Ord, Show, Read)

data LCom1RefType
    = LCom1RefTypeCon RefinedType LCom1RefType
    | LCom1RefTypeNil RefinedType
  deriving (Eq, Ord, Show, Read)

data LCom2RefType
    = LCom2RefTypeCon RefinedType LCom2RefType
    | LCom2RefTypeNil RefinedType RefinedType
  deriving (Eq, Ord, Show, Read)

data RefinedType
    = RTypeBase0 BaseType
    | RTypeQualFun LIdent BaseType Expr
    | RTypeQualBase LIdent BaseType Expr
    | RTypeBase1 BaseType
    | RTypeBase2 BaseType
  deriving (Eq, Ord, Show, Read)

data Pattern
    = PatPar LBar2Pattern
    | PatVar LIdent
    | PatVarAt LIdent Pattern
    | PatConArg LIdent LCom1Pattern
    | PatCon LIdent
    | PatWild
    | PatInt AnyInt
    | PatNegInt DecInt
    | PatRecEmp
    | PatRec LCom1RecPat
    | PatRecPoly LCom1RecPat LIdent
    | PatRecWild LCom1RecPat
    | PatTup LCom2Pattern
    | PatArr LCom1Pattern
  deriving (Eq, Ord, Show, Read)

data LCom1Pattern
    = LCom1PatternCon Pattern LCom1Pattern | LCom1PatternNil Pattern
  deriving (Eq, Ord, Show, Read)

data LCom2Pattern
    = LCom2PatternCon Pattern LCom2Pattern
    | LCom2PatternNil Pattern Pattern
  deriving (Eq, Ord, Show, Read)

data LBar2Pattern
    = LBar2PatternCon Pattern LBar2Pattern
    | LBar2PatternNil Pattern Pattern
  deriving (Eq, Ord, Show, Read)

data LCom1RecPat
    = LCom1RecPatCon LIdent Pattern LCom1RecPat
    | LCom1RecPatNil LIdent Pattern
  deriving (Eq, Ord, Show, Read)

data Expr
    = EAbs LCom1Pattern Expr
    | EAbsCase LSemi1Case
    | ECase Expr LSemi1Case
    | ELet LSemi1Let Expr
    | EIf Expr Expr Expr
    | EMultiIf LSemi1Multi
    | EIff Expr Expr
    | EImp Expr Expr
    | ELogOr Expr Expr
    | ELogAnd Expr Expr
    | EEq Expr Expr
    | ENotEq Expr Expr
    | EGtEq Expr Expr
    | ELtEq Expr Expr
    | EGt Expr Expr
    | ELt Expr Expr
    | EPlus Expr Expr
    | EMinus Expr Expr
    | ETimes Expr Expr
    | EDiv Expr Expr
    | EMod Expr Expr
    | ELogNot Expr
    | ENegate Expr
    | ERecSel Expr Label
    | ERecRem Expr Label
    | EInt AnyInt
    | EVar LIdent
    | EReftVar ReftIdent
    | EApp Expr LCom1Expr
    | ETup LCom2Expr
    | EArr LCom1Expr
    | ERecEmpt
    | ERec LCom1RecExpr
    | ERecPoly LCom1RecExpr Expr
  deriving (Eq, Ord, Show, Read)

data LSemi1Case
    = LSemi1CaseCon Pattern Expr LSemi1Case
    | LSemi1CaseNil Pattern Expr
  deriving (Eq, Ord, Show, Read)

data LSemi1Let
    = LSemi1LetCon Pattern Expr LSemi1Let | LSemi1LetNil Pattern Expr
  deriving (Eq, Ord, Show, Read)

data LSemi1Multi
    = LSemi1MultiCon Expr Expr LSemi1Multi | LSemi1MultiNil Expr Expr
  deriving (Eq, Ord, Show, Read)

data LCom1Expr = LCom1ExprCon Expr LCom1Expr | LCom1ExprNil Expr
  deriving (Eq, Ord, Show, Read)

data LCom2Expr
    = LCom2ExprCon Expr LCom2Expr | LCom2ExprNil Expr Expr
  deriving (Eq, Ord, Show, Read)

data LCom1RecExpr
    = LCom1RecExtendCon LIdent Expr LCom1RecExpr
    | LCom1RecRenameCon LIdent LIdent LCom1RecExpr
    | LCom1RecUpdateCon LIdent Expr LCom1RecExpr
    | LCom1RecExtendNil LIdent Expr
    | LCom1RecRenameNil LIdent LIdent
    | LCom1RecUpdateNil LIdent Expr
  deriving (Eq, Ord, Show, Read)

data AnyType
    = AnyTypeRType RefinedType
    | AnyTypeSess Session
    | AnyTypeAlias Alias
  deriving (Eq, Ord, Show, Read)

data LCom1AnyType
    = LCom1AnyTypeCon AnyType LCom1AnyType | LCom1AnyTypeNil AnyType
  deriving (Eq, Ord, Show, Read)

data Alias
    = AliasNoParam UIdent
    | AliasParam UIdent LCom1AnyType MaybeLCom1Expr
  deriving (Eq, Ord, Show, Read)

data MaybeLCom1Expr
    = MaybeLCom1ExprJust LCom1Expr | MaybeLCom1ExprNothing
  deriving (Eq, Ord, Show, Read)

data Session
    = SessRec UIdent Session
    | SessGet AnyType Session
    | SessPut AnyType Session
    | SessSel LCom1SChoice
    | SessCho LCom1SChoice
    | SessDual Session
    | SessEnd
    | SessAlias Alias
  deriving (Eq, Ord, Show, Read)

data LCom1SChoice
    = LCom1SChoiceCon LIdent Session LCom1SChoice
    | LCom1SChoiceNil LIdent Session
  deriving (Eq, Ord, Show, Read)

data ChanExp = ChanExprNeg NegChan | ChanExprExp Expr
  deriving (Eq, Ord, Show, Read)

data Chan = ChanPos LIdent | ChanNeg NegChan
  deriving (Eq, Ord, Show, Read)

data Proc
    = ProcPar LBar2ParProc
    | ProcLet Pattern Expr Proc
    | ProcNew LIdent Session Proc
    | ProcGet Chan Pattern Proc
    | ProcPut Chan ChanExp Proc
    | ProcSel Chan LIdent Proc
    | ProcChoice Chan LCom1PChoice
    | ProcRec LIdent LCom1RecProc Proc
    | ProcNamed LIdent LCom1ChanExp
    | ProcFwd Chan Chan
    | ProcEnd
  deriving (Eq, Ord, Show, Read)

data LCom1ChanExp
    = LCom1ChanExpCon ChanExp LCom1ChanExp | LCom1ChanExpNil ChanExp
  deriving (Eq, Ord, Show, Read)

data LCom1PChoice
    = LCom1PChoiceCon LIdent Proc LCom1PChoice
    | LCom1PChoiceNil LIdent Proc
  deriving (Eq, Ord, Show, Read)

data LCom1RecProc
    = LCom1RecProcCon Pattern ChanExp LCom1RecProc
    | LCom1RecProcNil Pattern ChanExp
  deriving (Eq, Ord, Show, Read)

data LBar2ParProc
    = LBar2ParProcCon Proc LBar2ParProc | LBar2ParProcNil Proc Proc
  deriving (Eq, Ord, Show, Read)

data DataCon = DataConEmpt LIdent | DataConArgs LIdent LCom1RefType
  deriving (Eq, Ord, Show, Read)

data LBar1DataCon
    = LBar1DataConCon DataCon LBar1DataCon | LBar1DataConNil DataCon
  deriving (Eq, Ord, Show, Read)

data TScheme = TPoly LSpc1LIdent RefinedType | TMono RefinedType
  deriving (Eq, Ord, Show, Read)

data Top
    = TopTypeDef LIdent TScheme
    | TopType UIdent AnyType
    | TopTypeParam UIdent LCom1LIdent AnyType
    | TopData UIdent LBar1DataCon
    | TopDataParam UIdent LCom1LIdent LBar1DataCon
    | TopExpr LIdent Expr
    | TopFunc LIdent LCom1Pattern Expr
    | TopPat Pattern Expr
    | TopProc LIdent LCom1Pattern Proc
  deriving (Eq, Ord, Show, Read)

data LTop = LTopCon Top LTop | LTopNil Top
  deriving (Eq, Ord, Show, Read)
