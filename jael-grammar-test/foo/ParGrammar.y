-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParGrammar where
import AbsGrammar
import LexGrammar
import ErrM

}

%name pLabel Label
%name pAnyInt AnyInt
%name pLSpc1LIdent LSpc1LIdent
%name pLCom1LIdent LCom1LIdent
%name pBaseType0 BaseType0
%name pBaseType1 BaseType1
%name pBaseType2 BaseType2
%name pBaseType10 BaseType10
%name pBaseType11 BaseType11
%name pBaseType BaseType
%name pBaseType99 BaseType99
%name pLCom1RowType LCom1RowType
%name pLCom1RefType LCom1RefType
%name pLCom2RefType LCom2RefType
%name pRefinedType0 RefinedType0
%name pRefinedType1 RefinedType1
%name pRefinedType2 RefinedType2
%name pRefinedType10 RefinedType10
%name pRefinedType11 RefinedType11
%name pRefinedType RefinedType
%name pRefinedType99 RefinedType99
%name pPattern1 Pattern1
%name pPattern2 Pattern2
%name pPattern3 Pattern3
%name pPattern4 Pattern4
%name pPattern Pattern
%name pLCom1Pattern LCom1Pattern
%name pLCom2Pattern LCom2Pattern
%name pLBar2Pattern LBar2Pattern
%name pLCom1RecPat LCom1RecPat
%name pExpr101 Expr101
%name pExpr102 Expr102
%name pExpr1 Expr1
%name pLSemi1Case LSemi1Case
%name pExpr201 Expr201
%name pExpr202 Expr202
%name pExpr2 Expr2
%name pLSemi1Let LSemi1Let
%name pExpr301 Expr301
%name pExpr302 Expr302
%name pExpr3 Expr3
%name pLSemi1Multi LSemi1Multi
%name pExpr4 Expr4
%name pExpr5 Expr5
%name pExpr6 Expr6
%name pExpr7 Expr7
%name pExpr8 Expr8
%name pExpr9 Expr9
%name pExpr10 Expr10
%name pExpr11 Expr11
%name pExpr1201 Expr1201
%name pExpr1202 Expr1202
%name pExpr1203 Expr1203
%name pExpr12 Expr12
%name pLCom1Expr LCom1Expr
%name pLCom2Expr LCom2Expr
%name pLCom1RecExpr LCom1RecExpr
%name pExpr Expr
%name pAnyType AnyType
%name pLCom1AnyType LCom1AnyType
%name pAlias Alias
%name pMaybeLCom1Expr MaybeLCom1Expr
%name pSession0 Session0
%name pSession1 Session1
%name pSession2 Session2
%name pSession12 Session12
%name pSession99 Session99
%name pSession Session
%name pLCom1SChoice LCom1SChoice
%name pChanExp ChanExp
%name pChan Chan
%name pProc1 Proc1
%name pProc2 Proc2
%name pProc Proc
%name pLCom1ChanExp LCom1ChanExp
%name pLCom1PChoice LCom1PChoice
%name pLCom1RecProc LCom1RecProc
%name pLBar2ParProc LBar2ParProc
%name pDataCon DataCon
%name pLBar1DataCon LBar1DataCon
%name pTScheme TScheme
%name pTop Top
%name pLTop LTop
-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%token
  '!' { PT _ (TS _ 1) }
  '!=' { PT _ (TS _ 2) }
  '$' { PT _ (TS _ 3) }
  '%' { PT _ (TS _ 4) }
  '&' { PT _ (TS _ 5) }
  '&&' { PT _ (TS _ 6) }
  '(' { PT _ (TS _ 7) }
  '(|' { PT _ (TS _ 8) }
  ')' { PT _ (TS _ 9) }
  '*' { PT _ (TS _ 10) }
  '+' { PT _ (TS _ 11) }
  ',' { PT _ (TS _ 12) }
  '-' { PT _ (TS _ 13) }
  '-->' { PT _ (TS _ 14) }
  '->' { PT _ (TS _ 15) }
  '.' { PT _ (TS _ 16) }
  '/' { PT _ (TS _ 17) }
  ':' { PT _ (TS _ 18) }
  ':=' { PT _ (TS _ 19) }
  ';' { PT _ (TS _ 20) }
  '<' { PT _ (TS _ 21) }
  '<-' { PT _ (TS _ 22) }
  '<->' { PT _ (TS _ 23) }
  '<=' { PT _ (TS _ 24) }
  '=' { PT _ (TS _ 25) }
  '==' { PT _ (TS _ 26) }
  '>' { PT _ (TS _ 27) }
  '>=' { PT _ (TS _ 28) }
  '?' { PT _ (TS _ 29) }
  '@' { PT _ (TS _ 30) }
  '[' { PT _ (TS _ 31) }
  '\\' { PT _ (TS _ 32) }
  '\\(' { PT _ (TS _ 33) }
  '\\case' { PT _ (TS _ 34) }
  ']' { PT _ (TS _ 35) }
  '_' { PT _ (TS _ 36) }
  'case' { PT _ (TS _ 37) }
  'cases' { PT _ (TS _ 38) }
  'coind' { PT _ (TS _ 39) }
  'corec' { PT _ (TS _ 40) }
  'data' { PT _ (TS _ 41) }
  'dual' { PT _ (TS _ 42) }
  'else' { PT _ (TS _ 43) }
  'forall' { PT _ (TS _ 44) }
  'if' { PT _ (TS _ 45) }
  'in' { PT _ (TS _ 46) }
  'let' { PT _ (TS _ 47) }
  'new' { PT _ (TS _ 48) }
  'of' { PT _ (TS _ 49) }
  'proc' { PT _ (TS _ 50) }
  'select' { PT _ (TS _ 51) }
  'then' { PT _ (TS _ 52) }
  'type' { PT _ (TS _ 53) }
  '{' { PT _ (TS _ 54) }
  '{}' { PT _ (TS _ 55) }
  '|' { PT _ (TS _ 56) }
  '|)' { PT _ (TS _ 57) }
  '||' { PT _ (TS _ 58) }
  '}' { PT _ (TS _ 59) }
  '~' { PT _ (TS _ 60) }
  L_UIdent { PT _ (T_UIdent $$) }
  L_LIdent { PT _ (T_LIdent $$) }
  L_NegChan { PT _ (T_NegChan $$) }
  L_ReftIdent { PT _ (T_ReftIdent $$) }
  L_DecInt { PT _ (T_DecInt $$) }
  L_HexInt { PT _ (T_HexInt $$) }
  L_OctInt { PT _ (T_OctInt $$) }
  L_BinInt { PT _ (T_BinInt $$) }

%%

UIdent :: { UIdent}
UIdent  : L_UIdent { UIdent ($1)}

LIdent :: { LIdent}
LIdent  : L_LIdent { LIdent ($1)}

NegChan :: { NegChan}
NegChan  : L_NegChan { NegChan ($1)}

ReftIdent :: { ReftIdent}
ReftIdent  : L_ReftIdent { ReftIdent ($1)}

DecInt :: { DecInt}
DecInt  : L_DecInt { DecInt ($1)}

HexInt :: { HexInt}
HexInt  : L_HexInt { HexInt ($1)}

OctInt :: { OctInt}
OctInt  : L_OctInt { OctInt ($1)}

BinInt :: { BinInt}
BinInt  : L_BinInt { BinInt ($1)}

Label :: { Label }
Label : LIdent { AbsGrammar.LabelLIdent $1 }
      | DecInt { AbsGrammar.LabelDecInt $1 }
AnyInt :: { AnyInt }
AnyInt : DecInt { AbsGrammar.AnyIntDec $1 }
       | HexInt { AbsGrammar.AnyIntHex $1 }
       | OctInt { AbsGrammar.AnyIntOct $1 }
       | BinInt { AbsGrammar.AnyIntBin $1 }
LSpc1LIdent :: { LSpc1LIdent }
LSpc1LIdent : LIdent LSpc1LIdent { AbsGrammar.LSpc1LIdentCon $1 $2 }
            | LIdent { AbsGrammar.LSpc1LIdentNil $1 }
LCom1LIdent :: { LCom1LIdent }
LCom1LIdent : LIdent ',' LCom1LIdent { AbsGrammar.LCom1LIdentCon $1 $3 }
            | LIdent { AbsGrammar.LCom1LIdentNil $1 }
BaseType0 :: { BaseType }
BaseType0 : RefinedType11 '->' RefinedType { AbsGrammar.BTypeFun $1 $3 }
          | LIdent ':' RefinedType11 '->' RefinedType { AbsGrammar.BTypeFunDep $1 $3 $5 }
BaseType1 :: { BaseType }
BaseType1 : LIdent { AbsGrammar.BTypeVar $1 }
          | '{}' { AbsGrammar.BTypeRecEmp }
          | '{' LIdent '}' { AbsGrammar.BTypeRecPoly $2 }
          | '{' LCom1RowType '}' { AbsGrammar.BTypeRec $2 }
          | '{' LCom1RowType '|' LIdent '}' { AbsGrammar.BTypeRecExt $2 $4 }
          | '(' LCom2RefType ')' { AbsGrammar.BTypeTup $2 }
          | '[' RefinedType ';' AnyInt ']' { AbsGrammar.BTypeArr $2 $4 }
BaseType2 :: { BaseType }
BaseType2 : Alias { AbsGrammar.BTypeAlias $1 }
BaseType10 :: { BaseType }
BaseType10 : BaseType0 { $1 }
BaseType11 :: { BaseType }
BaseType11 : BaseType1 { $1 } | BaseType2 { $1 }
BaseType :: { BaseType }
BaseType : BaseType10 { $1 } | BaseType11 { $1 }
BaseType99 :: { BaseType }
BaseType99 : BaseType0 { $1 } | BaseType1 { $1 }
LCom1RowType :: { LCom1RowType }
LCom1RowType : LIdent ':' RefinedType ',' LCom1RowType { AbsGrammar.LCom1RowTypeCon $1 $3 $5 }
             | LIdent ':' RefinedType { AbsGrammar.LCom1RowTypeNil $1 $3 }
LCom1RefType :: { LCom1RefType }
LCom1RefType : RefinedType ',' LCom1RefType { AbsGrammar.LCom1RefTypeCon $1 $3 }
             | RefinedType { AbsGrammar.LCom1RefTypeNil $1 }
LCom2RefType :: { LCom2RefType }
LCom2RefType : RefinedType ',' LCom2RefType { AbsGrammar.LCom2RefTypeCon $1 $3 }
             | RefinedType ',' RefinedType { AbsGrammar.LCom2RefTypeNil $1 $3 }
RefinedType0 :: { RefinedType }
RefinedType0 : BaseType0 { AbsGrammar.RTypeBase0 $1 }
RefinedType1 :: { RefinedType }
RefinedType1 : '(|' LIdent ':' '(' BaseType10 ')' '|' Expr '|)' { AbsGrammar.RTypeQualFun $2 $5 $8 }
             | '(|' LIdent ':' BaseType11 '|' Expr '|)' { AbsGrammar.RTypeQualBase $2 $4 $6 }
             | BaseType1 { AbsGrammar.RTypeBase1 $1 }
RefinedType2 :: { RefinedType }
RefinedType2 : BaseType2 { AbsGrammar.RTypeBase2 $1 }
RefinedType10 :: { RefinedType }
RefinedType10 : RefinedType0 { $1 }
RefinedType11 :: { RefinedType }
RefinedType11 : RefinedType1 { $1 }
              | RefinedType2 { $1 }
              | '(' RefinedType0 ')' { $2 }
RefinedType :: { RefinedType }
RefinedType : RefinedType10 { $1 } | RefinedType11 { $1 }
RefinedType99 :: { RefinedType }
RefinedType99 : RefinedType0 { $1 } | RefinedType1 { $1 }
Pattern1 :: { Pattern }
Pattern1 : LBar2Pattern { AbsGrammar.PatPar $1 } | Pattern2 { $1 }
Pattern2 :: { Pattern }
Pattern2 : '$' LIdent { AbsGrammar.PatVar $2 }
         | '$' LIdent '@' Pattern4 { AbsGrammar.PatVarAt $2 $4 }
         | Pattern3 { $1 }
Pattern3 :: { Pattern }
Pattern3 : LIdent '(' LCom1Pattern ')' { AbsGrammar.PatConArg $1 $3 }
         | LIdent { AbsGrammar.PatCon $1 }
         | Pattern4 { $1 }
Pattern4 :: { Pattern }
Pattern4 : '_' { AbsGrammar.PatWild }
         | AnyInt { AbsGrammar.PatInt $1 }
         | '~' DecInt { AbsGrammar.PatNegInt $2 }
         | '{}' { AbsGrammar.PatRecEmp }
         | '{' LCom1RecPat '}' { AbsGrammar.PatRec $2 }
         | '{' LCom1RecPat '|' '$' LIdent '}' { AbsGrammar.PatRecPoly $2 $5 }
         | '{' LCom1RecPat '|' '_' '}' { AbsGrammar.PatRecWild $2 }
         | '(' LCom2Pattern ')' { AbsGrammar.PatTup $2 }
         | '[' LCom1Pattern ']' { AbsGrammar.PatArr $2 }
         | '(' Pattern ')' { $2 }
Pattern :: { Pattern }
Pattern : Pattern1 { $1 }
LCom1Pattern :: { LCom1Pattern }
LCom1Pattern : Pattern ',' LCom1Pattern { AbsGrammar.LCom1PatternCon $1 $3 }
             | Pattern { AbsGrammar.LCom1PatternNil $1 }
LCom2Pattern :: { LCom2Pattern }
LCom2Pattern : Pattern ',' LCom2Pattern { AbsGrammar.LCom2PatternCon $1 $3 }
             | Pattern ',' Pattern { AbsGrammar.LCom2PatternNil $1 $3 }
LBar2Pattern :: { LBar2Pattern }
LBar2Pattern : Pattern2 '||' LBar2Pattern { AbsGrammar.LBar2PatternCon $1 $3 }
             | Pattern2 '||' Pattern2 { AbsGrammar.LBar2PatternNil $1 $3 }
LCom1RecPat :: { LCom1RecPat }
LCom1RecPat : LIdent '=' Pattern ',' LCom1RecPat { AbsGrammar.LCom1RecPatCon $1 $3 $5 }
            | LIdent '=' Pattern { AbsGrammar.LCom1RecPatNil $1 $3 }
Expr101 :: { Expr }
Expr101 : '\\(' LCom1Pattern ')' '->' Expr { AbsGrammar.EAbs $2 $5 }
Expr102 :: { Expr }
Expr102 : '\\case' '{' LSemi1Case '}' { AbsGrammar.EAbsCase $3 }
Expr1 :: { Expr }
Expr1 : Expr101 { $1 } | Expr102 { $1 } | Expr2 { $1 }
LSemi1Case :: { LSemi1Case }
LSemi1Case : Pattern '->' Expr ';' LSemi1Case { AbsGrammar.LSemi1CaseCon $1 $3 $5 }
           | Pattern '->' Expr { AbsGrammar.LSemi1CaseNil $1 $3 }
Expr201 :: { Expr }
Expr201 : 'case' Expr 'of' '{' LSemi1Case '}' { AbsGrammar.ECase $2 $5 }
Expr202 :: { Expr }
Expr202 : 'let' '{' LSemi1Let '}' 'in' Expr2 { AbsGrammar.ELet $3 $6 }
Expr2 :: { Expr }
Expr2 : Expr201 { $1 } | Expr202 { $1 } | Expr3 { $1 }
LSemi1Let :: { LSemi1Let }
LSemi1Let : Pattern '=' Expr ';' LSemi1Let { AbsGrammar.LSemi1LetCon $1 $3 $5 }
          | Pattern '=' Expr { AbsGrammar.LSemi1LetNil $1 $3 }
Expr301 :: { Expr }
Expr301 : 'if' Expr 'then' Expr3 'else' Expr3 { AbsGrammar.EIf $2 $4 $6 }
Expr302 :: { Expr }
Expr302 : 'if' '{' LSemi1Multi '}' { AbsGrammar.EMultiIf $3 }
Expr3 :: { Expr }
Expr3 : Expr301 { $1 } | Expr302 { $1 } | Expr4 { $1 }
LSemi1Multi :: { LSemi1Multi }
LSemi1Multi : Expr2 '->' Expr ';' LSemi1Multi { AbsGrammar.LSemi1MultiCon $1 $3 $5 }
            | Expr2 '->' Expr { AbsGrammar.LSemi1MultiNil $1 $3 }
Expr4 :: { Expr }
Expr4 : Expr5 '<->' Expr4 { AbsGrammar.EIff $1 $3 } | Expr5 { $1 }
Expr5 :: { Expr }
Expr5 : Expr6 '-->' Expr6 { AbsGrammar.EImp $1 $3 } | Expr6 { $1 }
Expr6 :: { Expr }
Expr6 : Expr7 '||' Expr6 { AbsGrammar.ELogOr $1 $3 } | Expr7 { $1 }
Expr7 :: { Expr }
Expr7 : Expr8 '&&' Expr7 { AbsGrammar.ELogAnd $1 $3 }
      | Expr8 { $1 }
Expr8 :: { Expr }
Expr8 : Expr9 '==' Expr9 { AbsGrammar.EEq $1 $3 }
      | Expr9 '!=' Expr9 { AbsGrammar.ENotEq $1 $3 }
      | Expr9 '>=' Expr9 { AbsGrammar.EGtEq $1 $3 }
      | Expr9 '<=' Expr9 { AbsGrammar.ELtEq $1 $3 }
      | Expr9 '>' Expr9 { AbsGrammar.EGt $1 $3 }
      | Expr9 '<' Expr9 { AbsGrammar.ELt $1 $3 }
      | Expr9 { $1 }
Expr9 :: { Expr }
Expr9 : Expr10 '+' Expr9 { AbsGrammar.EPlus $1 $3 }
      | Expr10 '-' Expr9 { AbsGrammar.EMinus $1 $3 }
      | Expr10 { $1 }
Expr10 :: { Expr }
Expr10 : Expr11 '*' Expr10 { AbsGrammar.ETimes $1 $3 }
       | Expr11 '/' Expr10 { AbsGrammar.EDiv $1 $3 }
       | Expr11 '%' Expr10 { AbsGrammar.EMod $1 $3 }
       | Expr11 { $1 }
Expr11 :: { Expr }
Expr11 : '!' Expr11 { AbsGrammar.ELogNot $2 }
       | '~' Expr11 { AbsGrammar.ENegate $2 }
       | Expr12 { $1 }
Expr1201 :: { Expr }
Expr1201 : Expr12 '.' Label { AbsGrammar.ERecSel $1 $3 }
         | Expr12 '\\' Label { AbsGrammar.ERecRem $1 $3 }
Expr1202 :: { Expr }
Expr1202 : AnyInt { AbsGrammar.EInt $1 }
         | LIdent { AbsGrammar.EVar $1 }
         | ReftIdent { AbsGrammar.EReftVar $1 }
         | Expr12 '(' LCom1Expr ')' { AbsGrammar.EApp $1 $3 }
         | '(' LCom2Expr ')' { AbsGrammar.ETup $2 }
         | '[' LCom1Expr ']' { AbsGrammar.EArr $2 }
Expr1203 :: { Expr }
Expr1203 : '{}' { AbsGrammar.ERecEmpt }
         | '{' LCom1RecExpr '}' { AbsGrammar.ERec $2 }
         | '{' LCom1RecExpr '|' Expr '}' { AbsGrammar.ERecPoly $2 $4 }
Expr12 :: { Expr }
Expr12 : Expr1201 { $1 }
       | Expr1202 { $1 }
       | Expr1203 { $1 }
       | '(' Expr ')' { $2 }
LCom1Expr :: { LCom1Expr }
LCom1Expr : Expr ',' LCom1Expr { AbsGrammar.LCom1ExprCon $1 $3 }
          | Expr { AbsGrammar.LCom1ExprNil $1 }
LCom2Expr :: { LCom2Expr }
LCom2Expr : Expr ',' LCom2Expr { AbsGrammar.LCom2ExprCon $1 $3 }
          | Expr ',' Expr { AbsGrammar.LCom2ExprNil $1 $3 }
LCom1RecExpr :: { LCom1RecExpr }
LCom1RecExpr : LIdent '=' Expr ',' LCom1RecExpr { AbsGrammar.LCom1RecExtendCon $1 $3 $5 }
             | LIdent '<-' LIdent ',' LCom1RecExpr { AbsGrammar.LCom1RecRenameCon $1 $3 $5 }
             | LIdent ':=' Expr ',' LCom1RecExpr { AbsGrammar.LCom1RecUpdateCon $1 $3 $5 }
             | LIdent '=' Expr { AbsGrammar.LCom1RecExtendNil $1 $3 }
             | LIdent '<-' LIdent { AbsGrammar.LCom1RecRenameNil $1 $3 }
             | LIdent ':=' Expr { AbsGrammar.LCom1RecUpdateNil $1 $3 }
Expr :: { Expr }
Expr : Expr1 { $1 }
AnyType :: { AnyType }
AnyType : RefinedType99 { AbsGrammar.AnyTypeRType $1 }
        | Session99 { AbsGrammar.AnyTypeSess $1 }
        | Alias { AbsGrammar.AnyTypeAlias $1 }
LCom1AnyType :: { LCom1AnyType }
LCom1AnyType : AnyType ',' LCom1AnyType { AbsGrammar.LCom1AnyTypeCon $1 $3 }
             | AnyType { AbsGrammar.LCom1AnyTypeNil $1 }
Alias :: { Alias }
Alias : UIdent { AbsGrammar.AliasNoParam $1 }
      | UIdent '(' LCom1AnyType MaybeLCom1Expr ')' { AbsGrammar.AliasParam $1 $3 $4 }
MaybeLCom1Expr :: { MaybeLCom1Expr }
MaybeLCom1Expr : ';' LCom1Expr { AbsGrammar.MaybeLCom1ExprJust $2 }
               | {- empty -} { AbsGrammar.MaybeLCom1ExprNothing }
Session0 :: { Session }
Session0 : 'coind' UIdent '.' Session12 { AbsGrammar.SessRec $2 $4 }
Session1 :: { Session }
Session1 : '?' '[' AnyType ']' Session { AbsGrammar.SessGet $3 $5 }
         | '!' '[' AnyType ']' Session { AbsGrammar.SessPut $3 $5 }
         | '+' '[' LCom1SChoice ']' { AbsGrammar.SessSel $3 }
         | '&' '[' LCom1SChoice ']' { AbsGrammar.SessCho $3 }
         | 'dual' Session2 { AbsGrammar.SessDual $2 }
         | {- empty -} { AbsGrammar.SessEnd }
Session2 :: { Session }
Session2 : Alias { AbsGrammar.SessAlias $1 }
Session12 :: { Session }
Session12 : Session1 { $1 } | Session2 { $1 }
Session99 :: { Session }
Session99 : Session0 { $1 } | Session1 { $1 }
Session :: { Session }
Session : Session0 { $1 } | Session1 { $1 } | Session2 { $1 }
LCom1SChoice :: { LCom1SChoice }
LCom1SChoice : LIdent '->' Session ',' LCom1SChoice { AbsGrammar.LCom1SChoiceCon $1 $3 $5 }
             | LIdent '->' Session { AbsGrammar.LCom1SChoiceNil $1 $3 }
ChanExp :: { ChanExp }
ChanExp : NegChan { AbsGrammar.ChanExprNeg $1 }
        | Expr { AbsGrammar.ChanExprExp $1 }
Chan :: { Chan }
Chan : LIdent { AbsGrammar.ChanPos $1 }
     | NegChan { AbsGrammar.ChanNeg $1 }
Proc1 :: { Proc }
Proc1 : '{' LBar2ParProc '}' { AbsGrammar.ProcPar $2 }
Proc2 :: { Proc }
Proc2 : 'let' Pattern '=' Expr Proc { AbsGrammar.ProcLet $2 $4 $5 }
      | 'new' LIdent ':' Session Proc { AbsGrammar.ProcNew $2 $4 $5 }
      | Chan '->' Pattern Proc { AbsGrammar.ProcGet $1 $3 $4 }
      | Chan '<-' ChanExp Proc { AbsGrammar.ProcPut $1 $3 $4 }
      | Chan 'select' LIdent Proc { AbsGrammar.ProcSel $1 $3 $4 }
      | Chan 'cases' '{' LCom1PChoice '}' { AbsGrammar.ProcChoice $1 $4 }
      | 'corec' LIdent '(' LCom1RecProc ')' Proc { AbsGrammar.ProcRec $2 $4 $6 }
      | LIdent '(' LCom1ChanExp ')' { AbsGrammar.ProcNamed $1 $3 }
      | Chan '<->' Chan { AbsGrammar.ProcFwd $1 $3 }
      | {- empty -} { AbsGrammar.ProcEnd }
Proc :: { Proc }
Proc : Proc1 { $1 } | Proc2 { $1 }
LCom1ChanExp :: { LCom1ChanExp }
LCom1ChanExp : ChanExp ',' LCom1ChanExp { AbsGrammar.LCom1ChanExpCon $1 $3 }
             | ChanExp { AbsGrammar.LCom1ChanExpNil $1 }
LCom1PChoice :: { LCom1PChoice }
LCom1PChoice : LIdent '->' Proc ',' LCom1PChoice { AbsGrammar.LCom1PChoiceCon $1 $3 $5 }
             | LIdent '->' Proc { AbsGrammar.LCom1PChoiceNil $1 $3 }
LCom1RecProc :: { LCom1RecProc }
LCom1RecProc : Pattern '=' ChanExp ',' LCom1RecProc { AbsGrammar.LCom1RecProcCon $1 $3 $5 }
             | Pattern '=' ChanExp { AbsGrammar.LCom1RecProcNil $1 $3 }
LBar2ParProc :: { LBar2ParProc }
LBar2ParProc : Proc2 '|' LBar2ParProc { AbsGrammar.LBar2ParProcCon $1 $3 }
             | Proc2 '|' Proc2 { AbsGrammar.LBar2ParProcNil $1 $3 }
DataCon :: { DataCon }
DataCon : LIdent { AbsGrammar.DataConEmpt $1 }
        | LIdent '(' LCom1RefType ')' { AbsGrammar.DataConArgs $1 $3 }
LBar1DataCon :: { LBar1DataCon }
LBar1DataCon : DataCon '|' LBar1DataCon { AbsGrammar.LBar1DataConCon $1 $3 }
             | DataCon { AbsGrammar.LBar1DataConNil $1 }
TScheme :: { TScheme }
TScheme : 'forall' LSpc1LIdent '.' RefinedType { AbsGrammar.TPoly $2 $4 }
        | RefinedType { AbsGrammar.TMono $1 }
Top :: { Top }
Top : LIdent ':' TScheme { AbsGrammar.TopTypeDef $1 $3 }
    | 'type' UIdent '=' AnyType { AbsGrammar.TopType $2 $4 }
    | 'type' UIdent '(' LCom1LIdent ')' '=' AnyType { AbsGrammar.TopTypeParam $2 $4 $7 }
    | 'data' UIdent '=' LBar1DataCon { AbsGrammar.TopData $2 $4 }
    | 'data' UIdent '(' LCom1LIdent ')' '=' LBar1DataCon { AbsGrammar.TopDataParam $2 $4 $7 }
    | LIdent '=' Expr { AbsGrammar.TopExpr $1 $3 }
    | LIdent '(' LCom1Pattern ')' '=' Expr { AbsGrammar.TopFunc $1 $3 $6 }
    | 'let' Pattern '=' Expr { AbsGrammar.TopPat $2 $4 }
    | 'proc' LIdent '(' LCom1Pattern ')' '=' Proc { AbsGrammar.TopProc $2 $4 $7 }
LTop :: { LTop }
LTop : Top ';' LTop { AbsGrammar.LTopCon $1 $3 }
     | Top ';' { AbsGrammar.LTopNil $1 }
{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens
}

