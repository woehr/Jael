{-# Language NoImplicitPrelude #-}
-- Module for handling sequential specific grammar
module Jael.Seq.Expr
( gToEx
) where

import ClassyPrelude
import Data.List.NonEmpty ( NonEmpty( (:|) ))
import qualified Data.List.NonEmpty as NE
import Jael.Grammar
import Jael.Seq.AST
import Jael.Util
import Text.Read (reads)

-- The LetExpr grammar is only allowed in certain places so it isn't of the GExpr type
letExprToEx :: GELetExpr -> Ex
letExprToEx (GELetExpr [] e)    = gToEx e
-- i[dentifier]; h[ead] e[xpression]; t[ail] l[et expression]s; e[xpression]
letExprToEx (GELetExpr (GELetIdent (LIdent i) he : tls) e) =
  ELet (pack i) (gToEx he) (letExprToEx $ GELetExpr tls e)

tupArgToEx :: GETupArg -> Ex
tupArgToEx (GETupArg a) = gToEx a

parseIntErrorMsg :: String
parseIntErrorMsg = "Lexer should not produce MyInteger that " ++
                   "can't be parsed. See definition in Grammar.cf"

parseInt :: IntTok -> Integer
parseInt (IntTok []) = error parseIntErrorMsg
parseInt (IntTok s@(x:xs)) = let bNeg = x == '~'
                                 readRes = if bNeg
                                              then reads xs
                                              else reads s
                             in  case readRes of
                                      [(i, [])] -> if bNeg then -i else i
                                      _         -> error parseIntErrorMsg

-- Helper function to apply arguments to an expression
applyArgs :: Ex -> NE.NonEmpty GEAppArg -> Ex
applyArgs e (GEAppArg a :| as) = case NE.nonEmpty as of
                                      Nothing -> EApp e (gToEx a)
                                      Just ne -> applyArgs (EApp e (gToEx a)) ne

unaryOp :: Text -> GExpr -> Ex
unaryOp op e = EApp (EVar op) (gToEx e)

-- Helper for creating an Ex for binary operators
binOp :: Text -> GExpr -> GExpr -> Ex
binOp op l r = EApp (EApp (EVar op) (gToEx l)) (gToEx r)

-- Converts grammar to AST but does not verify its correctness
gToEx :: GExpr -> Ex
gToEx (GELeftApp   e1 e2) = binOp   "<$" e1 e2
gToEx (GERightApp  e1 e2) = binOp   "$>" e1 e2
gToEx (GELogOr     e1 e2) = binOp   "||" e1 e2
gToEx (GELogAnd    e1 e2) = binOp   "&&" e1 e2
gToEx (GEEq        e1 e2) = binOp   "==" e1 e2
gToEx (GENotEq     e1 e2) = binOp   "!=" e1 e2
gToEx (GEGtEq      e1 e2) = binOp   ">=" e1 e2
gToEx (GELtEq      e1 e2) = binOp   "<=" e1 e2
gToEx (GEGt        e1 e2) = binOp   ">"  e1 e2
gToEx (GELt        e1 e2) = binOp   "<"  e1 e2
gToEx (GEPlus      e1 e2) = binOp   "+"  e1 e2
gToEx (GEMinus     e1 e2) = binOp   "-"  e1 e2
gToEx (GETimes     e1 e2) = binOp   "*"  e1 e2
gToEx (GEDiv       e1 e2) = binOp   "/"  e1 e2
gToEx (GEMod       e1 e2) = binOp   "%"  e1 e2
gToEx (GELeftComp  e1 e2) = binOp   "<o" e1 e2
gToEx (GERightComp e1 e2) = binOp   "o>" e1 e2
gToEx (GELogNot    e    ) = unaryOp "!"  e
gToEx (GEIdx       e1 e2) = EIdx (gToEx e1) (gToEx e2)

gToEx (GEIf b e1 e2) = EApp (EApp (EApp (EVar "if") (gToEx b)) (letExprToEx e1)) (letExprToEx e2)

gToEx (GEApp _ []) = notEnoughElements 1 "GEAppArg" "GEApp"
gToEx (GEApp e as) = applyArgs (gToEx e) (NE.fromList as)

gToEx (GEAbs [] _) = notEnoughElements 1 "GEAbsArg" "GEAbs"
gToEx (GEAbs [GEAbsArg (LIdent i)]      le) = EAbs (pack i) (letExprToEx le)
gToEx (GEAbs (GEAbsArg (LIdent i) : xs) le) = EAbs (pack i) (gToEx $ GEAbs xs le)

gToEx (GEVar (LIdent i)) = EVar (pack i)
gToEx (GEInt i) = EInt $ parseInt i
gToEx (GETrue)  = EBool True
gToEx (GEFalse) = EBool False
gToEx (GEUnit GUnit)  = EUnit

gToEx (GETup xs) = case NE.nonEmpty (map tupArgToEx xs) of
                        Nothing -> notEnoughElements 1 "GETupArg" "GETup"
                        Just (y:|ys) -> foldl' EApp (EApp (EVar $ "tup" ++ tshow (length xs)) y) ys

