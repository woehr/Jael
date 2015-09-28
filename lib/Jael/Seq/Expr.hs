{-# Language NoImplicitPrelude #-}
-- Module for handling sequential specific grammar
module Jael.Seq.Expr
( freeVars
, gToEx
) where

import ClassyPrelude hiding (Foldable)
import Data.Functor.Foldable
import Data.List.NonEmpty ( NonEmpty( (:|) ))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Jael.Grammar
import Jael.Seq.AST
import Jael.Util
import Text.Read (reads)

freeVars :: Ex -> S.Set Text
freeVars = cata alg
  where alg :: ExF (S.Set Text) -> S.Set Text
        alg (EVarF x) = S.singleton x
        alg (EAppF f e) = f `S.union` e
        alg (EAbsF x e) = S.delete x e
        alg (ELetF x e1 e2) = e1 `S.union` (S.delete x e2)
        alg _ = S.empty

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

unaryPrm :: Prm -> GExpr -> Ex
unaryPrm p e = EApp (EPrm p) (gToEx e)

binPrm :: Prm -> GExpr -> GExpr -> Ex
binPrm p l r = EApp (EApp (EPrm p) (gToEx l)) (gToEx r)

binOp :: Text -> GExpr -> GExpr -> Ex
binOp op l r = EApp (EApp (EVar op) (gToEx l)) (gToEx r)

-- Converts grammar to AST but no type checking yet
gToEx :: GExpr -> Ex
--gToEx (GELeftApp   e1 e2) = binOp   "<$" e1 e2
--gToEx (GERightApp  e1 e2) = binOp   "$>" e1 e2
gToEx (GELogOr     e1 e2) = binPrm  POr e1 e2
gToEx (GELogAnd    e1 e2) = binPrm  PAnd e1 e2
gToEx (GEEq        e1 e2) = binPrm  PEq e1 e2
gToEx (GENotEq     e1 e2) = binPrm  PNeq e1 e2
gToEx (GEGtEq      e1 e2) = binPrm  PGeq e1 e2
gToEx (GELtEq      e1 e2) = binPrm  PLeq e1 e2
gToEx (GEGt        e1 e2) = binPrm  PGt  e1 e2
gToEx (GELt        e1 e2) = binPrm  PLt  e1 e2
gToEx (GEPlus      e1 e2) = binPrm  PAdd  e1 e2
gToEx (GEMinus     e1 e2) = binPrm  PSub  e1 e2
gToEx (GETimes     e1 e2) = binPrm  PTimes  e1 e2
gToEx (GEDiv       e1 e2) = binPrm  PDiv  e1 e2
gToEx (GEMod       e1 e2) = binPrm  PMod  e1 e2
gToEx (GELeftComp  e1 e2) = binOp   "<o" e1 e2
gToEx (GERightComp e1 e2) = binOp   "o>" e1 e2
gToEx (GELogNot    e    ) = unaryPrm PNot  e
gToEx (GEBitCat    e1 e2) = binPrm   PBitCat e1 e2

gToEx (GEIf b e1 e2) = EApp (EApp (EApp (EPrm PIf) (gToEx b)) (letExprToEx e1)) (letExprToEx e2)

gToEx (GEApp _ []) = notEnoughElements 1 "GEAppArg" "GEApp"
gToEx (GEApp e as) = applyArgs (gToEx e) (NE.fromList as)

gToEx (GEAbs [] _) = notEnoughElements 1 "GEAbsArg" "GEAbs"
gToEx (GEAbs [GEAbsArg (LIdent i)]      le) = EAbs (pack i) (letExprToEx le)
gToEx (GEAbs (GEAbsArg (LIdent i) : xs) le) = EAbs (pack i) (gToEx $ GEAbs xs le)

gToEx (GEInt i) = ELit . LInt $ parseInt i
gToEx (GETrue)  = ELit $ LBool True
gToEx (GEFalse) = ELit $ LBool False
gToEx (GETup xs) = case NE.nonEmpty (map tupArgToEx xs) of
                        Nothing -> notEnoughElements 1 "GETupArg" "GETup"
                        Just (y:|ys) -> foldl' EApp (EApp (EVar $ "tup" ++ tshow (length xs)) y) ys
gToEx (GEUnit GUnit)  = ELit LUnit
gToEx (GEVar (LIdent i)) = EVar (pack i)
gToEx (GEScopedFn (UIdent t) (GEScopeIdent (LIdent f))) = (EVar . pack $ t ++ "::" ++ f)
gToEx (GEScopedFn (UIdent t) (GEScopeIndex (IntTok n))) = (EVar . pack $ t ++ "::" ++ n)

