{-|
This module does not parse per se (much), but transforms automatically generated
structures from the lbnf grammar definition into internal program structures.
-}
module Jael.Parser where

import qualified Data.Map as M
import qualified Data.Text as T
import           Development.Placeholders
import qualified Language.Fixpoint.Types as L
import           Numeric (readInt, readDec, readHex, readOct)
import qualified Jael.Grammar as G
import           Jael.Expr
import           Jael.Prog
import           Jael.Type
import           Jael.Util

type ParseFun a = [G.Token] -> G.Err a

-- This could use a better name as it's not really a parser error but an
-- error in the program structure.
data ParserErr = PE_DuplicateDefinition T.Text
               deriving (Eq, Show)

insertCheckDups :: Ord a => M.Map a b -> a -> b -> Either a (M.Map a b)
insertCheckDups m k v = case M.lookup k m of
                             Just _ -> Left k
                             Nothing -> Right $ M.insert k v m

runParser :: ParseFun a -> T.Text -> Either T.Text a
runParser p t = case p . G.myLexer . T.unpack $ t of
                     G.Bad err  -> Left (T.pack err)
                     G.Ok  tree -> Right tree

parseTopDef :: ParsedProg -> G.TopDef -> Either ParserErr ParsedProg
parseTopDef p@Prog{..} (G.TopDefGlobal (G.Global1 (G.LIdent (_, n)) e)) =
  case insertCheckDups pExprs (T.pack n) (parseExpr e) of
       Left x -> Left (PE_DuplicateDefinition x)
       Right m -> Right p{pExprs=m}
parseTopDef _ _ = $notImplemented

parseProgram :: G.Prog -> Either ParserErr ParsedProg
parseProgram (G.Prog1 defs) = foldM parseTopDef emptyProg defs

parseIntErrorMsg :: String
parseIntErrorMsg = "Lexer should not produce integer strings that parse\
                   \ incorrectly. See definition in Grammar.cf"

readBin :: ReadS Integer
readBin = readInt 2 (\x -> x == '0' || x == '1') digitToInt

intDoParse :: ReadS Integer -> String -> Integer
intDoParse p xs =
  case p xs of
       [(i, [])] -> i
       _         -> error parseIntErrorMsg

parseAnyInt :: G.AnyInt -> Integer
parseAnyInt (G.AnyIntDecInt x) = parseDecInt x
parseAnyInt (G.AnyIntHexInt x) = parseHexInt x
parseAnyInt (G.AnyIntOctInt x) = parseOctInt x
parseAnyInt (G.AnyIntBinInt x) = parseBinInt x

parseDecInt :: G.DecInt -> Integer
parseDecInt (G.DecInt (_, s@(x:xs))) =
  if x == '~'
     then -(intDoParse readDec xs)
     else   intDoParse readDec s
parseDecInt _ = error parseIntErrorMsg

parseHexInt :: G.HexInt -> Integer
parseHexInt (G.HexInt ((_, '0':'x':xs))) = intDoParse readHex xs
parseHexInt _ = error parseIntErrorMsg

parseOctInt :: G.OctInt -> Integer
parseOctInt (G.OctInt ((_, 'o':xs))) = intDoParse readOct xs
parseOctInt _ = error parseIntErrorMsg

parseBinInt :: G.BinInt -> Integer
parseBinInt (G.BinInt ((_, 'b':xs))) = intDoParse readBin xs
parseBinInt _ = error parseIntErrorMsg

parseELetElem :: G.ELetElem -> (T.Text, MaybeTypedExpr)
parseELetElem (G.ELetElem1 (G.LIdent (_, n)) e) = (T.pack n, parseExpr e)

parseELetExpr :: G.ELetExpr -> MaybeTypedExpr
parseELetExpr (G.ELetExpr1 [] e) = parseExpr e
parseELetExpr (G.ELetExpr1 ls e) =
--  let folder (n, x) acc = ELet n x acc
  foldr (\(n, e1) e2 -> mkUntypedExpr (ELetF n e1 e2)) (parseExpr e) (map parseELetElem ls)

parseBType :: G.BType -> BaseType
parseBType G.TUnit = BTUnit
parseBType G.TBool = BTBool
parseBType G.TInt  = BTInt
parseBType _ = $(todo "Finish implementing function")

parseQPred :: G.QPred -> L.Expr
parseQPred = $notImplemented

parseQType :: G.QType -> Type
parseQType (G.QType1 (G.LIdent (_, n)) b p) = TQual (VV $ T.pack n) (parseBType b) (parseQPred p)

parseType :: G.Type -> Type
parseType (G.TypeB b) = TBase (parseBType b)
parseType (G.TypeQ q) = parseQType q
parseType (G.TypeV (G.LIdent (_, n))) = TVar (T.pack n)

parseExpr :: G.Expr -> MaybeTypedExpr
parseExpr (G.EInt i) = mkUntypedExpr $ EConF (CInt $ parseDecInt i)
parseExpr (G.ETrue)  = mkUntypedExpr $ EConF (CBool True)
parseExpr (G.EFalse) = mkUntypedExpr $ EConF (CBool False)
parseExpr (G.EUnit)  = mkUntypedExpr $ EConF CUnit
parseExpr (G.EVar (G.LIdent (_, i))) = mkUntypedExpr $ EVarF (T.pack i)
parseExpr _ = $(todo "Finish implementing function")

--gToS1Ex (GELogOr     e1 e2) = binPrm  POr e1 e2
--gToS1Ex (GELogAnd    e1 e2) = binPrm  PAnd e1 e2
--gToS1Ex (GEEq        e1 e2) = binPrm  PEq e1 e2
--gToS1Ex (GENotEq     e1 e2) = binPrm  PNeq e1 e2
--gToS1Ex (GEGtEq      e1 e2) = binPrm  PGeq e1 e2
--gToS1Ex (GELtEq      e1 e2) = binPrm  PLeq e1 e2
--gToS1Ex (GEGt        e1 e2) = binPrm  PGt  e1 e2
--gToS1Ex (GELt        e1 e2) = binPrm  PLt  e1 e2
--gToS1Ex (GEPlus      e1 e2) = binPrm  PAdd  e1 e2
--gToS1Ex (GEMinus     e1 e2) = binPrm  PSub  e1 e2
--gToS1Ex (GETimes     e1 e2) = binPrm  PTimes  e1 e2
--gToS1Ex (GEDiv       e1 e2) = binPrm  PDiv  e1 e2
--gToS1Ex (GEMod       e1 e2) = binPrm  PMod  e1 e2
--gToS1Ex (GEBitCat    e1 e2) = binPrm  PBitCat e1 e2
--gToS1Ex (GELogNot    e    ) = S1CallPrm PNot [gToS1Ex e]
--
--gToS1Ex (GEIf b e1 e2) = S1If (gToS1Ex b) (gLetToS1Ex e1) (gLetToS1Ex e2)
--
--gToS1Ex (GEApp _ []) = error "The grammar should ensure there is always at\
--                            \ least one argument to an application."
--gToS1Ex (GEApp (LIdent n) as) = S1Call (pack n) (map (\(GEAppArg x) -> gToS1Ex x) as)
--gToS1Ex (GEAppScoped _ []) = error "The grammar should ensure there is at least\
--                                  \ one argument to a (scoped) application."
--gToS1Ex (GEAppScoped (LScopedIdent n) as) = S1Call (pack n) (map (\(GEAppArg x) -> gToS1Ex x) as)
--
--gToS1Ex (GETup x xs) = if null xs
--                          then error "The grammar should ensure at least 2\
--                                    \ tuple arguments."
--                          else S1Tup (map (\(GETupArg a) -> gToS1Ex a) (x:xs))

