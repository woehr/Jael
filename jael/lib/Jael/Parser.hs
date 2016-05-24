{-|
This module does not parse per se (much), but transforms automatically generated
structures from the lbnf grammar definition into internal program structures.
-}
module Jael.Parser where

import qualified Data.Map as M
import qualified Data.Text as T
--import           Development.Placeholders
import qualified Language.Fixpoint.Types as L
import qualified Jael.Grammar as G
import           Jael.Expr
import           Jael.Prog
import           Jael.Type
import           Jael.Util

type ParseFun a = [G.Token] -> G.Err a

-- This could use a better name as it's not really a parser error but an
-- error in the program structure.
data ParserErr = PE_DuplicateDefinition T.Text
               | PE_MultipleTypeAnn
               deriving (Eq, Show)

type ParserErrM = Either ParserErr

insertCheckDups :: Ord a => M.Map a b -> a -> b -> Either a (M.Map a b)
insertCheckDups m k v = case M.lookup k m of
                             Just _ -> Left k
                             Nothing -> Right $ M.insert k v m

runParser :: ParseFun a -> T.Text -> Either T.Text a
runParser p t = case p . G.myLexer . T.unpack $ t of
                     G.Bad err  -> Left (T.pack err)
                     G.Ok  tree -> Right tree

parseUIdent :: G.UIdent -> T.Text
parseUIdent (G.UIdent (_, n)) = T.pack n

parseLIdent :: G.LIdent -> T.Text
parseLIdent (G.LIdent (_, n)) = T.pack n

parseLScoped :: G.LScopedIdent -> T.Text
parseLScoped (G.LScopedIdent (_, n)) = T.pack n

parseFuncArg :: G.FuncArg -> (MaybeTypedExpr, Type) -> (MaybeTypedExpr, Type)
parseFuncArg (G.FuncArg1 n t1) (e, t2) =
  let argTy = parseType t1
      absTy = TFun argTy t2
  in  (mkAnnExpr (EAbsF (parseLIdent n) e) (Just absTy), absTy)

parseFunc :: G.Func -> ParserErrM (T.Text, MaybeTypedExpr)
parseFunc (G.Func1 n as ret e) =
  let AnnExpr (Ann ft fe) = parseExpr e
      bodyType = parseType ret
      bodyExpr = mkAnnExpr fe $ Just bodyType
  in  case ft of
        Just _  -> throwError PE_MultipleTypeAnn
        Nothing -> return (parseLIdent n, fst $ foldr parseFuncArg (bodyExpr, bodyType) as)

parseGlobal :: G.Global -> ParserErrM (T.Text, MaybeTypedExpr)
parseGlobal (G.Global1 n e) = return (parseLIdent n, parseExpr e)


parseTopDef :: ParsedProg -> G.TopDef -> ParserErrM ParsedProg
parseTopDef p@Prog{..} (G.TopDefGlobal (G.Global1 n e)) =
  case insertCheckDups pExprs (parseLIdent n) (parseExpr e) of
       Left x -> Left (PE_DuplicateDefinition x)
       Right m -> Right p{pExprs=m}

parseTopDef p@Prog{..} (G.TopDefFunc f) = do
  (n, e) <- parseFunc f
  case insertCheckDups pExprs n e of
       Left x  -> throwError (PE_DuplicateDefinition x)
       Right m -> return p{pExprs=m}

parseTopDef p@Prog{..} (G.TopDefTypeDef (G.TDefProto n s)) = undefined

parseProgram :: G.Prog -> ParserErrM ParsedProg
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
parseHexInt (G.HexInt (_, '0':'x':xs)) = intDoParse readHex xs
parseHexInt _ = error parseIntErrorMsg

parseOctInt :: G.OctInt -> Integer
parseOctInt (G.OctInt (_, 'o':xs)) = intDoParse readOct xs
parseOctInt _ = error parseIntErrorMsg

parseBinInt :: G.BinInt -> Integer
parseBinInt (G.BinInt (_, 'b':xs)) = intDoParse readBin xs
parseBinInt _ = error parseIntErrorMsg

parseLetElem :: G.LetElem -> (T.Text, MaybeTypedExpr)
parseLetElem (G.LetElem1 n e) = (parseLIdent n, parseExpr e)

parseLetExpr :: G.LetExpr -> MaybeTypedExpr
parseLetExpr (G.LetExpr1 [] e) = parseExpr e
parseLetExpr (G.LetExpr1 ls e) =
  foldr ((\(n, e1) e2 -> mkUntypedExpr (ELetF n e1 e2)) . parseLetElem)
        (parseExpr e)
        ls

parseBType :: G.BType -> BaseType
parseBType G.TUnit   = BTUnit
parseBType G.TBool   = BTBool
parseBType G.TInt    = BTInt
parseBType G.TBit    = BTBit
parseBType (G.TBuffer t) = BTBuffer (parseType t)

parseQPred :: G.QPred -> L.Expr
parseQPred (G.QPredIff l r) = L.PIff (parseQPred l) (parseQPred r)
parseQPred (G.QPredImp l r) = L.PImp (parseQPred l) (parseQPred r)
parseQPred (G.QPredOr  l r) = L.POr  (map parseQPred [l, r])
parseQPred (G.QPredAnd l r) = L.PAnd (map parseQPred [l, r])
parseQPred (G.QPredNot p)   = L.PNot (parseQPred p)
parseQPred G.QPredTrue      = L.prop True
parseQPred G.QPredFalse     = L.prop False
parseQPred (G.QPredAtom l op r) = L.PAtom (parseQRel op) (parseQExpr l) (parseQExpr r)

parseQExpr :: G.QExpr -> L.Expr
parseQExpr (G.QExprAdd l r)  = L.EBin L.Plus (parseQExpr l) (parseQExpr r)
parseQExpr (G.QExprSub l r)  = L.EBin L.Minus (parseQExpr l) (parseQExpr r)
parseQExpr (G.QExprMul c e)  = L.EBin L.Times (L.expr $ parseAnyInt c) (parseQExpr e)
parseQExpr (G.QExprInt i)    = L.expr $ parseAnyInt i
parseQExpr (G.QExprVar n)    = L.eVar $ parseLIdent n
parseQExpr (G.QExprApp n (a:as)) =
  foldl'
    L.EApp
    (L.EApp (L.EVar $ L.symbol $ parseLIdent n) (parseCommaSepQExpr a))
    (map parseCommaSepQExpr as)
parseQExpr (G.QExprApp _ _) = error "[G.CommaSepQExpr] should always have an element"

parseCommaSepQExpr :: G.CommaSepQExpr -> L.Expr
parseCommaSepQExpr (G.CommaSepQExprQExpr e) = parseQExpr e

parseQRel :: G.QRel -> L.Brel
parseQRel G.QRelEq = L.Eq
parseQRel G.QRelNe = L.Ne
parseQRel G.QRelGe = L.Ge
parseQRel G.QRelLe = L.Le
parseQRel G.QRelGt = L.Gt
parseQRel G.QRelLt = L.Lt

parseQType :: G.QType -> Type
parseQType (G.QTypeVVTy n b p) = TQual (VV $ parseLIdent n) (Just $ parseBType b) (parseQPred p)
parseQType (G.QTypeNoVVTy b p) = TQual VVFresh              (Just $ parseBType b) (parseQPred p)
parseQType (G.QTypeNoVVNoTy p) = TQual VVFresh              Nothing               (parseQPred p)

parseCommaSepType :: G.CommaSepType -> Type
parseCommaSepType (G.CommaSepTypeType t) = parseType t

parseTNamedParams :: G.TNamedParams -> [Type]
parseTNamedParams G.TNamedParamsEmpty = []
parseTNamedParams (G.TNamedParamsList ts) = map parseCommaSepType ts

parseType :: G.Type -> Type
parseType (G.TypeBase b)     = TBase (parseBType b)
parseType (G.TypeQual q)     =       parseQType q
parseType (G.TypeVar  n)     = TVar  (parseLIdent n)
parseType (G.TypeTup t ts)   = TTup $ map parseCommaSepType (t:ts)
parseType (G.TypeNamed n ps) = TNamed (parseUIdent n) $ parseTNamedParams ps

parseCommaSepExpr :: G.CommaSepExpr -> MaybeTypedExpr
parseCommaSepExpr (G.CommaSepExprExpr e) = parseExpr e

parseETup :: [MaybeTypedExpr] -> MaybeTypedExpr
parseETup (x:[]) = x
parseETup (x:ys) = mkTupExpr x (parseETup ys)
parseETup _ = error "parseETup should always be given a list with 2 elements"

parseExpr :: G.Expr -> MaybeTypedExpr
parseExpr (G.EInt i) = mkConstExpr (CInt $ parseDecInt i)
parseExpr G.ETrue    = mkConstExpr (CBool True)
parseExpr G.EFalse   = mkConstExpr (CBool False)
parseExpr G.EUnit    = mkConstExpr CUnit
parseExpr (G.EVar i) = mkVarExpr $ parseLIdent i
parseExpr (G.ETup e1 es) = parseETup $ map parseCommaSepExpr (e1:es)
parseExpr (G.EApp n es) = mkAppExpr (mkVarExpr $ parseLIdent n) (map parseCommaSepExpr es)
parseExpr (G.EAppScoped n es) = mkAppExpr (mkVarExpr $ parseLScoped n) (map parseCommaSepExpr es)
parseExpr (G.EAnn e t) =
   case parseExpr e of
        AnnExpr (Ann (Just _) _) -> error "Attempted to annotate an expression multiple times."
        AnnExpr (Ann Nothing e') -> AnnExpr (Ann (Just (parseType t)) e')

parseExpr (G.EIf b t e) = mkUntypedExpr $ EIteF (parseExpr b) (parseLetExpr t) (parseLetExpr e)
parseExpr (G.ELogOr  l r) = mkApp COr  [l, r]
parseExpr (G.ELogAnd l r) = mkApp CAnd [l, r]
parseExpr (G.EEq     l r) = mkApp CEq  [l, r]
parseExpr (G.ENotEq  l r) = mkApp CNe  [l, r]
parseExpr (G.EGtEq   l r) = mkApp CGe  [l, r]
parseExpr (G.ELtEq   l r) = mkApp CLe  [l, r]
parseExpr (G.EGt     l r) = mkApp CGt  [l, r]
parseExpr (G.ELt     l r) = mkApp CLt  [l, r]
parseExpr (G.EPlus   l r) = mkApp CAdd [l, r]
parseExpr (G.EMinus  l r) = mkApp CSub [l, r]
parseExpr (G.ETimes  l r) = mkApp CMul [l, r]
parseExpr (G.EDiv    l r) = mkApp CDiv [l, r]
parseExpr (G.EMod    l r) = mkApp CMod [l, r]
parseExpr (G.EBitCat l r) = mkApp CBitCat [l, r]
parseExpr (G.ELogNot e  ) = mkApp CNot [e]

mkApp :: Constant -> [G.Expr] -> MaybeTypedExpr
mkApp f as = mkAppExpr (mkConstExpr f) $ map parseExpr as
