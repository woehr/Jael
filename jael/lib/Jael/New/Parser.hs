{-# Language OverloadedStrings #-}

module Jael.New.Parser
where

import Prelude hiding (try)
import qualified Data.HashSet as H
import qualified Data.Text as T
import Text.Trifecta
import Text.Parser.Token.Highlight

import Jael.New.Types
import Jael.New.Expr

identifierStyle :: String -> Parser Char -> IdentifierStyle Parser
identifierStyle name p = IdentifierStyle
    { _styleName      = name
    , _styleStart     = p
    , _styleLetter    = oneOf (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_")
    , _styleReserved  = reservedIdentifiers
    , _styleHighlight = Identifier
    , _styleReservedHighlight = ReservedIdentifier
    }

reservedIdentifiers :: H.HashSet String
reservedIdentifiers = H.fromList
  -- Types
  [ "forall"
  -- Exprs
  , "case"
  , "of"
  , "if"
  , "then"
  , "else"
  , "let"
  -- Top level
  , "func"
  , "proc"
  , "type"
  , "data"
  , "enum"
  , "struct"
  ]

-- Improvement: Get a list of all BuiltinFunc constructors, create a set of
-- characters from their strings, and use that (so adding stuff won't break this)
builtinChars :: String
builtinChars = "|&!=><+-*/%#"

lident :: Parser T.Text
lident = ident (identifierStyle "lowercase identifier" $ oneOf $ ['a'..'z'] ++ "_")

uident :: Parser T.Text
uident = ident (identifierStyle "uppercase identifier" $ oneOf ['A'..'Z'])

number :: TokenParsing m => Integer -> m Char -> m Integer
number base baseDigit =
  foldl' (\x d -> base*x + toInteger (digitToInt d)) 0 <$> some baseDigit

binDigit :: Parser Char
binDigit = oneOf "01" <?> "binary digit"

binInt :: Parser Integer
binInt = string "0b" *> number 2 binDigit

octInt :: Parser Integer
octInt = string "0o" *> number 8 octDigit

hexInt :: Parser Integer
hexInt = string "0x" *> number 16 hexDigit

decInt :: Parser Integer
decInt = let dec :: Parser Integer
             dec = number 10 digit
         in negate <$ char '~' <*> dec <|> dec

anyint :: Parser Integer
anyint =  try binInt
      <|> try octInt
      <|> try hexInt
      <|>     decInt

pConstant :: Parser Constant
pConstant =  CInt   <$> anyint
         <|> CChar  <$> charLiteral

reserved :: T.Text -> Parser ()
reserved =
  reserveText (identifierStyle "reserved" $ oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ "_")

optionalList :: Parser [a] -> Parser [a]
optionalList = fmap concat . optional

commaSep2 :: Parser a -> Parser [a]
commaSep2 p = (:) <$> p <* comma <*> commaSep1 p

{-
  s := forall l+. t
     | t
-}
pScheme :: Parser Type
pScheme =  TAll <$> between (reserved "forall") (symbol ".") (some lident) <*> pType0
       <|> pType0

{-
  t0 := t1 -> t0 // right assoc.
      | t1
-}
pType0 :: Parser Type
pType0 =  chainr1 pType1 (TFun <$ symbol "->")
      <|> pType1

{-
  t1 := l
      | u
      | u(t0,* t0)
      | (t0,+ t0)
      | { lident:t1,* lident:t1 }
      | (t0)
-}
pType1 :: Parser Type
pType1 =
  let     labelledType :: Parser (T.Text, Type)
          labelledType = (,) <$> lident <* colon <*> pType1
  in      TVar <$> lident
      <|> TCon <$> (Just <$> uident) <*> optionalList (parens $ commaSep1 pType0)
      <|> TCon Nothing <$> try (parens $ commaSep2 pType0)
      <|> TRec <$> braces (commaSep1 labelledType)
      <|> parens pType0

{-
  p := _
     | ...
     | int, char
     | {l=p,* l=p}
     | [p,* p]
     | (p,+ p)
     | l
     | l(p,* p)
-}
pPattern :: Parser Pattern
pPattern =
  let labelledPattern :: Parser (T.Text, Pattern)
      labelledPattern = (,) <$> lident <* symbolic '=' <*> pPattern
  in        PWild      <$ symbol "_"
        <|> PMultiWild <$ symbol "..."
        <|> PConst  <$> pConstant
        <|> PRec    <$> braces (commaSep labelledPattern)
        <|> PArr    <$> brackets (commaSep1 pPattern)
        <|> PPat "" <$> try (parens $ commaSep2 pPattern)
        <|> PPat    <$> lident <*> optionalList (parens $ commaSep1 pPattern)

-- The type of expressions parsed from a source file
type E = Expr

{-
  b  := l :t0?
  e0 := \(b,* b) -> e0
      | e1
-}
pExpr0 :: Parser E
pExpr0 =  let commaSepBinds = commaSep1 $ (,) <$> lident
                                              <*> optional (colon *> pType0)
          in  (flip . foldr . uncurry $ EAbs) <$  symbol "\\"
                                              <*> parens commaSepBinds
                                              <*  symbol "->"
                                              <*> pExpr0
      <|> pExpr1

{-
  alt := p -> e0
  e1  := case e2 of { alt;* alt }
-}
pExpr1 :: Parser E
pExpr1 =  let alt = (,) <$> pPattern
                        <*  symbol "->"
                        <*> pExpr0
          in  ECase <$  reserved "case"
                    <*> pExpr2
                    <*  reserved "of"
                    <*> braces (semiSep1 alt)
      <|> pExpr2

{-
  e2 := if e3 then e0 else e0
-}
pExpr2 :: Parser E
pExpr2 = EIf <$ reserved "if"   <*> pExpr3
             <* reserved "then" <*> pExpr0
             <* reserved "else" <*> pExpr0
      <|> pExpr3

{-
  let := p = e0
  e3  := { let;+ e0 }

  Patterns on the left side of let expressions allow us to do interesting things?
  Consider the syntactically valid {1=x;x}, would it be useful to desugar to
  case x of {1 -> x; _ -> impossible}
-}
pExpr3 :: Parser E
pExpr3 =  let e = (,) <$> pPattern
                      <*  symbolic '='
                      <*> pExpr0
                      <*  semi
          in try (braces $ ELet <$> some (try e) <*> pExpr0)
      <|> pExpr4

pBuiltin :: BuiltinFunc -> Parser E
pBuiltin f = EBuiltin f <$ (try $ symbol (symbolOf f) <* notFollowedBy (oneOf builtinChars))

-- Like oneOf but for BuiltinFunc
builtinOf :: [BuiltinFunc] -> Parser E
builtinOf = foldr (\f p -> pBuiltin f <|> p) empty

binApp :: E -> E -> E -> E
binApp l op r = EApp (EApp op l) r

{-
  noAssoc p fs =  try (binApp <$> p <*> builtinOf fs) <*> p
              <|> p
-}
noAssoc :: Parser E -> [BuiltinFunc] -> Parser E
noAssoc p fs = p >>= \x -> binApp x <$> builtinOf fs <*> p <|> pure x

leftAssoc :: Parser E -> [BuiltinFunc] -> Parser E
leftAssoc p fs = p `chainl1` (flip binApp <$> builtinOf fs)

--EIff.       Expr4 ::= Expr5 "<->" Expr5 ;
pExpr4 :: Parser E
pExpr4 =  noAssoc pExpr5 [BFIff]

--EImp.       Expr5 ::= Expr6 "-->" Expr6 ;
pExpr5 :: Parser E
pExpr5 =  noAssoc pExpr6 [BFImp]

--ELogOr.     Expr6 ::= Expr7 "||" Expr6 ;
pExpr6 :: Parser E
pExpr6 =  leftAssoc pExpr7 [BFOr]

--ELogAnd.    Expr7 ::= Expr8 "&&" Expr7 ;
pExpr7 :: Parser E
pExpr7 =  leftAssoc pExpr8 [BFAnd]

--EEq.        Expr8 ::= Expr9 "==" Expr9 ;
--ENotEq.     Expr8 ::= Expr9 "!=" Expr9 ;
--EGtEq.      Expr8 ::= Expr9 ">=" Expr9 ;
--ELtEq.      Expr8 ::= Expr9 "<=" Expr9 ;
--EGt.        Expr8 ::= Expr9 ">"  Expr9 ;
--ELt.        Expr8 ::= Expr9 "<"  Expr9 ;
pExpr8 :: Parser E
pExpr8 = noAssoc pExpr9 [BFEq, BFNe, BFGt, BFGe, BFLt, BFLe]

--EPlus.      Expr9 ::= Expr9 "+" Expr10 ;
--EMinus.     Expr9 ::= Expr9 "-" Expr10 ;
pExpr9 :: Parser E
pExpr9 = leftAssoc pExpr10 [BFAdd, BFSub]

--ETimes.     Expr10 ::= Expr10 "*" Expr11 ;
--EDiv.       Expr10 ::= Expr10 "/" Expr11 ;
--EMod.       Expr10 ::= Expr10 "%" Expr11 ;
pExpr10 :: Parser E
pExpr10 = leftAssoc pExpr11 [BFTimes, BFDiv, BFMod]

--EBitCat.    Expr11  ::= Expr12 "#" Expr11 ;
pExpr11 :: Parser E
pExpr11 = leftAssoc pExpr12 [BFCat]

pExpr12 :: Parser E
pExpr12 =  EApp (EBuiltin BFNot) <$ pBuiltin BFNot <*> pExpr99
       <|> pExpr99

appArgs :: E -> [E] -> E
appArgs = foldl' EApp

{-
  e99 := anyint
       | charLiteral
       | lident
       | lident(e0,* e0)
       | { l=e0,* l=e0 }
       | (e0,+ e0)
       | (e0)
-}
pExpr99 :: Parser E
pExpr99 =
  let      labelledExpr :: Parser (T.Text, E)
           labelledExpr = (,) <$> lident <* symbolic '=' <*> pExpr0
  in       EConst <$> pConstant
           -- An lident followed by comma separated expressions or nothing
       <|> (EVar <$> lident >>= \e -> appArgs e <$> (parens $ commaSep1 pExpr0) <|> pure e)
       <|> ERec  <$> braces (commaSep labelledExpr)
       <|> ETup  <$> try (parens $ commaSep2 pExpr0)
       <|> parens pExpr0

data DataDecl = DataDecl [(T.Text, [Type])]
              deriving (Eq, Show)

pData :: Parser DataDecl
pData = reserved "data" $> DataDecl []

data EnumDecl = EnumDecl [(T.Text, Integer)]
              deriving (Eq, Show)

pEnum :: Parser EnumDecl
pEnum = reserved "enum" $> EnumDecl []

data StructDecl = StructDecl [(T.Text, Type)]
                deriving (Eq, Show)

pStruct :: Parser StructDecl
pStruct = reserved "struct" $> StructDecl []
