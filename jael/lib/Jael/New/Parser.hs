{-# Language DeriveFunctor #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
{-# Language TupleSections #-}

module Jael.New.Parser where

import Prelude hiding (try)
import qualified Control.Comonad.Trans.Cofree as C
import qualified Data.HashSet as H
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Trifecta
import Text.Parser.Token.Highlight

import Jael.New.DataDecl
import Jael.New.Expr
import Jael.New.Misc
import Jael.New.Type
import Jael.New.QType

-- The type of expressions parsed from a source file
--type P = Cofree PatternF Span
type P = PTree PatternF
type E = PTree (ExprF () P T.Text)
type T = PTree (QTypeF T.Text E)

identifierStyle :: TokenParsing m => String -> m Char -> IdentifierStyle m
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
  , "sizeof"
  ]

-- Improvement: Get a list of all BuiltinFunc constructors, create a set of
-- characters from their strings, and use that (so adding stuff won't break this)
opChars :: String
opChars = "\\|&!=><+-*/%#$"

reserved :: (TokenParsing m, Monad m) => T.Text -> m ()
reserved = reserveText
         $ identifierStyle "reserved"
         $ oneOf $ ['a'..'z'] ++ ['A'..'Z']

spannit :: (DeltaParsing p, Functor f)
        => p (f (PTree f)) -> p (PTree f)
spannit p = flip fmap (spanned p) $ \(x :~ s) -> s :< x

ifGtOne' :: DeltaParsing m
         => ([PTree f] -> f (PTree f))
         -> m [PTree f] -> m (PTree f)
ifGtOne' f pxs = flip fmap (spanned pxs) $ \case
  ([x] :~ _) -> x
  (xs  :~ s) -> s :< f xs

lident :: (TokenParsing m, Monad m) => m T.Text
lident = ident
       $ identifierStyle "lowercase identifier"
       $ oneOf $ ['a'..'z']

tsymbol :: (TokenParsing m, Monad m) => String -> m T.Text
tsymbol = symbol >=> return . T.pack

tsymbolic :: (TokenParsing m, Monad m) => Char -> m T.Text
tsymbolic = symbolic >=> return . T.singleton

uident :: (TokenParsing m, Monad m) => m T.Text
uident = ident
       $ identifierStyle "uppercase identifier"
       $ oneOf ['A'..'Z']

label :: (TokenParsing m, Monad m) => m T.Text
label = lident <|> (whole >>= \JInt{..} -> return . T.pack . show $ intValue)

rArrow :: (TokenParsing m, Monad m) => m T.Text
rArrow =  tsymbol "->"
      <|> tsymbolic '→'
      <|> tsymbolic '⟶'

lArrow :: (TokenParsing m, Monad m) => m T.Text
lArrow =  tsymbol "<-"
      <|> tsymbolic '←'
      <|> tsymbolic '⟵'

bar :: (TokenParsing m, Monad m) => m T.Text
bar = tsymbolic '|'

bananas :: TokenParsing m => m a -> m a
bananas p =  (nesting . between (symbol "(|")  (symbol "|)"))  p
         <|> (nesting . between (symbolic '⦇') (symbolic '⦈')) p
         <|> (nesting . between (symbolic '⦅') (symbolic '⦆')) p

wildcard :: (TokenParsing m, Monad m) => m T.Text
wildcard = tsymbolic '_'

patOr :: (TokenParsing m, Monad m) => m T.Text
patOr = (   tsymbol "\\/"
        <|> tsymbolic '⋁'
        <|> tsymbolic '∨'
        <|> tsymbolic '⋎'
        )

forall :: (TokenParsing m, Monad m) => m ()
forall =  reserved "forall"
      <|> tsymbolic '∀' *> pure ()

-- number' :: base -> digit parser -> (value, number of characters)
number :: TokenParsing m => Integer -> m Char -> m (Integer, Integer)
number base baseDigit = token $
  (\xs -> ( foldl' (\x d -> base*x + toInteger (digitToInt d)) 0 xs
          , toInteger $ length xs)
  ) <$> some baseDigit

binDigit :: CharParsing m => m Char
binDigit = oneOf "01" <?> "binary digit"

binInt :: TokenParsing m => m JInt
binInt =  string "0b"
       $> uncurry (JInt BinInt)
      <*> number 2 binDigit

octInt :: TokenParsing m => m JInt
octInt =  string "0o"
       $> uncurry (JInt OctInt)
      <*> number 8 octDigit

hexInt :: TokenParsing m => m JInt
hexInt =  string "0x"
       $> uncurry (JInt HexInt)
      <*> number 16 hexDigit

whole :: TokenParsing m => m JInt
whole = uncurry (JInt DecInt) <$> number 10 digit

negJInt :: JInt -> JInt
negJInt i@(JInt { intValue = v }) = i { intValue = negate v }

decInt :: TokenParsing m => m JInt
decInt =  char '~' *> (negJInt <$> whole)
      <|> whole

anyint :: TokenParsing m => m JInt
anyint =  try binInt
      <|> try octInt
      <|> try hexInt
      <|>     decInt

intSize :: (TokenParsing m, Monad m) => m SizeSpec
intSize = (intValue <$> decInt >>= \d -> BitSize  d <$ char 'b'
                                     <|> ByteSize d <$ char 'B'
                                     <|> KiloSize d <$ char 'K'
                                     <|> MegaSize d <$ char 'M')

sizeOf :: Parser SizeSpec
sizeOf = SizeOf <$ reserved "sizeof" <*> parens lident

ifGtOne :: Monad m => ([a] -> a) -> m [a] -> m a
ifGtOne f mxs = mxs >>= \xs -> case xs of
  (x:[]) -> pure x
  _      -> pure $ f xs

{-
  s := decInt (oneOf "bBKM")
     | sizeof(ident)
     | s+s
-}
pSizeSpec :: Parser SizeSpec
pSizeSpec = ifGtOne SizedSum $ (intSize <|> sizeOf) `sepBy1` symbolic '+'

pConstant :: TokenParsing m => m Constant
pConstant =  CInt   <$> anyint
         <|> CChar  <$> charLiteral

optionalList :: Parser [a] -> Parser [a]
optionalList = fmap concat . optional

commaSep2 :: Parser a -> Parser [a]
commaSep2 p = (:) <$> p <* comma <*> commaSep1 p

{-
  t1 := l
      | u
      | u(t0,* t0)
      | { lident:t0,* lident:t0 }
      | [t0; anyint]
      | (t0,+ t0)
      | (t0)
-}
pBaseType :: Parser (TypeF T.Text T)
pBaseType =
  (     TVarF <$> lident
    <|> TConF <$> uident <*> optionalList (parens $ commaSep1 pType1)
    <|> brackets (TArrF <$> pType1 <* semi <*> (intValue <$> anyint))
    <|> (braces $
              try (TRecF . Row [] <$> optional lident <* notFollowedBy colon)
          <|> TRecF <$>
                (Row <$> (commaSep $ (,) <$> lident <* colon <*> pType1)
                     <*> optional (bar *> lident)
                )
        )
    <|> try (parens $ pType1 >>= \t -> TTupF . (t:) <$> some (comma *> pType1))
  )
  <|> try (parens pBaseType)

pType0 :: Parser T
pType0 =  spannit (UQAllF <$> between forall dot (some lident) <*> pType1)
      <|> pType1

pType1 :: Parser T
pType1 =  chainr1 pType2 (pSpanBinary $ UQFunF <$ rArrow)
      <|> pType2

pType2 :: Parser T
pType2 = spannit (bananas $
           (\l t e -> Just (l,e) C.:< t) <$> lident
                                         <*  colon
                                         <*> pBaseType
                                         <*  bar
                                         <*> pExpr0
           )
      <|> spannit ((Nothing C.:<) <$> pBaseType)
      <|> parens pType1

{-
  p0 := p1|+ p1
      | p1
  p1 := _
      | ...
      | int, char
      | {l=p,* l=p}
      | [p,* p]
      | $l @p?
      | l
      | l(p,* p)
      | (p,+ p)
      | (p0)
-}

--ifGtOne :: Monad m => ([a] -> a) -> m [a] -> m a
--pPattern0 =  ifGtOne POrF $ spanned (pPattern1 `sepBy1` bar)
pPattern0 :: Parser P
pPattern0 =  ifGtOne' POrF (pPattern1 `sepBy1` patOr)

pPattern1 :: Parser P
pPattern1 =
  let labelledPattern :: Parser (T.Text, P)
      labelledPattern = (,) <$> lident <* symbolic '=' <*> pPattern0
   in spannit
        (    PWildF      <$  wildcard
         <|> PMultiWildF <$  symbol "..."
         <|> PConstF     <$> pConstant
         <|> PRecF [] Nothing <$ try (symbolic '{' <* symbolic '}')
         <|> uncurry PRecF <$> braces ((,)
             <$> (commaSep1 labelledPattern)
             <*> (optional $ bar *> (symbolic '$' *> lident <|> wildcard))
             )
         <|> PArrF       <$> brackets (commaSep pPattern0)
         <|> PBindF      <$  symbolic '$'
                               <*> lident
                               <*> optional (symbolic '@' *> parens pPattern0)
         <|> PPatF       <$> lident
                               <*> optionalList (parens $ commaSep1 pPattern0)
        )
      <|> parens (ifGtOne' PTupF $ pPattern0 `sepBy1` symbolic ',')

caseBody :: Parser [(P, E)]
caseBody = let alt = (,) <$> pPattern0
                         <*  symbol "->"
                         <*> pExpr0
           in  braces (semiSep1 alt)

pOp :: HasSymbol a => a -> Parser E
pOp x = spannit $ exprConstructor x <$
  (try $ symbol (symbolOf x) <* notFollowedBy (oneOf opChars))

pOpOf :: HasSymbol a => [a] -> Parser E
pOpOf = foldr (\f p -> pOp f <|> p) empty

pSpanBinary :: Parser (PTree f -> PTree g -> f (PTree f))
            -> Parser (PTree f -> PTree g -> PTree f)
pSpanBinary pBinOp = do
  op <- pBinOp
  return $ \l@(ls :< _) r@(rs :< _) ->
    ls <> rs :< op l r

-- Left fold with span
spanLeft :: (PTree f -> a -> f (PTree f))
         -> PTree f -> [Spanned a] -> PTree f
spanLeft f = foldl' f' where
  f' (l@(ls :< _)) (r :~ rs) = ls <> rs :< f l r

binApp :: E -> E -> E -> ExprF () P T.Text E
binApp l op r = EAppF op [l, r]

pBinApp :: Parser E -> Parser (E -> E -> E)
pBinApp pBinOp = pSpanBinary $ flip binApp <$> pBinOp

noAssoc :: Parser E -> [BinOp] -> Parser E
noAssoc p fs = p >>= \x -> (spannit $ binApp x <$> pOpOf fs <*> p) <|> pure x

leftAssoc :: Parser E -> [BinOp] -> Parser E
leftAssoc p fs = p `chainl1` pBinApp (pOpOf fs)

{-
  b  := l :t0?
  e0 := \(b,* b) -> e0
      | \case { alt;* alt }
      | e1
-}
pExpr0 :: Parser E
pExpr0 =
      spannit (symbolic '\\' *> (
            EAbsF <$> parens (commaSep1 pPattern0)
                  <*  symbol "->"
                  <*> pure []
                  <*> pExpr0
        <|> ELamCaseF <$  reserved "case"
                      <*> caseBody
      ))
  <|> pExpr1

{-
  alt := p -> e0
  e1  := case e2 of { alt;* alt }
-}
pExpr1 :: Parser E
pExpr1 =
      spannit (ECaseF <$  reserved "case"
              <*> pExpr2
              <*  reserved "of"
              <*> caseBody
      )
  <|> pExpr2

{-
  e2 := if e0 then e0 else e0
      | if | e0 then e0
           | e0 then e0
           |    else e0
-}
pExpr2 :: Parser E
pExpr2 =
      spannit (reserved "if" >> (
            EIfF <$> pExpr0 <* reserved "then"
                 <*> pExpr0 <* reserved "else"
                 <*> pExpr0
        <|> EMultiIfF <$> some (try $ Guarded <$ bar <*> pExpr0 <* reserved "then" <*> pExpr0)
                      <*> optional (bar *> reserved "else" *> pExpr0)
      ))
  <|> pExpr3

{-
  let := p = e0
  e3  := { let;+ e0 }

  Patterns on the left side of let expressions allow us to do interesting things
  Consider the syntactically valid {1=x;x}, would it be useful to desugar to
  case x of {1 -> x; _ -> impossible}
-}
pExpr3 :: Parser E
pExpr3 =  let letbind = (,) <$> pPattern0
                                <*  symbolic '='
                                <*> pExpr0
                                <*  semi
          in try (spannit $ braces $ ELetF <$> some (try letbind) <*> pExpr0)
      <|> pExpr4

--EIff.       Expr4 ::= Expr5 "<->" Expr5 ;
pExpr4 :: Parser E
pExpr4 =  noAssoc pExpr5 [OpIff]

--EImp.       Expr5 ::= Expr6 "-->" Expr6 ;
pExpr5 :: Parser E
pExpr5 =  noAssoc pExpr6 [OpImp]

--ELogOr.     Expr6 ::= Expr7 "||" Expr6 ;
pExpr6 :: Parser E
pExpr6 =  leftAssoc pExpr7 [OpOr]

--ELogAnd.    Expr7 ::= Expr8 "&&" Expr7 ;
pExpr7 :: Parser E
pExpr7 =  leftAssoc pExpr8 [OpAnd]

--EEq.        Expr8 ::= Expr9 "==" Expr9 ;
--ENotEq.     Expr8 ::= Expr9 "!=" Expr9 ;
--EGtEq.      Expr8 ::= Expr9 ">=" Expr9 ;
--ELtEq.      Expr8 ::= Expr9 "<=" Expr9 ;
--EGt.        Expr8 ::= Expr9 ">"  Expr9 ;
--ELt.        Expr8 ::= Expr9 "<"  Expr9 ;
pExpr8 :: Parser E
pExpr8 = noAssoc pExpr9 [OpEq, OpNe, OpGt, OpGe, OpLt, OpLe]

--EPlus.      Expr9 ::= Expr9 "+" Expr10 ;
--EMinus.     Expr9 ::= Expr9 "-" Expr10 ;
pExpr9 :: Parser E
pExpr9 = leftAssoc pExpr10 [OpAdd, OpSub]

--ETimes.     Expr10 ::= Expr10 "*" Expr11 ;
--EDiv.       Expr10 ::= Expr10 "/" Expr11 ;
--EMod.       Expr10 ::= Expr10 "%" Expr11 ;
pExpr10 :: Parser E
pExpr10 = leftAssoc pExpr11 [OpTimes, OpDiv, OpMod]

pExpr11 :: Parser E
pExpr11 = pExpr12 -- TODO: `chainl1` (pSpanBinary $ ERecExtF <$ symbol "++")

pExpr12 :: Parser E
pExpr12 = pExpr13 >>= \e ->
      spanLeft ERecResF e <$> some (spanned $ symbol "--" *> label)
  <|> pure e

pExpr13 :: Parser E
pExpr13 =  (spannit $ EAppF <$> pOp OpNot <*> ((:[]) <$> pExpr14))
       <|> pExpr14

pExpr14 :: Parser E
pExpr14 = pExpr99 >>= \e ->
      spanLeft ERecSelF e <$> some (spanned $ dot *> label)
  <|> pure e

{-
  e99 := anyint
       | charLiteral
       | lident
       | lident(e0,* e0)
       | { l=e0,* l=e0 |e0? }
       | (e0,+ e0)
       | (e0)
-}
pVar :: Parser E
pVar = spannit $ EVarF <$> lident

pExpr99 :: Parser E
pExpr99 =
  let      labelledExpr :: Parser (T.Text, E)
           labelledExpr = (,) <$> lident <* symbolic '=' <*> pExpr0
  in       spannit (EConstF <$> pConstant)
           -- An lident followed by comma separated expressions or nothing
       <|> (pVar >>= \v -> spannit (EAppF v <$> (parens $ commaSep1 pExpr0)) <|> pure v)
       <|> spannit (braces (commaSep labelledExpr >>= \ls ->
                       ERecExtF ls <$ bar <*> pExpr0
                   <|> pure (ERecF ls)
           ))
       <|> spannit (ETupF  <$> try (parens $ commaSep2 pExpr0))
       <|> spannit (EArrF  <$> try (brackets $ commaSep pExpr0))
       <|> parens pExpr0

{-
  con := l (t0,* t0)?
-}
pDataCon :: Parser (T.Text, [T])
pDataCon = (,)
        <$> lident
        <*> optionalList (parens $ commaSep1 pType0)

{-
  data := "data" u (l,* l)? { con;* con }
-}
pData :: Parser (DataDecl T)
pData = reserved "data"
      $> DataDecl
     <*> uident
     <*> optionalList (parens $ commaSep1 lident)
     <*> (M.fromList <$> braces (semiSep1 pDataCon))

data BitCons = BitConIdent T.Text
             | BitConInt JInt
             | BitConWild
             deriving (Eq, Show)

data BitCase p = BitCase p [(BitCons, Maybe SizeSpec)]
             deriving (Eq, Show)

data BitRep p = BitRep T.Text [T.Text] (Maybe SizeSpec) [BitCase p]
            deriving (Eq, Show)

{-
  (l | anyint | _) <sizespec>?
-}
pBitCons :: Parser (BitCons, Maybe SizeSpec)
pBitCons =  (,)
        <$> (BitConWild <$ wildcard <|> BitConIdent <$> lident <|> BitConInt <$> anyint)
        <*> optional (angles pSizeSpec)
{-
  bitcase := p = bitcon#* bitcon
-}
pBitCase :: Parser (BitCase P)
pBitCase =  BitCase
        <$> pPattern0
        <*  symbolic '='
        <*> pBitCons `sepBy1` symbolic '#'

{-
  bitrep := "bitrep" u (l,* l)? <sizespec> { bitcase;* bitcase }
-}
pBitRep :: Parser (BitRep P)
pBitRep = reserved "bitrep"
        $> BitRep
       <*> uident
       <*> optionalList (parens $ commaSep1 lident)
       <*> optional (angles pSizeSpec)
       <*> braces (semiSep1 pBitCase)
