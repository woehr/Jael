{-|
Functions for parsing and transforming the BNFC AST to Jael data structures.
No error checking.
-}

{-# Language FlexibleInstances #-}
{-# Language FunctionalDependencies #-}
{-# Language MultiParamTypeClasses #-}
{-# Language NoImplicitPrelude #-}
{-# Language RecordWildCards #-}

module Jael.Parser where

import           BasePrelude

import           Control.Comonad.Cofree
import qualified Control.Comonad.Trans.Cofree as C
import           Data.Functor.Foldable
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Language.Fixpoint.Types as L

import qualified Jael.Grammar as G
import           Jael.Expr
import           Jael.Prog
import           Jael.Type
import           Jael.Util

type ParseFun a = [G.Token] -> G.Err a

runParser :: ParseFun a -> T.Text -> Either T.Text a
runParser p t = case p . G.myLexer . T.unpack $ t of
                     G.Bad err  -> Left (T.pack err)
                     G.Ok  tree -> Right tree

  -- Transform the grammar AST into sturctures more easily handled by the
  -- compiler. No error checking is done.
class Jaelify a b | a -> b where
  jaelify :: a -> b

instance Jaelify G.UIdent Ident where
  jaelify (G.UIdent (x, n)) = Token (T.pack n) x

instance Jaelify G.LIdent Ident where
  jaelify (G.LIdent (x, n)) = Token (T.pack n) x

instance Jaelify G.LScopedIdent Ident where
  jaelify (G.LScopedIdent (x, n)) = Token (T.pack n) x

instance Jaelify G.DecInt IntConst where
  jaelify (G.DecInt (x, s)) = Token (parseDecInt s) x

instance Jaelify G.HexInt IntConst where
  jaelify (G.HexInt (x, s)) = Token (parseHexInt s) x

instance Jaelify G.OctInt IntConst where
  jaelify (G.OctInt (x, s)) = Token (parseOctInt s) x

instance Jaelify G.BinInt IntConst where
  jaelify (G.BinInt (x, s)) = Token (parseBinInt s) x

instance Jaelify G.AnyInt IntConst where
  jaelify (G.AnyIntDecInt x) = jaelify x
  jaelify (G.AnyIntHexInt x) = jaelify x
  jaelify (G.AnyIntOctInt x) = jaelify x
  jaelify (G.AnyIntBinInt x) = jaelify x

parseDecInt :: String -> Integer
parseDecInt s@(x:xs) =
  if x == '~'
     then -(intDoParse readDec xs)
     else   intDoParse readDec s
parseDecInt _ = error "impossible"

parseHexInt :: String -> Integer
parseHexInt ('0':'x':xs) = intDoParse readHex xs
parseHexInt _ = error "impossible"

parseOctInt :: String -> Integer
parseOctInt ('0':'o':xs) = intDoParse readOct xs
parseOctInt _ = error "impossible"

parseBinInt :: String -> Integer
parseBinInt ('0':'b':xs) = intDoParse readBin xs
parseBinInt _ = error "impossible"

intDoParse :: ReadS Integer -> String -> Integer
intDoParse p xs =
  case p xs of
       [(i, [])] -> i
       -- The lexer should never give strings that can't be parsed as Ints
       _         -> error "impossible"

readBin :: ReadS Integer
readBin = readInt 2 (\x -> x == '0' || x == '1') digitToInt

instance Jaelify G.Expr MaybeTypedExpr where
  jaelify (G.EInt i) = MTECon (CInt $ jaelify i) []
  jaelify G.ETrue    = MTECon (CBool True) []
  jaelify G.EFalse   = MTECon (CBool False) []
  jaelify G.EUnit    = MTECon CUnit []
  jaelify (G.EVar i) = MTEVar (jaelify i) []
  jaelify (G.ETup e1 es) = parseETup $ map parseCommaSepExpr (e1:es)

  jaelify (G.EApp n es) =
    foldl' (\acc e -> [] :< EAppF acc e)
           (MTEVar (jaelify n) [])
           (map parseCommaSepExpr es)

  jaelify (G.EAppScoped n es) =
    foldl' (\acc e -> [] :< EAppF acc e)
           (MTEVar (jaelify n) [])
           (map parseCommaSepExpr es)

  jaelify (G.EAbs ns ret e) =
    let (xs, _) = unzip args
        ret' = unMaybeType ret
        (et :< ef) = jaelify e
        e' = (ret' ++ et) :< ef
        absExpr = foldr (\x y -> [] :< EAbsF x y) e' xs
        -- Push the type annotation from the lambda into variables
    in  cata alg absExpr
    where
      args = map unLambdaArg ns
      argMap = M.fromList $ map (first value) args

      unLambdaArg (G.LambdaArg1 n mt) = (jaelify n, unMaybeType mt)

      unMaybeType [G.MaybeType1 t] = [jaelify t]
      unMaybeType []               = []
      unMaybeType _ = error "Grammar should only allow lists of 0 or 1 length"

      alg :: C.CofreeF ExprF [QType] MaybeTypedExpr -> MaybeTypedExpr
      alg (ts C.:< v@(EVarF (Token n _))) = case M.lookup n argMap of
        Just t -> (t++ts) :< v
        Nothing -> ts :< v
      alg x = embed x

  jaelify (G.EAnn e t) =
    let (ts :< e') = jaelify e
    in  (jaelify t:ts) :< e'

  jaelify (G.EIf b t e) = ([] :<) $ EIteF (jaelify b) (jaelify t) (jaelify e)
  jaelify (G.ELogOr  l r) = mkApp COr  [l, r]
  jaelify (G.ELogAnd l r) = mkApp CAnd [l, r]
  jaelify (G.EEq     l r) = mkApp CEq  [l, r]
  jaelify (G.ENotEq  l r) = mkApp CNe  [l, r]
  jaelify (G.EGtEq   l r) = mkApp CGe  [l, r]
  jaelify (G.ELtEq   l r) = mkApp CLe  [l, r]
  jaelify (G.EGt     l r) = mkApp CGt  [l, r]
  jaelify (G.ELt     l r) = mkApp CLt  [l, r]
  jaelify (G.EPlus   l r) = mkApp CAdd [l, r]
  jaelify (G.EMinus  l r) = mkApp CSub [l, r]
  jaelify (G.ETimes  l r) = mkApp CMul [l, r]
  jaelify (G.EDiv    l r) = mkApp CDiv [l, r]
  jaelify (G.EMod    l r) = mkApp CMod [l, r]
  jaelify (G.EBitCat l r) = mkApp CBitCat [l, r]
  jaelify (G.ELogNot e  ) = mkApp CNot [e]

instance Jaelify G.LetExpr MaybeTypedExpr where
  jaelify (G.LetExpr1 ls e) =
    foldr ((\(n, e1) e2 -> ([] :< ELetF n e1 e2)) . letExpr)
          (jaelify e)
          ls
    where letExpr :: G.LetElem -> (Ident, MaybeTypedExpr)
          letExpr (G.LetElem1 n e') = (jaelify n, jaelify e')

instance Jaelify G.Func FuncExpr where
  jaelify (G.Func1 n as ret e) =
    (jaelify n, map argTup as, jaelify ret, jaelify e)
    where argTup (G.FuncArg1 a t) = (jaelify a, jaelify t)

instance Jaelify G.Global GlobExpr where
  jaelify (G.Global1 n e) = (jaelify n, jaelify e)

instance Jaelify G.Prog (Program [Type] [GlobExpr] [FuncExpr]) where
  jaelify (G.Prog1 defs) =
    let (types, funcs, globs) = foldr splitDef ([], [], []) defs
    in  Program types funcs globs
    where
      splitDef :: G.TopDef
               -> ([Type], [GlobExpr], [FuncExpr])
               -> ([Type], [GlobExpr], [FuncExpr])
      splitDef (G.TopDefGlobal g) (v, w, x) = (v, jaelify g : w, x)
      splitDef (G.TopDefFunc   f) (v, w, x) = (v, w, jaelify f : x)
      splitDef (G.TopDefTypeDef _) _ = undefined
      splitDef (G.TopDefProcDef _) _ = undefined

instance Jaelify G.Type QType where
  jaelify (G.TypeQType t) = jaelify t
  jaelify (G.TypeUnqualType t) = [] :< jaelify t

instance Jaelify G.QType QType where
  jaelify (G.QTypeVar n t p) =
    [L.reft (L.symbol . value . jaelify $ n) $ parseQPred p] :< jaelify t
  jaelify (G.QTypeNoVar t p) =
    [L.reft (L.vv Nothing) (parseQPred p)] :< jaelify t

instance Jaelify G.UnqualType (TypeF QType) where
  jaelify (G.UnqualTypeTypeA (G.TypeNamed n))  =
    TNamedF (jaelify n) []
  jaelify (G.UnqualTypeTypeB (G.TypeNamedParams n ts)) =
    TNamedF (jaelify n) $ map unCommaSeparate ts

  jaelify (G.UnqualTypeTypeB (G.TypeBase G.TUnit)) = TBuiltinF BTUnit
  jaelify (G.UnqualTypeTypeB (G.TypeBase G.TBool)) = TBuiltinF BTBool
  jaelify (G.UnqualTypeTypeB (G.TypeBase G.TInt))  = TBuiltinF BTInt
  jaelify (G.UnqualTypeTypeB (G.TypeBase G.TBit))  = TBuiltinF BTBits
  jaelify (G.UnqualTypeTypeB (G.TypeBase (G.TBuffer t))) =
    TBuiltinF $ BTBuffer (jaelify t)

  jaelify (G.UnqualTypeTypeB (G.TypeVar  n))   =
    TVarF (jaelify n)
  jaelify (G.UnqualTypeTypeB (G.TypeTup t ts)) =
    TTupF $ map unCommaSeparate (t:ts)

unCommaSeparate :: G.CommaSepType -> QType
unCommaSeparate (G.CommaSepTypeType t) = jaelify t

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
parseQExpr (G.QExprMul c e)  = L.EBin L.Times (L.expr . value $ jaelify c) (parseQExpr e)
parseQExpr (G.QExprInt i)    = L.expr . value $ jaelify i
parseQExpr (G.QExprVar n)    = L.eVar . value $ jaelify n
parseQExpr (G.QExprApp n (a:as)) =
  foldl'
    L.EApp
    (L.EApp (L.EVar . L.symbol . value . jaelify $ n) (parseCommaSepQExpr a))
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

parseCommaSepExpr :: G.CommaSepExpr -> MaybeTypedExpr
parseCommaSepExpr (G.CommaSepExprExpr e) = jaelify e

{-@ es :: [MaybeTypedExpr] | len es >= 2 @-}
parseETup :: [MaybeTypedExpr] -> MaybeTypedExpr
parseETup es = [] :< ETupF es

mkApp :: Constant -> [G.Expr] -> MaybeTypedExpr
mkApp f as = foldl' (\acc e -> [] :< EAppF acc e)
                    (MTECon f [])
                    (map jaelify as)

gListToMaybe :: Jaelify a b => [a] -> Maybe b
gListToMaybe [] = Nothing
gListToMaybe (x:[]) = Just $ jaelify x
gListToMaybe _ = error "Expected grammar to only allow lists of 0 or 1 elements"
