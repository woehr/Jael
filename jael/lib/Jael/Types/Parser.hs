{-|
Functions for parsing and transforming the BNFC AST to Jael data structures.
No error checking.
-}

{-# Language FlexibleInstances #-}
{-# Language FunctionalDependencies #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}

module Jael.Types.Parser where

import qualified Control.Comonad.Trans.Cofree as C
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Language.Fixpoint.Types as F

import qualified Jael.Grammar as G
import           Jael.Types.Expr
import           Jael.Types.Prog
import           Jael.Types.Type
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

-- instance Jaelify G.LScopedIdent Ident where
--   jaelify (G.LScopedIdent (x, n)) = Token (T.pack n) x

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

  -- jaelify (G.EAppScoped n es) =
  --   foldl' (\acc e -> [] :< EAppF acc e)
  --          (MTEVar (jaelify n) [])
  --          (map parseCommaSepExpr es)

  jaelify (G.ELet ls e) =
    foldr ((\(n, e1) e2 -> ([] :< ELetF n e1 e2)) . letExpr)
          (jaelify e)
          ls
    where letExpr :: G.LetElem -> (Ident, MaybeTypedExpr)
          letExpr (G.LetElem1 n e') = (jaelify n, jaelify e')

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
  jaelify (G.EIff    l r) = mkApp CIff [l, r]
  jaelify (G.EImp    l r) = mkApp CImp [l, r]
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

newtype GrammarPred = GrammarPred G.Expr

instance F.Expression GrammarPred where
  expr (GrammarPred g) = toFExpr g

type SubMap = HM.HashMap F.Symbol F.Expr

toFExpr :: G.Expr -> F.Expr
toFExpr (G.EInt i  )    = F.ECon . F.I . value . jaelify $ i
toFExpr G.ETrue         = F.PTrue
toFExpr G.EFalse        = F.PFalse
toFExpr G.EUnit         = F.ECon (F.L "void" $ F.FTC . F.symbolFTycon . F.dummyLoc . F.symbol $ ("Void"::T.Text))
toFExpr (G.EVar i)      = toFEVar i
--toFExpr (G.ETup e1 es)  = undefined

toFExpr (G.EApp n es)   =
  F.eApps (toFEVar n) (flip map es $ \(G.CommaSepExprExpr e) -> toFExpr e)

toFExpr (G.ELet ls e)   =
  flip F.subst (toFExpr e) $ F.Su $ foldl' mkSubs HM.empty ls

  where mkSubs :: SubMap -> G.LetElem -> SubMap
        mkSubs acc (G.LetElem1 (G.LIdent (_, s)) e') =
          flip (HM.insert $ F.symbol s) acc $ F.subst (F.Su acc) (toFExpr e')

toFExpr (G.EIf b t e)   = F.EIte (toFExpr b) (toFExpr t) (toFExpr e)
toFExpr (G.EIff    l r) = F.PIff (toFExpr l) (toFExpr r)
toFExpr (G.EImp    l r) = F.PImp (toFExpr l) (toFExpr r)
toFExpr (G.ELogOr  l r) = F.POr  [toFExpr l,  toFExpr r]
toFExpr (G.ELogAnd l r) = F.PAnd [toFExpr l,  toFExpr r]

toFExpr (G.EEq     l r) = F.PAtom F.Eq (toFExpr l) (toFExpr r)
toFExpr (G.ENotEq  l r) = F.PAtom F.Ne (toFExpr l) (toFExpr r)
toFExpr (G.EGtEq   l r) = F.PAtom F.Ge (toFExpr l) (toFExpr r)
toFExpr (G.ELtEq   l r) = F.PAtom F.Le (toFExpr l) (toFExpr r)
toFExpr (G.EGt     l r) = F.PAtom F.Gt (toFExpr l) (toFExpr r)
toFExpr (G.ELt     l r) = F.PAtom F.Lt (toFExpr l) (toFExpr r)

toFExpr (G.EPlus   l r) = F.EBin F.Plus  (toFExpr l) (toFExpr r)
toFExpr (G.EMinus  l r) = F.EBin F.Minus (toFExpr l) (toFExpr r)
toFExpr (G.ETimes  l r) = F.EBin F.Times (toFExpr l) (toFExpr r)
toFExpr (G.EDiv    l r) = F.EBin F.Div   (toFExpr l) (toFExpr r)
toFExpr (G.EMod    l r) = F.EBin F.Mod   (toFExpr l) (toFExpr r)
toFExpr (G.ELogNot e  ) = F.PNot (toFExpr e)
--toFExpr (G.EBitCat l r) = undefined

toFExpr _ = error "Unsupported expression in predicate"

toFEVar :: G.LIdent -> F.Expr
toFEVar = F.EVar . F.symbol . value . jaelify

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

instance Jaelify G.BaseType QType where
  jaelify (G.BaseTypeQType t) = jaelify t
  jaelify (G.BaseTypeUnqualType t) = Nothing :< jaelify t

instance Jaelify G.QType QType where
  jaelify (G.QTypeUnqual n t p) =
    Just (F.reft (F.symbol . value . jaelify $ n)
                 (F.expr $ GrammarPred p)
         ) :< jaelify t

  jaelify (G.QTypeNamed n t p) =
    Just (F.reft (F.symbol . value . jaelify $ n)
                 (F.expr $ GrammarPred p)
         ) :< TConF (jaelify t) []

instance Jaelify G.UnqualType (TypeF QType) where
  jaelify (G.TypeNamedParams n ts) =
    TConF (jaelify n) $ map unCommaSeparate ts
  jaelify (G.TypeBuiltin G.TUnit) = TUnitF
  jaelify (G.TypeBuiltin G.TBool) = TBoolF
  jaelify (G.TypeBuiltin G.TInt)  = TIntF
  jaelify (G.TypeBuiltin G.TBit)  = TBitsF
  jaelify (G.TypeBuiltin (G.TBuffer t)) = TBufferF (jaelify t)
  jaelify (G.TypeVar  n)          = TVarF (jaelify n)
  jaelify (G.TypeTup t ts)        = TTupF $ map unCommaSeparate (t:ts)

instance Jaelify G.TypeNoUIdent QType where
  jaelify (G.TypeFun i t1 t2) =
    Nothing :< TFunF (jaelify i) (jaelify t1) (jaelify t2)
  jaelify (G.TypeBase t) = jaelify t

instance Jaelify G.Type QType where
  jaelify (G.TypeTypeNoUIdent t) = jaelify t
  jaelify (G.TypeUIdent i) = Nothing :< TConF (jaelify i) []


unCommaSeparate :: G.CommaSepType -> QType
unCommaSeparate (G.CommaSepTypeTypeNoUIdent t) = jaelify t

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
gListToMaybe []  = Nothing
gListToMaybe [x] = Just $ jaelify x
gListToMaybe _   = error "Expected grammar to only allow lists of 0 or 1 elements"
