{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Jael.AST where

--import           Debug.Trace
import           Control.Lens.Cons

import qualified Control.Comonad.Trans.Cofree  as C
import qualified Data.Text                     as T
import qualified Jael.Grammar                  as G

import           Jael.DataDecl
import           Jael.Expr
import           Jael.Pattern
import           Jael.Prelude
import           Jael.QType
import           Jael.Type

type ParsePattern = Pattern T.Text T.Text Literal
type ParseType = QType Type'F ParseExpr
type ParseExpr = Expr' ParsePattern

--newtype FreshVarS = FreshVarS { _freshCount :: Integer }
--                  deriving (Eq, Show)
--makeClassy ''FreshVarS

--initState :: FreshVarS
--initState = FreshVarS 0

--evalExpr :: G.Expr -> ParseExpr
--evalExpr = flip evalState initState . toExpr

--freshVar :: (HasFreshVarS s, MonadState s m) => m Integer
--freshVar = freshCount <+= 1

--type ParseExprM = StateT FreshVarS Identity

appArgs :: (ToExprList e) => ParseExpr -> e -> ParseExpr
appArgs o gs = EApp o (toExprList gs)

number :: Integer -> String -> Integer
number base xs =
  fromMaybe
      (error $ "Grammar error: base " <> show base <> " for string " <> show xs)
    $ foldlM (\x d -> (base * x +) <$> digitToInt d) 0 xs

class ToJInt a where
  toJInt :: a -> JInt

instance ToJInt G.AnyInt where
  toJInt (G.AnyIntBin x) = toJInt x
  toJInt (G.AnyIntOct x) = toJInt x
  toJInt (G.AnyIntHex x) = toJInt x
  toJInt (G.AnyIntDec x) = toJInt x

instance ToJInt G.BinInt where
  toJInt (G.BinInt s) = let i = drop 2 s in JInt BinInt (number 2  i) (length i)

instance ToJInt G.OctInt where
  toJInt (G.OctInt s) = let i = drop 2 s in JInt OctInt (number 8  i) (length i)

instance ToJInt G.HexInt where
  toJInt (G.HexInt s) = let i = drop 2 s in JInt HexInt (number 16 i) (length i)

instance ToJInt G.DecInt where
  toJInt (G.DecInt s) = JInt DecInt (number 10 s) (length s)

class ToText g where
  toText :: g -> T.Text

class ToPattern g where
  toPattern :: g -> ParsePattern

class ToExpr g where
  toExpr :: g -> ParseExpr

class ToType g where
  toType :: g -> ParseType

class (ToList g, ToText (Element g)) => ToTextList g where
  toTextList :: g -> [T.Text]
  toTextList = fmap toText . otoList

class (ToList g, ToPattern (Element g)) => ToPatternList g where
  toPatternList :: g -> [ParsePattern]
  toPatternList = fmap toPattern . otoList

class (ToList g, ToExpr (Element g)) => ToExprList g where
  toExprList :: g -> [ParseExpr]
  toExprList = fmap toExpr . otoList

class (ToList g, ToType (Element g)) => ToTypeList g where
  toTypeList :: g -> [ParseType]
  toTypeList = fmap toType . otoList

instance ToText T.Text where
  toText = id

instance ToText String where
  toText = T.pack

instance ToPattern ParsePattern where
  toPattern = id

instance ToExpr ParseExpr where
  toExpr = id

instance ToType ParseType where
  toType = id

instance (ToText g) => ToTextList [g]
instance (ToPattern g) => ToPatternList [g]
instance (ToExpr g) => ToExprList [g]
instance (ToType g) => ToTypeList [g]

instance ToTextList G.LSpc1LIdent
instance ToTextList G.LCom1LIdent
instance ToPatternList G.LCom1Pattern
instance ToPatternList G.LCom2Pattern
instance ToPatternList G.LBar2Pattern
instance ToExprList G.LCom1Expr
instance ToExprList G.LCom2Expr
instance ToTypeList G.LCom1RefType
instance ToTypeList G.LCom2RefType
instance ToTypeList G.LCom1AnyType

instance ToText G.LIdent where
  toText (G.LIdent s) = T.pack s

instance ToText G.UIdent where
  toText (G.UIdent s) = T.pack s

instance ToText G.ReftIdent where
  toText (G.ReftIdent s) = T.pack s

instance ToText G.NegChan where
  toText (G.NegChan s) = T.pack s

instance ToText G.Label where
  toText (G.LabelLIdent s) = toText s
  toText (G.LabelDecInt i) = T.pack . show . intValue . toJInt $ i

recPatToParse :: G.LCom1RecPat -> [(T.Text, ParsePattern)]
recPatToParse = fmap (bimap toText toPattern) . otoList

instance ToPattern G.Pattern where
  toPattern (G.PatConArg l ps)  = PCon (toText l) (toPatternList ps)
  toPattern (G.PatPar ps)       = POr (toPatternList ps)
  toPattern (G.PatVar l)        = PVar (toText l)
  toPattern (G.PatVarAt l p)    = PAt (toText l) (toPattern p)
  toPattern (G.PatInt i)        = PLit (LInt $ toJInt i)
  toPattern (G.PatNegInt i)     = PLit (LInt $ negateJInt (toJInt i))
  toPattern (G.PatCon l)        = PCon (toText l) []
  toPattern (G.PatRec ps)       = PRec (recPatToParse ps) (PRecEmpty :: RecTailPat T.Text)
  toPattern (G.PatRecPoly ps l) = PRec (recPatToParse ps) (PVar (toText l) :: RecTailPat T.Text)
  toPattern (G.PatRecWild ps)   = PRec (recPatToParse ps) (PWild :: RecTailPat T.Text)
  toPattern (G.PatTup ps)       = PTup (toPatternList ps)
  toPattern (G.PatArr ps)       = PArr (toPatternList ps)
  toPattern G.PatRecEmp         = PRecEmpty
  toPattern G.PatWild           = PWild

casesToTups :: G.LSemi1Case -> [(ParsePattern, ParseExpr)]
casesToTups cs = fmap (\(x, y) -> (toPattern x, toExpr y)) (otoList cs)

instance ToExpr G.Expr where
  toExpr (G.EAbs ps e)     = EAbs (toPatternList ps) (toExpr e)
  toExpr (G.EAbsCase cs)   = ELamCase (casesToTups cs)
  toExpr (G.ECase e cs)    = ECase (toExpr e) (casesToTups cs)
  toExpr (G.ELet bs e)     = ELet (fmap (bimap toPattern toExpr) (otoList bs)) (toExpr e)
  toExpr (G.EIf b t e)     = EIf (toExpr b) (toExpr t) (toExpr e)
  toExpr (G.EMultiIf be's) = EMultiIf (fmap (\(b,e) -> toExpr b :# toExpr e) (otoList be's)) Nothing
  toExpr (G.EIff l r)      = appArgs (EOp OpIff) [l, r]
  toExpr (G.EImp l r)      = appArgs (EOp OpImp) [l, r]
  toExpr (G.ELogOr l r)    = appArgs (EOp OpOr)  [l, r]
  toExpr (G.ELogAnd l r)   = appArgs (EOp OpAnd) [l, r]
  toExpr (G.EEq l r)       = appArgs (EOp OpEq)  [l, r]
  toExpr (G.ENotEq l r)    = appArgs (EOp OpNe)  [l, r]
  toExpr (G.EGtEq l r)     = appArgs (EOp OpGe)  [l, r]
  toExpr (G.ELtEq l r)     = appArgs (EOp OpLe)  [l, r]
  toExpr (G.EGt l r)       = appArgs (EOp OpGt)  [l, r]
  toExpr (G.ELt l r)       = appArgs (EOp OpLt)  [l, r]
  toExpr (G.EPlus l r)     = appArgs (EOp OpAdd) [l, r]
  toExpr (G.EMinus l r)    = appArgs (EOp OpSub) [l, r]
  toExpr (G.ETimes l r)    = appArgs (EOp OpTimes) [l, r]
  toExpr (G.EDiv l r)      = appArgs (EOp OpDiv) [l, r]
  toExpr (G.EMod l r)      = appArgs (EOp OpMod) [l, r]
  toExpr (G.ELogNot e)     = appArgs (EOp OpNot) [e]
  toExpr (G.ENegate e)     = appArgs (EOp OpSub) [ELit . LInt . defaultInt $ 0, toExpr e]
  toExpr (G.ERecSel e l)   = ERecSelect (toExpr e) (toText l)
  toExpr (G.ERecRem e l)   = ERecRemove (toExpr e) (toText l)
  toExpr (G.EInt i)        = ELit (LInt $ toJInt i)
  toExpr (G.EVar l)        = EVar (toText l)
  toExpr (G.EReftVar v)    = EVar (toText v)
  toExpr (G.EApp e leMl1)  = EApp (toExpr e) (toExprList leMl1)
  toExpr (G.ETup es)       = ETup $ toExprList es
  toExpr (G.EArr es)       = EArr $ toExprList es
  toExpr G.ERecEmpt        = ERecEmpty
  toExpr (G.ERec re's)     = foldr grecToRec ERecEmpty (otoList re's)
  toExpr (G.ERecPoly re's e) = ERec $ foldr grecToRec (toExpr e) (otoList re's)

grecToRec :: RecordOp -> ParseExpr -> ParseExpr
grecToRec (RecExtend l e ) x = ERecExtend (toText l) (toExpr e) x
grecToRec (RecRename l l') x = ERecRename (toText l) (toText l') x
grecToRec (RecUpdate l e ) x = ERecUpdate (toText l) (toExpr e) x

instance ToType G.BaseType where
  toType (G.BTypeFun rt1 rt2) =
    UnqualType $ TFunF @T.Text Nothing (toType rt1) (toType rt2)
  toType (G.BTypeFunDep l bt rt) =
    UnqualType $ TFunF (Just $ toText l) (toType bt) (toType rt)
  toType (G.BTypeVar l) =
    UnqualType $ TVarF (toText l)
  toType G.BTypeRecEmp =
    UnqualType $ TRecF (monoRow @T.Text @T.Text [])
  toType (G.BTypeRecPoly l) =
    UnqualType $ TRecF (polyRow @_ @T.Text [] (toText l))
  toType (G.BTypeRec rs) =
    UnqualType $ TRecF $ monoRow @T.Text (bimap toText toType <$> otoList rs)
  toType (G.BTypeRecExt rs l) =
    UnqualType $ TRecF $ polyRow (bimap toText toType <$> otoList rs) (toText l)
  toType (G.BTypeTup rts) =
    UnqualType $ TTupF (toTypeList rts)
  toType (G.BTypeArr rt i) =
    UnqualType $ TArrF (toType rt) (intValue (toJInt i))
  toType (G.BTypeAlias (G.AliasNoParam u)) =
    UnqualType $ TConF (toText u) []
  toType (G.BTypeAlias (G.AliasParam u ts G.MaybeLCom1ExprNothing)) =
    UnqualType $ TConF (toText u) (toTypeList ts)
  toType (G.BTypeAlias (G.AliasParam _u _ts (G.MaybeLCom1ExprJust _es))) =
    error "TODO: Type aliases with expression parameters"

qualifyType :: Refinement ParseExpr -> ParseType -> ParseType
qualifyType r = \case
  (Fix (Nothing C.:< t)) -> Fix (Just r C.:< t)
  _ -> error "Grammar error. Should always be an unqualified type."

instance ToType G.RefinedType where
  toType (G.RTypeBase0 bt)        = toType bt
  toType (G.RTypeQualFun l bt e)  = qualifyType (toText l, toExpr e) (toType bt)
  toType (G.RTypeQualBase l bt e) = qualifyType (toText l, toExpr e) (toType bt)
  toType (G.RTypeBase1 bt)        = toType bt
  toType (G.RTypeBase2 bt)        = toType bt

instance ToType G.TScheme where
  toType (G.TPoly ls rt) = foldr (\a b -> UnqualType (TAllF a b)) (toType rt) (toTextList ls)
  toType (G.TMono    rt) = toType rt

instance ToType G.AnyType where
  toType (G.AnyTypeRType rt)                   = toType rt
  toType (G.AnyTypeAlias (G.AliasNoParam u))   = UnqualType $ TConF (toText u) []
  toType (G.AnyTypeAlias (G.AliasParam u rts _es)) = UnqualType $ TConF (toText u) (toTypeList rts)
  toType (G.AnyTypeSess _s)                    = undefined

toData :: G.UIdent -> [T.Text] -> G.LBar1DataCon -> DataDecl ParseType
toData u ls cs = DataDecl
  { dataName  = toText u
  , dataTVars = toTextList ls
  , dataCons  = fmap (uncurry toDataCon) (zip [0 ..] (otoList cs))
  }

toData' :: G.UIdent -> G.LCom1LIdent -> G.LBar1DataCon -> DataDecl ParseType
toData' u ls cs = toData u (toTextList ls) cs

toDataCon :: Integer -> G.DataCon -> (T.Text, ConsInfo ParseType)
toDataCon tag (G.DataConEmpt l) = (toText l, mkConsInfo l [] tag)
toDataCon tag (G.DataConArgs l ts) =
  (toText l, mkConsInfo l (toTypeList ts) tag)

mkConsInfo :: G.LIdent -> [ParseType] -> Integer -> ConsInfo ParseType
mkConsInfo a b c = ConsInfo {ddciName = toText a, ddciParams = b, ddciTag = c}

data AST var pat typ exptyp
  = AST
  --{ _astTypeAlias      :: M.Map T.Text a
  { _astTypeConstraint :: [(var, typ)]
  , _astDataDef        :: [(T.Text, DataDecl typ)]
  , _astExpr           :: [(pat, Expr pat var)]
  --, _astSessAlias      :: M.Map T.Text d
  --, _astProc           :: M.Map T.Text e
  } deriving (Eq, Generic, Show)

makeLenses ''AST

emptyAST :: AST a b c ()
emptyAST = AST [] [] []

parseTo' :: _
parseTo' = either error id . parseTo

parseToAST :: String -> AST T.Text ParsePattern ParseType ()
parseToAST = parseTo' G.pLTop grammarToAST

parseToPattern :: String -> ParsePattern
parseToPattern = parseTo' G.pPattern toPattern

parseToQType :: String -> ParseType
parseToQType = parseTo' G.pRefinedType toType

grammarToAST :: G.LTop -> AST T.Text ParsePattern ParseType ()
grammarToAST = foldr splitTop emptyAST . otoList

splitTop :: G.Top
         -> AST T.Text ParsePattern ParseType ()
         -> AST T.Text ParsePattern ParseType ()
splitTop (G.TopTypeDef l tscheme) =
  astTypeConstraint %~ cons (toText l, toType tscheme)
splitTop (G.TopType _u _s         ) = undefined
splitTop (G.TopTypeParam _u _ls _s) = undefined
splitTop (G.TopFunc l ps e) =
  astExpr %~ cons (PVar (toText l), EAbs (toPatternList ps) (toExpr e))
splitTop (G.TopExpr l e      ) = astExpr %~ cons (PVar (toText l), toExpr e)
splitTop (G.TopPat  p e      ) = astExpr %~ cons (toPattern p, toExpr e)
splitTop (G.TopProc _l _ps _p) = undefined
splitTop (G.TopData u cs     ) = astDataDef %~ cons (toText u, toData u [] cs)
splitTop (G.TopDataParam u ls cs) =
  astDataDef %~ cons (toText u, toData' u ls cs)
