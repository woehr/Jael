{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Jael.Pretty where

import           Data.Either
import           Data.Function
import           Data.Functor
import           Data.List                                ( repeat
                                                          , zipWith
                                                          )
import           Data.Tuple
import           GHC.Err

import           Data.Text.Prettyprint.Doc
import           Haskus.Utils.VariantF

import qualified Data.TreeDiff                 as TD

import           Jael.Expr
import           Jael.Pattern
import           Jael.Prelude                      hiding ( group )

--------------------- Recursion scheme based pretty class ----------------------

class Pretty' (f :: * -> *) where
  pretty' :: f (Doc ann) -> Doc ann

instance (Functor (VariantF xs), Pretty' (VariantF xs)
         ) => Pretty (EADT xs) where
  pretty = cata pretty'

instance Pretty' (VariantF '[]) where
  pretty' = error "Empty variant"

instance (Pretty' x, Pretty' (VariantF xs)) => Pretty' (VariantF (x ': xs)) where
  pretty' v = case popVariantFHead v of
    Right x -> pretty' x
    Left xs -> pretty' xs

----------------------------- Expr (and related) ------------------------------

instance Pretty Literal where
  pretty (LInt i) = pretty i

instance Pretty JInt where
  pretty JInt{..} = case intFormat of
    BinInt -> let s = showIntAtBase 2 intToDigit intValue ""
      in pretty $ "0b" <> replicate (fromInteger $ intLength - length s) '0' <> s
    OctInt -> let s = showOct intValue "" in pretty $ "0o" <> replicate (fromInteger $ intLength - length s) '0' <> s
    HexInt -> let s = showHex intValue "" in pretty $ "0x" <> replicate (fromInteger $ intLength - length s) '0' <> s
    DecInt -> if intValue >= 0
      then pretty intValue
      else "~" <> pretty (-intValue)

---------------------------------- Patterns -----------------------------------

instance Pretty' POrF where
  pretty' (POrF xs) = encloseSep lparen rparen "||" xs

instance (Pretty b) => Pretty' (PConF b) where
  pretty' (PConF b xs) = if null xs
    then pretty b
    else ppApp b xs

instance (Pretty v) => Pretty' (PVarF v) where
  pretty' (PVarF v) = vc <> pretty v

instance (Pretty v) => Pretty' (PAtF v) where
  pretty' (PAtF v x) = vc <> pretty v <> "@" <+> ppParens [x]

instance (Pretty l) => Pretty' (PLitF l) where
  pretty' (PLitF l) = pretty l

instance Pretty' PWildF where
  pretty' _ = "_"

instance Pretty' PRecEmptyF where
  pretty' _ = "{}"

instance Pretty' PTupF where
  pretty' (PTupF xs) = ppParens xs

instance Pretty' PArrF where
  pretty' (PArrF xs) = ppBrackets xs

instance (Pretty v, Pretty b) => Pretty' (PRecF v b) where
  pretty' (PRecF [] PRecEmpty) = pretty (PRecEmpty :: RecTailPat v)
  pretty' (PRecF [] t)         = braces (pretty t)
  pretty' (PRecF rs PRecEmpty) = ppRec (fmap ppRow rs)
  pretty' (PRecF rs t)         = ppBracesPipe (fmap ppRow rs) (pretty t)

----------------------------------- Helpers -----------------------------------

-- for Variable Character
vc :: Doc ann
vc = "$"

myFlatAlt :: Doc ann -> Doc ann -> (Doc ann, Doc ann)
myFlatAlt l r = (flatAlt (l <> space) l, flatAlt (hardline <> r) r)

ppApp :: (Pretty a) => a -> [Doc ann] -> Doc ann
ppApp a = (pretty a <>) . align . ppParens

ppParens :: [Doc ann] -> Doc ann
ppParens =
  group . encloseSep (flatAlt "( " "(") (flatAlt (hardline <> ")") ")") ", "

ppBrackets :: [Doc ann] -> Doc ann
ppBrackets =
  group . encloseSep (flatAlt "[ " "[") (flatAlt (hardline <> "]") "]") ", "

ppBraces :: [Doc ann] -> Doc ann
ppBraces =
  group . encloseSep (flatAlt "{ " "}") (flatAlt (hardline <> "}") "}") ", "

ppRec :: [Doc ann] -> Doc ann
ppRec = group . uncurry encloseSep (myFlatAlt lbrace rbrace) ", "

ppRow :: (Pretty l) => (l, Doc ann) -> Doc ann
ppRow (l, p) = pretty l <+> equals <+> p

ppBracesPipe :: [Doc ann] -> Doc ann -> Doc ann
ppBracesPipe xs y = group
  (uncurry encloseSepTail
           (myFlatAlt lbrace rbrace)
           ", "
           (flatAlt "| " " | ")
           xs
           y
  )

encloseSepTail :: Doc ann
               -> Doc ann
               -> Doc ann
               -> Doc ann
               -> [Doc ann]
               -> Doc ann
               -> Doc ann
encloseSepTail l r s t xs y =
  cat (zipWith (<>) (l : repeat s) xs <> [t <> y]) <> r

--------------------------- TreeDiff related stuff ----------------------------

prettyTreeDiffDict :: TD.Pretty (Doc ann)
prettyTreeDiffDict = TD.Pretty
  { ppCon    = fromString
  , ppRec = ppBraces . fmap (\(fn, d) -> fromString fn <+> fillSep [equals, d])
  , ppLst    = ppBrackets
  , ppCpy    = id
  , ppIns    = ("+" <>)
  , ppDel    = ("-" <>)
  , ppSep    = sep
  , ppParens = parens
  , ppHang   = \x y -> hang 4 (fillSep [x, y])
  }

ppDiff :: (TD.ToExpr a) => a -> a -> Doc ann
ppDiff x y = TD.ppEditExpr prettyTreeDiffDict (TD.ediff x y)
