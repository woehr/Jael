{-# LANGUAGE OverloadedStrings #-}

module Jael.Pretty.TreeDiff where

import           Data.String
import           Data.Text.Prettyprint.Doc
import qualified Data.TreeDiff                 as TD

import           Jael.Pretty.Helpers

prettyTreeDiffDict :: TD.Pretty (Doc ann)
prettyTreeDiffDict = TD.Pretty
  { TD.ppCon    = fromString
  , TD.ppLst    = ppBrackets
  , TD.ppCpy    = id
  , TD.ppIns    = ("+" <>)
  , TD.ppDel    = ("-" <>)
  , TD.ppSep    = sep
  , TD.ppParens = parens
  , TD.ppHang   = \x y -> hang 4 (fillSep [x, y])
  , TD.ppRec    = ppBraces
    . fmap (\(fn, d) -> fromString fn <+> fillSep [equals, d])
  }

ppDiff :: (TD.ToExpr a) => a -> a -> Doc ann
ppDiff x y = TD.ppEditExpr prettyTreeDiffDict (TD.ediff x y)
