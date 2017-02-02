{-# Language FlexibleInstances #-}
{-# Language NoImplicitPrelude #-}
{-# Language TypeSynonymInstances #-}

module Jael.Pretty where

import BasePrelude

import qualified Data.Text.Lazy as T
import           Text.PrettyPrint.Leijen.Text

import qualified Language.Fixpoint.Types as L


-- liquid-fixpoint pretty printers
class FPPretty a where
  fpPretty :: a -> Doc

instance FPPretty L.Expr where
  fpPretty = text . T.pack . L.showpp
