{-# Language DeriveGeneric     #-}

module Jael.Grammar.AST where

import qualified Data.Text                     as T
import           Data.TreeDiff                  ( ToExpr )
import           GHC.Generics                   ( Generic )
import        Jael.Grammar.Token ( IntInfo )

data JaelExpr = JELet [(T.Text, JaelExpr)] JaelExpr
              | JEIf JaelExpr JaelExpr JaelExpr

              | JEAdd JaelExpr JaelExpr
              | JESub JaelExpr JaelExpr
              | JEMul JaelExpr JaelExpr
              | JEDiv JaelExpr JaelExpr
              | JEMod JaelExpr JaelExpr

              | JEAnd JaelExpr JaelExpr
              | JEOr  JaelExpr JaelExpr
              | JEImp JaelExpr JaelExpr
              | JEIff JaelExpr JaelExpr

              | JEInt IntInfo
              | JELIdent T.Text
              | JEUIdent T.Text
              deriving (Eq, Show, Generic)

instance ToExpr JaelExpr
