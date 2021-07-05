{-# Language DeriveGeneric     #-}

module Jael.Grammar.AST where

import           Jael.Grammar.Token
import           Data.TreeDiff                  ( ToExpr )
import           GHC.Generics                   ( Generic )

data JaelExpr = JELet [(S, JaelExpr)] JaelExpr
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
              | JELIdent S
              | JEUIdent S
              deriving (Eq, Show, Generic)

instance ToExpr JaelExpr
