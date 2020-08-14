{-# Language DeriveGeneric     #-}

module Jael.Grammar.AST where

import           Jael.Grammar.Token
import           Data.TreeDiff                  ( ToExpr )
import           GHC.Generics                   ( Generic )

data JaelExpr = JEAdd JaelExpr JaelExpr
            | JEInt IntInfo
        deriving (Eq, Show, Generic)
instance ToExpr JaelExpr
