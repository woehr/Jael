{-# Language NoImplicitPrelude #-}

module Jael.Seq.Types where

import ClassyPrelude
import Jael.Grammar
import Jael.Seq.AST

newtype DuplicateTyVars = DuplicateTyVars [Text]
  deriving Show

newtype DuplicateFields = DuplicateFields [Text]
  deriving Show

newtype FreeTyVars = FreeTyVars [Text]
  deriving Show

newtype UnusedTyVars = UnusedTyVars [Text]
  deriving Show

data TDefError = TDefError DuplicateTyVars
                           DuplicateFields
                           FreeTyVars
                           UnusedTyVars
                 deriving Show

gToType :: GType -> Ty
gToType GTInt = TInt
gToType GTBool = TBool
gToType (GTUnit GUnit) = TUnit
gToType (GTNamed (UIdent n) GTNamedNoParam) = TNamed (pack n) []
gToType (GTNamed (UIdent n) (GTNamedParams xs)) = TNamed (pack n) (map (\(GTNamedParam t) -> gToType t) xs)
gToType (GTTVar (LIdent s)) = TVar (pack s)

