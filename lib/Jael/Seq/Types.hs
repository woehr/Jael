{-# Language NoImplicitPrelude #-}

module Jael.Seq.Types where

import ClassyPrelude
import Jael.Grammar
import Jael.Seq.AST

gToType :: GType -> Ty
gToType GTInt = TInt
gToType GTBool = TBool
gToType (GTUnit GUnit) = TUnit
gToType (GTNamed (UIdent n) GTNamedNoParam) = TNamed (pack n) []
gToType (GTNamed (UIdent n) (GTNamedParams xs)) = TNamed (pack n) (map (\(GTNamedParam t) -> gToType t) xs)
gToType (GTTVar (LIdent s)) = TVar (pack s)

