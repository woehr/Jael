{-#Language NoImplicitPrelude #-}

module Jael.Seq.Enum
( Enum(..)
, gToEnum
, validateEnum
) where

import ClassyPrelude
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Grammar
import Jael.Util
import Jael.Seq.AST
import Jael.Seq.Types

data Enumeration = Enumeration

tysToTyVars :: [Ty] -> [Text]
tysToTyVars [] = []
tysToTyVars (t:ts) = case t of
                          (TVar x) -> x:tysToTyVars ts
                          _        -> tysToTyVars ts

validateEnum :: Enumeration -> Either TDefError [(Text, PolyTy)]
validateEnum e = undefined

gToTVars :: [GTVars] -> [Text]
gToTVars = map (\(GTVars (LIdent s)) -> pack s)

gToEnum :: GTEnumDef -> Enumeration
gToEnum (GTEnumDef (UIdent n) tvs xs) = undefined

