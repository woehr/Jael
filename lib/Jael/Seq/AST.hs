{-# Language NoImplicitPrelude #-}

module Jael.Seq.AST where

import ClassyPrelude
import Data.List.NonEmpty
import qualified Data.Map as M
import qualified Data.Set as S

data Ex = EVar Text
        | EUnit
        | EInt Integer
        | EBool Bool
        | EIdx Ex Ex
        | EApp Ex Ex
        | EAbs Text Ex
        | ELet Text Ex Ex
          deriving (Show)

data Ty = TVar Text
        | TUnit
        | TInt
        | TBool
        | TNamed Text [Ty]
        | TFun Ty Ty
          deriving (Eq, Show)

data PolyTy = PolyTy [Text] Ty
              deriving (Show)

type TyEnv = M.Map Text PolyTy

type TySub = M.Map Text Ty

