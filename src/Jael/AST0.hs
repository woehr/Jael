{-# LANGUAGE NoImplicitPrelude #-}

module Jael.AST0 where

import ClassyPrelude

import Jael.Grammar (Prog)

-- AST0 represents the top level definitions of a program by mapping
-- their name to their type and expression.
type AST0 = Map Text (Ty, Ex)

toAST0 :: Prog -> Either Text AST0
toAST0 p = Left "Error in toAST0"


data Lit  = LInt Integer
          | LFloat Double
            deriving (Show)

data Ex = EVar String
        | ELit Lit
        | EApp Ex Ex
        | EAbs Text Ex
        | ELet Text Ex Ex
          deriving (Show)

data Ty = TVar String
        | TInt
        | TFloat
        | TBool
        | TFun Ty Ty
          deriving (Show)

data PolyTy = PolyTy [Text] Ty
              deriving (Show)

