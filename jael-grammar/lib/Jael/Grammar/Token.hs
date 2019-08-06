{-# Language DataKinds #-}
{-# Language DeriveFunctor #-}
{-# Language PatternSynonyms #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
{-# Language UndecidableInstances #-}

module Jael.Grammar.Token where

import Data.OpenADT (OpenADT)
import Data.Row (type (.==), type (.+), Row)

import qualified Data.Row.Internal as RI
import qualified Data.Text as T

import Jael.Types.Expr
import Jael.Grammar.Located

data Token a
  = TokenDec a
  | TokenAlpha a
  | TokenEOF
  deriving (Eq, Functor, Show)

pattern IgnoreLocation :: Token a -> Located (Token a)
pattern IgnoreLocation x <- Located _ _ x

pattern EOF :: Located (Token a)
pattern EOF <- IgnoreLocation TokenEOF

type SimpleExprRowF =
     "eAbsF" .== EAbsF T.Text
  .+ "eAppF" .== EAppF
  .+ "eVarF" .== EVarF T.Text

type family Phase (f :: * -> *) :: Row (* -> *) where
  Phase EAppF = "eAbsF" .== EAbsF T.Text .+ "eAppF" .== EAppF .+ "eVarF" .== EVarF T.Text

type Expr ph 

foo :: Expr Phase1 
