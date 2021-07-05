{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Jael.Error where

import Jael.Prelude

data JaelError
  = JENameError ExprError
  | JEPatError  PatternError
  deriving (Show)

data GrammarError =
  Foo | Bar
deriving instance Show GrammarError

data PatternError
  = forall a. Show a => PEDupBinds [a]
  | forall a. Show a => PEInvalidConstructor a
  | forall a. Show a => PEArity { pearityName     :: a
                                , pearityActual   :: Integer
                                , pearityExpected :: Integer
                                }
  | PEMultiMulti
  | PEInvalidMulti
deriving instance Show PatternError

data ExprError
  = forall a. Show a => EEUnboundVar a
  | forall a. Show a => EEShadowedVar a
deriving instance Show ExprError

makeClassyPrisms ''JaelError
makeClassyPrisms ''PatternError
makeClassyPrisms ''GrammarError
makeClassyPrisms ''ExprError
