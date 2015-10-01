{-# Language NoImplicitPrelude #-}

module Jael.Err
( module Jael.Err
, module Control.Monad.Error
) where

import ClassyPrelude
-- TODO: Change to Control.Monad.Except when using 7.10
import Control.Monad.Error

data CompileErr = ParseErr Text
                | DupDef [Text]
                | UndefVar [Text]
                | UndefType [Text]
                | CallCycle [Text]
                | RecType [Text]
                | TypeDefErr [Text]
                | TypeInfErr [Text]
  deriving (Eq, Show)

type CompileErrM = Either CompileErr

