{-# Language NoImplicitPrelude #-}

module Jael.Err where

import ClassyPrelude

data CompileErrs = ParseErr Text
                 | DupDef [Text]
                 | UndefVar [Text]
                 | UndefType [Text]
                 | CallCycle [Text]
                 | RecType [Text]
                 | TypeDefErr [Text]
                 | TypeInfErr [Text]
  deriving (Eq, Show)

type CompileErr = Either CompileErrs

