{-# Language NoImplicitPrelude #-}

module Jael.Err
( module Jael.Err
, module Control.Monad.Except
) where

import ClassyPrelude
import qualified Data.Set as S
-- TODO: Change to Control.Monad.Except when using 7.10
import Control.Monad.Except

data CompileErr = ParseErr Text
                | DupDef [Text]
                | UndefName (S.Set Text)
                | DepCycle [Text]
                | TypeDefErr [Text]
                | TypeInfErr [Text]
  deriving (Eq, Show)

type CompileErrM = Either CompileErr

