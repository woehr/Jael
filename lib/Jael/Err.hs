{-# Language NoImplicitPrelude #-}

module Jael.Err
( module Jael.Err
, module Control.Monad.Except
) where

import ClassyPrelude
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Except

data CompileErr = ParseErr Text
                | DupDef [Text]
                | UndefName (S.Set Text)
                | DepCycle [Text]
                | TypeDefErr [Text]
                | TypeInfErr [Text]
                | AmbigName (M.Map Text (S.Set Text))
  deriving (Eq, Show)

type CompileErrM = Either CompileErr

