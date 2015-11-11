module Jael.Err where

import qualified Data.Map as M
import qualified Data.Set as S

data CompileErr = ParseErr Text
                | DupDef [Text]
                | UndefName (S.Set Text)
                | DepCycle [Text]
                | TypeDefErr [Text]
                | TypeInfErr [Text]
                | AmbigName (M.Map Text (S.Set Text))
  deriving (Eq, Show)

type CompileErrM = Either CompileErr

