module Jael.Compile where

import Jael.Parser

data CompileErr = ParseErr Text
--                | DupDef [Text]
--                | FuncArgDupDef Text
--                | UndefName (S.Set Text)
--                | DepCycle [Text]
--                | TypeDefErr [Text]
--                | TypeInfErr S2TypeErr
--                | AmbigName (M.Map Text (S.Set Text))
--                | ProcSeqErr (M.Map Text [S2ProcErr])
--                | ProtocolValidationErr (M.Map Text SessDefErr)
--                | ProcessValidationErr (M.Map Text ProcDefErr)
  deriving (Eq, Show)

type CompileErrM = Either CompileErr

-- Split the compilation process into somewhat arbitrary stages to facilitate
-- easier testing.
compile :: Text -> CompileErrM Text
compile inp = do
  gProg <- either (throwError . ParseErr) id (parseProgram inp)
  return "Unimplemented"
