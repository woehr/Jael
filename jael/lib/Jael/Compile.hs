module Jael.Compile where

import qualified Data.Text as T
import qualified Jael.Grammar as G
import           Jael.Parser

data CompileErr = CE_GrammarErr T.Text
                | CE_ParserErr ParserErr
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
compile :: T.Text -> CompileErrM T.Text
compile inp = do
  gProg <- either (throwError . CE_GrammarErr) return (runParser G.pProg inp)
  prog  <- either (throwError . CE_ParserErr)  return (parseProgram gProg)
  return (T.pack . show $ prog)
