module Jael.Compile where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Jael.Grammar as G
import           Jael.Expr
import           Jael.Parser
import           Jael.Prog
--import           Jael.Type
import           Jael.Util
import qualified Text.PrettyPrint.Leijen.Text as PP

import Debug.Trace

data CompileErr = CE_GrammarErr T.Text
                | CE_ParserErr ParserErr
--                | CE_DupDef [T.Text]
--                | FuncArgDupDef Text
--                | UndefName (S.Set Text)
                | CE_DepCycle [T.Text]
--                | TypeDefErr [Text]
                | CE_HMTypeErr HMTypeErr
                | CE_LiquidTypeErr
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
  gProg <- left CE_GrammarErr (runParser G.pProg inp)
  prog  <- left CE_ParserErr  (parseProgram gProg)
  exprOrder <- left CE_DepCycle $ liftA reverse
                                $ findCycles
                                $ M.map (freeVars . unann) (pExprs prog)
  hmTypedExprs <- left CE_HMTypeErr $ foldM (inferHM $ pExprs prog) M.empty exprOrder
  trace (unlines . map (\(k,v)->T.unpack k ++ "=\n" ++ show v) . M.toList $ M.map PP.pretty hmTypedExprs)
        (return "")

inferHM :: M.Map T.Text MaybeTypedExpr
        -> M.Map T.Text TypedExpr
        -> T.Text
        -> Either HMTypeErr (M.Map T.Text TypedExpr)
inferHM untyped typed n =
  let env = M.map annOf typed
      expr = M.findWithDefault (error "should not happen") n untyped
  in  liftA (\v -> M.insert n v typed) (hm env expr)
