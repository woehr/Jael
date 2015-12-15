{-# Language RecordWildCards #-}

module Jael.Compile.Common where

import qualified Data.Map as M
import qualified Data.Set as S
import           Jael.Conc.Proc
import           Jael.Conc.Session
import           Jael.Conc.TyCk.S2
import           Jael.Seq.AST
import           Jael.Seq.TI.S2
import           Jael.Seq.Types
import           Jael.Seq.UserDefinedType

data CompileErr = ParseErr Text
                | DupDef [Text]
                | FuncArgDupDef Text
                | UndefName (S.Set Text)
                | DepCycle [Text]
                | TypeDefErr [Text]
                | TypeInfErr S2TypeErr
                | AmbigName (M.Map Text (S.Set Text))
                | ProcSeqErr (M.Map Text [S2ProcErr])
                | ProtocolValidationErr (M.Map Text SessDefErr)
  deriving (Eq, Show)

type CompileErrM = Either CompileErr

data TopArea = TopArea { taAddr :: Integer
                       , taType :: Text
                       } deriving (Show)

data Stage1 = Stage1 { s1Exprs     :: M.Map Text (TopExpr S1Ex S1Ty)
                     , s1Udts      :: M.Map Text UserDefinedType
                     , s1Areas     :: M.Map Text TopArea
                     , s1Protos    :: M.Map Text Session
                     , s1Procs     :: M.Map Text S1TopProc
                     , s1ExprOrder :: [Text]
                     } deriving (Show)

data Stage2 = Stage2 { s1Data      :: Stage1
                     , s2Exprs     :: M.Map Text (TopExpr S2TyEx S2Ty)
                     , s2ProcExprs :: M.Map Text S2PEx
                     , s2Procs     :: M.Map Text S2TopProc
                     } deriving (Show)

