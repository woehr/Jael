{-# Language RecordWildCards #-}

module Jael.Compile.Common where

import qualified Data.Map as M
import qualified Data.Set as S
import           Jael.Conc.Proc
import           Jael.Conc.Session
import           Jael.Seq.CG_AST
import           Jael.Seq.Types
import           Jael.Seq.UserDefinedType

data CompileErr = ParseErr Text
                | DupDef [Text]
                | FuncArgDupDef Text
                | UndefName (S.Set Text)
                | DepCycle [Text]
                | TypeDefErr [Text]
                | TypeInfErr CGTypeErr
                | AmbigName (M.Map Text (S.Set Text))
  deriving (Eq, Show)

type CompileErrM = Either CompileErr

data TopExpr e t = TopGlob { tgExpr :: e }
                 | TopFunc { tfArgs  :: [(Text, t)]
                           , tfRetTy :: t
                           , tfExpr  :: e
                 } deriving (Show)

instance (SeqTypable e, SeqTypable t) => SeqTypable (TopExpr e t) where
  tyOf (TopGlob{..}) = tyOf tgExpr
  tyOf (TopFunc{..}) = typesToFun (map (tyOf . snd) tfArgs, tyOf tfRetTy)

data TopArea = TopArea { taAddr :: Integer
                       , taType :: Text
                       } deriving (Show)

data Stage1 = Stage1 { s1Exprs     :: M.Map Text (TopExpr S1Ex S1Ty)
                     , s1Udts      :: M.Map Text UserDefinedType
                     , s1Areas     :: M.Map Text TopArea
                     , s1Protos    :: M.Map Text Session
                     , s1Procs     :: M.Map Text TopProc
                     , s1ExprOrder :: [Text]
                     } deriving (Show)

data Stage2 = Stage2 { s1Data   :: Stage1
                     , s2SeqEnv :: TyEnv
                     , s2Exprs  :: M.Map Text (TopExpr S2Ex S2Ty)
                     } deriving (Show)

