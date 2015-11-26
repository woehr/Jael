{-# Language RecordWildCards #-}

module Jael.Compile.Common where

import qualified Data.Map as M
import qualified Data.Set as S
import           Jael.Conc.Proc
import           Jael.Conc.Session
import           Jael.Seq.CG_AST
import           Jael.Seq.CG_Types
import           Jael.Seq.HM_Types
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

type Program = ( M.Map Text (TopExpr CGEx GramTy)
               , M.Map Text UserDefinedType
               , M.Map Text TopArea
               , M.Map Text Session
               , M.Map Text TopProc
               )

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


