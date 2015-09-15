{-# Language NoImplicitPrelude, DeriveFunctor #-}

module Jael.Hw.Seq where

import ClassyPrelude hiding (Foldable)
import Data.Functor.Foldable
import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Seq.AST
import Jael.Seq.Types

data HwTy = Hw

data HwEx = HwLet Text Ex HwEx
          | HwRead Text Text HwEx
          | HwWrite Text Ex HwEx
          | HwYield Text Ex
  deriving (Show)

data HwExF a = HwLetF Text Ex a
             | HwReadF Text Text a
             | HwWriteF Text Ex a
             | HwYieldF Text Ex
  deriving (Functor, Show)

data HwSeqErr = ReusedVars (S.Set Text)
  deriving (Eq, Show)

hwSeqInfer :: M.Map Text (Integer, Ty)
           -> HwEx
           -> Either HwSeqErr HwEx
hwSeqInfer memLocs ex = undefined

