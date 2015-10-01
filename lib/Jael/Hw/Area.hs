{-# Language NoImplicitPrelude #-}
{-# Language MultiParamTypeClasses #-}

module Jael.Hw.Area where

import ClassyPrelude
import qualified Data.Set as S
import Jael.Grammar
import Jael.Seq.Types
import Jael.Conc.Session
import Jael.UserDefTy

data HwArea = HwArea
  deriving (Show)

data HwAreaErr = HwAreaErr
  { aErrTvUsed :: S.Set Text
  } deriving (Eq, Show)

data HwAreaTy = HwSeqTy Ty
              | HwSessTy Session
  deriving (Show)

data HwAreaGrammar = HwAreaGrammar GAnyInt GTAreaDef
  deriving (Show)

instance UserDefTy HwArea HwAreaGrammar HwAreaTy HwAreaErr where
  gToUserDefTy = undefined
  validate = validateHwArea
  typeDeps = undefined
  envItems = undefined

{-# Deprecated validateHwArea "TODO: Needs to be implemented" #-}
validateHwArea :: HwArea -> Maybe HwAreaErr
validateHwArea _ = Nothing

