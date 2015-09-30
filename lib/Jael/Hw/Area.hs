{-# Language NoImplicitPrelude #-}

module Jael.Hw.Area where

import ClassyPrelude
import qualified Data.Set as S
import Jael.UserDefTy

data HwArea = HwArea
  deriving (Show)

data HwAreaErr = HwAreaErr
  { aErrTvUsed :: S.Set Text
  } deriving (Eq, Show)

