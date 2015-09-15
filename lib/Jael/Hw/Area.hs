{-# Language NoImplicitPrelude #-}

module Jael.Hw.Area where

import ClassyPrelude
import qualified Data.Map as M
import Jael.Seq.Types
import Jael.Seq.UserDefTy

data HwAreaErr = PolyArea
  deriving (Eq, Show)

-- Given an area's name and type, a map of user defined types, and a starting
-- offset, return a map of the names and types of everything within the area.
calcMemLocations :: (Text, UserDefTy)
                 -> M.Map Text UserDefTy
                 -> Integer
                 -> Either HwAreaErr (M.Map Text (Integer, Ty))
calcMemLocations = undefined

