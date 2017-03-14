{-# Language NoImplicitPrelude #-}
{-# Language OverloadedStrings #-}

module Jael.Classes.TIOpsSpec (spec) where

import           Jael.Prelude
import           Jael.Test.Util
import           Test.Hspec

import qualified Data.Text as T
--import qualified Language.Fixpoint.Types as F
--import qualified Text.PrettyPrint.Leijen.Text as P

import           Jael.Types
import           Jael.Util

spec :: Spec
spec = do
  describe "Inference" $ do
    it "" $ do
      let p = "\\(x:{v:Int|v>0}) { x+1 }" :: T.Text
      pending

    it "" $ do
      let p = "\\(x)\
              \{ id = \\(y) {y}\
              \; id(x)+id(1)\
              \}" :: T.Text
      pending
