{-# Language NoImplicitPrelude #-}
{-# Language OverloadedStrings #-}

module Jael.ConsGenSpec (spec) where

import Jael.Prelude
import Test.Hspec

import qualified Data.Text as T
import qualified Jael.Grammar as G
import qualified Language.Fixpoint.Types as F

spec :: Spec
spec = do
  describe "Liquid constraint generation" $ do
    it "generates constrains" $ do
      pending
