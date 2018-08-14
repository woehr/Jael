{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Jael.QuotesSpec where

import qualified Data.Text as T

import Jael.AST
import Jael.Expr
import Jael.Prelude
import Jael.QType
import Jael.Quotes
import Jael.Type

import Test.Hspec

type SpecQType = ParseType

spec :: Spec
spec = describe "rtype quasi-quoter" $ do
  it "should Quote Jael syntax as QTypes" $
    [rtype| Int |] `shouldBe` (UnqualType (TConF @T.Text "Int" []) :: SpecQType)
  it "should replace ``-quoted variables with Haskell variables" $ do
    let _a = "10" :: String
    [rtype| (| v:A | `_a` |) |] `shouldBe` (QualType ("v", ELit (LInt (defaultInt 10))) (TConF @T.Text "A" []) :: SpecQType)
