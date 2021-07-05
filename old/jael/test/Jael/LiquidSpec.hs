{-# LANGUAGE NoImplicitPrelude #-}

module Jael.LiquidSpec where

import Test.Hspec

import Jael.Liquid
import Jael.Prelude.Minimal

type CE a b c = CoreExpr a b c

spec :: Spec
spec = describe "Jael's liquid type checking" $ do
  it "should ..." $ do
    undefined `shouldBe` (2 :: Integer)
