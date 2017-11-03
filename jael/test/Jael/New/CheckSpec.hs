{-# Language OverloadedStrings #-}

module Jael.New.CheckSpec (spec) where

import qualified Data.Set as S
import Text.Trifecta
import Text.Trifecta.Delta

import Test.Hspec
import Jael.Test.Util
import Jael.New.Check

--spanEqs :: Span -> Span -> Bool
--spanEqs (Span d1 d2 _) (Span d1' d2' _) =
--  (d1 == d1') && (d2 == d2')

spanToDeltas :: Span -> (Delta, Delta)
spanToDeltas (Span d1 d2 _) = (d1, d2)

spec :: Spec
spec = do
  describe "non-parser syntax related checks" $ do
    it "should (expr) find unbound variables" $ do
      let result = map (second id) . unboundVars (S.fromList []) . parseExpr' $
            "a"
      map (second spanToDeltas) result `shouldBe`
        [("a", (Directed "test" 0 0 0 0, Directed "test" 0 1 1 1))]

    it "should (expr) find shadowed variables" $ do
      pending

    it "should (pattern) find duplicate binds" $ do
      pending
    it "should (pattern) find invalid constructors" $ do
      pending
    it "should (pattern) find multiple multi-wildcard usage" $ do
      pending
    it "should (pattern) identify multi-wildcards in non-array positions" $ do
      pending
    it "should (pattern) identify constructors used with incorrect arities" $ do
      pending
