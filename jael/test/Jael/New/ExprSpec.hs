{-# Language OverloadedStrings #-}

module Jael.New.ExprSpec (spec) where

import Test.Hspec
import Jael.Test.Util
import Jael.New.Check
import Jael.New.DataDecl
import Jael.New.Expr
import Jael.New.Type
import Jael.New.QType

someData :: DataDecl Type
someData = fmap (hoistFix unQType) . parseData $ "data T { a; b; c(Int, Int) }"

patternErrs :: String -> [PatternErr]
patternErrs s = case checkPattern someData . parsePattern $ s of
  Left es -> es
  Right _ -> []

spec :: Spec
spec = do
  describe "pattern expansion" $ do
    it "should remove or-patterns" $ do
      map removeAnn (expandPattern (parsePattern "a(d⋎e)⋎b⋎c")) `shouldBe`
        [ PPat "a" [PPat "d"[]]
        , PPat "a" [PPat "e"[]]
        , PPat "b" []
        , PPat "c" []
        ]

    it "should remove or-patterns (records)" $ do
      map removeAnn (expandPattern (parsePattern "{x=a \\/ b ∨ c, y=d ⋎ e ⋁ f | $r}"))
        `shouldBe`
        [ PRec [("x", PPat "a" []), ("y", PPat "d" [])] (TailBind "r")
        , PRec [("x", PPat "b" []), ("y", PPat "d" [])] (TailBind "r")
        , PRec [("x", PPat "c" []), ("y", PPat "d" [])] (TailBind "r")
        , PRec [("x", PPat "a" []), ("y", PPat "e" [])] (TailBind "r")
        , PRec [("x", PPat "b" []), ("y", PPat "e" [])] (TailBind "r")
        , PRec [("x", PPat "c" []), ("y", PPat "e" [])] (TailBind "r")
        , PRec [("x", PPat "a" []), ("y", PPat "f" [])] (TailBind "r")
        , PRec [("x", PPat "b" []), ("y", PPat "f" [])] (TailBind "r")
        , PRec [("x", PPat "c" []), ("y", PPat "f" [])] (TailBind "r")
        ]

  describe "pattern checking" $ do
    it "should dup binds" $ do
      patternErrs "c($a,$a)" `shouldBe` [PEDupBind "a"]
    it "should missing constructor" $ do
      patternErrs "d" `shouldBe` [PEInvalidConstructor "d"]
      patternErrs "$x@(d)" `shouldBe` [PEInvalidConstructor "d"]
    it "should arity" $ do
      patternErrs "c" `shouldBe` [PEArity "c" 2 0]
      patternErrs "c(1, c(1))" `shouldBe` [PEArity "c" 2 1]
      patternErrs "$x@(c)" `shouldBe` [PEArity "c" 2 0]
