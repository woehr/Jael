{-# Language OverloadedStrings #-}

module Jael.New.ExprSpec (spec) where

import Test.Hspec
import Jael.Test.Util
import Jael.New.Check
import Jael.New.DataDecl
import Jael.New.Expr
import Jael.New.Type
import Jael.New.QType
import Jael.New.Misc

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
        [ PRec [("x", PPat "a" []), ("y", PPat "d" [])] (Just "r")
        , PRec [("x", PPat "b" []), ("y", PPat "d" [])] (Just "r")
        , PRec [("x", PPat "c" []), ("y", PPat "d" [])] (Just "r")
        , PRec [("x", PPat "a" []), ("y", PPat "e" [])] (Just "r")
        , PRec [("x", PPat "b" []), ("y", PPat "e" [])] (Just "r")
        , PRec [("x", PPat "c" []), ("y", PPat "e" [])] (Just "r")
        , PRec [("x", PPat "a" []), ("y", PPat "f" [])] (Just "r")
        , PRec [("x", PPat "b" []), ("y", PPat "f" [])] (Just "r")
        , PRec [("x", PPat "c" []), ("y", PPat "f" [])] (Just "r")
        ]

  describe "pattern checking" $ do
    it "should dup binds" $ do
      patternErrs "c($a,$a)" `shouldBe` [PE_DupBind "a"]
    it "should missing constructor" $ do
      patternErrs "d" `shouldBe` [PE_InvalidConstructor "d"]
      patternErrs "$x@(d)" `shouldBe` [PE_InvalidConstructor "d"]
    it "should multiple multi" $ do
      patternErrs "[..., ...]" `shouldBe` [PE_MultiMulti]
      patternErrs "$x@([...,...])" `shouldBe` [PE_MultiMulti]
    it "should invalid multi" $ do
      patternErrs "..." `shouldBe` [PE_InvalidMulti]
      patternErrs "c(_, ...)" `shouldBe` [PE_InvalidMulti]
      patternErrs "{x=...}" `shouldBe` [PE_InvalidMulti]
      patternErrs "$x@(...)" `shouldBe` [PE_InvalidMulti]
    it "should valid multi" $ do
      patternErrs "[[a,b,...], ..., [...,b,c(1,2)]]" `shouldBe` []
    it "should arity" $ do
      patternErrs "c" `shouldBe` [PE_Arity "c" 2 0]
      patternErrs "c(1, c(1))" `shouldBe` [PE_Arity "c" 2 1]
      patternErrs "$x@(c)" `shouldBe` [PE_Arity "c" 2 0]
