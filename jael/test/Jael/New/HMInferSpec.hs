{-# Language OverloadedStrings #-}

module Jael.New.HMInferSpec (spec) where

import qualified Data.Text as T

import Test.Hspec

import Jael.Test.Util
import Jael.New.DataDecl
--import Jael.New.HMInfer
import Jael.New.Expr
import Jael.New.Type
import Jael.New.QType
import Jael.New.Parser
import Jael.New.Misc

defaultData :: [(T.Text, DataDecl Type)]
defaultData = map (fmap (fmap (unQType . removeAnn)) . parseThrow pData)
  [ "data Bool { false; true }"
  , "data Maybe(a) { nothing; just(a) }"
  ]

inferThrow :: [(T.Text, DataDecl Type)] -> Expr () Pattern -> Type
inferThrow ds e = undefined ds e -- case infer ds e of
  --Left err -> error err
  --Right t  -> t

shouldHaveType :: String -> String -> Expectation
shouldHaveType e t = inferThrow defaultData (parseExpr e) `shouldBe` parseType t

spec :: Spec
spec = do
  describe "..." $ do
    it "should ..." $ do
      "" `shouldHaveType` ""
