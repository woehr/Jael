{-# Language OverloadedStrings #-}

module Jael.New.HMInferSpec (spec) where

import qualified Data.Map as M
import qualified Data.Text as T

import Test.Hspec

import Jael.Test.Util
import Jael.New.Check
import Jael.New.DataDecl
import Jael.New.HMInfer
import Jael.New.Expr
import Jael.New.Type
import Jael.New.QType
import Jael.New.Parser
import Jael.New.Misc

defaultData :: M.Map T.Text Type
defaultData =
  let ds = map (parseThrow pData)
             [ "data Bool { false; true }"
             , "data Maybe(a) { nothing; just(a) }"
             ]
      ds' = flip map ds $ second (fmap $ hoistFix unQType . removeAnn)
      ds'' = map (\(n, dd) -> (dataDeclType n dd, dataCons dd)) ds'
      cs = map (\(t, c) -> flip M.map c $ foldr TFun t) ds''
   in M.unions cs

inferThrow :: M.Map T.Text Type -> E' -> (Type, TypedE)
inferThrow ds e =
  case infer ds e of
    Left err -> error (show err)
    Right t  -> t

shouldHaveType :: String -> String -> Expectation
shouldHaveType e t =
  let expr = parseExpr' e
      expr' = hoistCofree (mapExprP expandPattern) expr
      (actualType, _) = inferThrow defaultData expr'
      expectedType = parseType t
   in actualType `shouldBe` expectedType

spec :: Spec
spec = do
  describe "inference of simple expressions" $ do
    it "should infer ADT constants" $ do
      "true" `shouldHaveType` "Bool"
    it "should infer integers" $ do
      "1" `shouldHaveType` "Int"
    xit "should infer polymorphic abstractions" $ do
      "\\($a) -> a" `shouldHaveType` "a -> a"
    xit "should infer concrete abstractions" $ do
      "\\($a) -> 1" `shouldHaveType` "a->Int"
    it "should infer applications" $ do
      "1+2" `shouldHaveType` "Int"
    xit "should infer pattern binds" $ do
      "\\($a@(($b, $c))) -> (a, b, c)" `shouldHaveType` "(a,b) -> ((a,b),a,b)"
    it "should infer if expressions" $ do
      "if true then 1 else 2" `shouldHaveType` "Int"
