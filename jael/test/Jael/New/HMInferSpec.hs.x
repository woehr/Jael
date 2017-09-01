{-# Language OverloadedStrings #-}

module Jael.New.HMInferSpec (spec) where

import Test.Hspec

import Jael.New.HMInfer
import Jael.New.Expr
import Jael.New.Types
import Jael.New.Parser

defaultData = map parseData
  [ "data Bool { false; true }"
  , "data Maybe(a) { nothing; just(a) }"
  ]

parseThrow :: Parser a -> String -> a
parseThrow p t = case parseString (p <* eof) (Directed "" 0 0 0 0) t of
                   Failure e -> error . show . _errDoc $ e
                   Success x -> x

parseScheme :: String -> Type
parseScheme = parseThrow pScheme

parseData :: String -> DataDecl
parseData = parseThrow pData

parseExpr :: String -> E
parseExpr = parseThrow pExpr0

inferThrow :: [DataDecl] -> E -> Type
inferThrow ds e = case infer ds e of
  Left err -> error err
  Right t  -> t

shouldHaveType :: String -> String -> a
shouldHaveType e t = inferThrow defaultData (parseExpr e) `shouldBe` parseScheme t

spec :: Spec
spec = do
  describe "..." $ do
    it "should ..." $ do
      undefined `shouldBe` undefined
