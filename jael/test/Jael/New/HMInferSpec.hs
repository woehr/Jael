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
      ds' = map (fmap $ hoistFix unQType . removeAnn) ds
   in M.unions $ map dataConTypes ds'

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
   in if alphaEq actualType expectedType
         then return ()
         else expectationFailure $
           "Types not equal, expected:\n\t" ++ show expectedType ++ "\n" ++
           "but got:\n\t" ++ show actualType ++ "\n"

spec :: Spec
spec = do
  describe "inference of simple expressions" $ do
    it "should infer ADT constants" $ do
      "true" `shouldHaveType` "Bool"
    it "should infer integers" $ do
      "1" `shouldHaveType` "Int"
    it "should infer abstractions" $ do
      "\\($a) -> a" `shouldHaveType` "forall a. a -> a"
      "\\($a) -> 1" `shouldHaveType` "forall a. a->Int"
      "\\($a@(($b, $c))) -> (a, b, c)"
        `shouldHaveType` "forall a b. (a,b) -> ((a,b),a,b)"
      "\\(($a,_,1) | (_,$a,_)) -> a"
        `shouldHaveType` "forall a. (a,a,Int)->a"
    it "should infer applications" $ do
      "1+2" `shouldHaveType` "Int"
    it "should infer if expressions" $ do
      "if true then 1 else 2" `shouldHaveType` "Int"
    it "should infer let expressions" $ do
      "{ $x = 1; x }" `shouldHaveType` "Int"
      "{ $x = just; x(1) }" `shouldHaveType` "Maybe(Int)"
      "{ $x = nothing; x }" `shouldHaveType` "forall a. Maybe(a)"
      "{ $x@($y) = nothing; (x,y) }"
        `shouldHaveType` "forall a b. (Maybe(a), Maybe(b))"
      "{ $x@([_,$y,$z]) = [1, 2, 3]; (x,y,z) }"
        `shouldHaveType` "([Int; 3], Int, Int)"
      "{ [$x@(nothing)] = [just 1]; x }" `shouldHaveType` "Maybe(Int)"
