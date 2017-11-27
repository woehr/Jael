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

defaultData :: M.Map T.Text Type
defaultData =
  let ds = map (parseThrow pData)
             [ "data Bool { false; true }"
             , "data Maybe(a) { nothing; just(a) }"
             ]
      ds' = map (fmap $ hoistFix unQType . removeAnn) ds
   in M.unions $ map dataConTypes ds'

inferThrow :: M.Map T.Text Type -> E' -> TypedExpr
inferThrow ds e =
  case infer ds e of
    Left err -> error (show err)
    Right t  -> t

typeStrEq :: String -> String -> (Bool, Type, Type)
typeStrEq e t =
  let expr = parseExpr' e
      expr' = hoistCofree (mapExprP expandPattern) expr
      te = inferThrow defaultData expr'
      expectedType = parseType t
      actualType = exprType defaultData te
   in (alphaEq expectedType actualType, expectedType, actualType)

shouldHaveType :: String -> String -> Expectation
shouldHaveType e t =
  let (b, expectedType, actualType) = typeStrEq e t
   in if b
        then return ()
        else expectationFailure $
          "Types not equal, expected:\n\t" ++ show expectedType ++ "\n" ++
          "but got:\n\t" ++ show actualType ++ "\n"

shouldNotHaveType :: String -> String -> Expectation
shouldNotHaveType e t =
  let (b, expectedType, actualType) = typeStrEq e t
   in if not b
        then return ()
        else expectationFailure $
          "Types are equal, expected:\n\t" ++ show expectedType ++ "\n" ++
          "and:\n\t" ++ show actualType ++ "\n" ++
          "to be unequal."

shouldNotUnify :: String -> Expectation
shouldNotUnify e =
  let expr = parseExpr' e
      expr' = hoistCofree (mapExprP expandPattern) expr
   in case infer defaultData expr' of
        Left _ -> return ()
        Right res -> expectationFailure $
          "Type inference succeeded, the resulting type was:\n"
            ++ show (exprType defaultData res)

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
      "\\(($a,_,1) ⋎ (_,$a,_)) -> a"
        `shouldHaveType` "forall a. (a,a,Int)->a"

    it "should infer applications" $ do
      "1+2" `shouldHaveType` "Int"

    it "should infer if expressions" $ do
      "if true then 1 else 2" `shouldHaveType` "Int"

    it "should infer let expressions" $ do
      "{ $x = 1; x }" `shouldHaveType` "Int"
      "{ $x = just; x(1) }" `shouldHaveType` "Maybe(Int)"
      "{ $x = nothing; x }" `shouldHaveType` "forall a. Maybe(a)"
      "{ $x@($y) = nothing; (x,y) }" `shouldHaveType`
        "forall a b. (Maybe(a), Maybe(b))"
      "{ $x@([_,$y,$z]) = [1, 2, 3]; (x,y,z) }" `shouldHaveType`
        "([Int; 3], Int, Int)"
      "{ $x = 1+1; x }" `shouldHaveType` "Int"
      "{ $x@(nothing) = just(1); x }" `shouldHaveType` "Maybe(Int)"
      "{ $x@(just(1)) = nothing; x }" `shouldHaveType` "Maybe(Int)"
      "{ just($y) = nothing; y }" `shouldHaveType` "forall a. a"
      "{ just($x) = just(1); x }" `shouldHaveType` "Int"

    it "should infer records" $ do
      -- Order of labels does not matter
      "{ x=1, y=true, z=nothing }" `shouldHaveType`
        "forall a. { z: Maybe(a), y: Bool, x: Int }"
      -- Order of the types of x do matter
      "{ x=1, x=true, x=nothing }" `shouldHaveType`
        "forall a. { x: Int, x: Bool, x: Maybe(a) }"
      "{ x=1, x=true }" `shouldNotHaveType`
        "{ x: Bool, x: Int }"
      -- Order of x's and y's types matter only
      -- amongst labels of the same name
      "{ y=true, x=1, y=0, x=false}" `shouldHaveType`
        "{ x:Int, x:Bool, y:Bool, y:Int }"
      "{ y=true, x=1, y=0, x=false}" `shouldNotHaveType`
        "{ x: Bool, x:Int, y:Int, y:Bool }"
      "\\($x) -> { x = 1 | x }" `shouldHaveType` "forall r. {r}-> {x:Int|r}"
    it "should infer record types where unification is necessary" $ do
      "{ {y=4, z=$y | _} = {x=1,y=2,y=just,z=nothing}; { a=y } }"
        `shouldHaveType` "forall a. { a:Maybe(a) }"
      "{ {y=4, z=$y | $r} = {x=1,y=2,y=just,z=nothing}; {a=y|r} }"
        `shouldHaveType` "forall a b. { a:Maybe(a), x:Int, y:b->Maybe(b) }"
      "{ $r@({y=4, z=$y | $s}) = {x=1,y=2,y=just,z=nothing}; {a=r, b=s} }"
        `shouldHaveType` "forall a b c.\
                        \{ a:{x:Int,y:Int,y:a->Maybe(a),z:Maybe(b)}\
                        \, b:{x:Int,y:c->Maybe(c)} }"
    it "should terminate (and not unify)" $ do
      shouldNotUnify "\\($r) -> if true then { x=1 | r } else { y = 2 | r }"
      shouldNotUnify "\\($r) -> if true then { a=1, x=1, z=1 | r } else { a=2, y=2, z=2 | r }"
    it "should typecheck record restriction" $ do
      "\\($r) -> r--x" `shouldHaveType` "forall a r. {x:a|r} -> {r}"
    it "should typecheck record selection" $ do
      "\\($r) -> r.x" `shouldHaveType` "forall a r. {x:a|r} -> a"
    it "should typecheck compound record operations" $ do
      -- Update
      "\\($r, $a) -> { x=a | r--x}" `shouldHaveType` "forall a b r. {x:a|r} -> b -> {x:b|r}"
      "\\($r, $a) -> { x:=a | r}" `shouldHaveType` "forall a b r. {x:a|r} -> b -> {x:b|r}"
      -- Rename
      "\\($r) -> { y=r.x | r--x}" `shouldHaveType` "forall a r. {x:a|r} -> {y:a|r}"
      "\\($r) -> { y <- x | r}" `shouldHaveType` "forall a r. {x:a|r} -> {y:a|r}"
    it "should typecheck case expressions" $ do
      "case true of { false -> 0; $x -> 1 }" `shouldHaveType` "Int"
      "\\($x) -> case x of { nothing -> 0; just($y) -> y }" `shouldHaveType` "Maybe(Int) -> Int"
      "\\($x) -> case x of { $y -> y; _ -> x }" `shouldHaveType` "forall a. a -> a"
    it "should typecheck multi-if expressions" $ do
      "if | true then 0 | false then 1 | true then 2 " `shouldHaveType` "Int"
      "if | true then 0 | else 1" `shouldHaveType` "Int"
      shouldNotUnify "if | true then 0 | else true"
      shouldNotUnify "if | true then 0 | false then false"
      shouldNotUnify "if | 1 then 0 | false then 0"
    it "should typecheck lambda cases" $ do
      "\\case { true -> 0; false -> 1 }" `shouldHaveType` "Bool -> Int"
      "\\case { 1 -> 0; 2 -> 1; $x -> x }" `shouldHaveType` "Int -> Int"
      "\\case { just($x) -> x }" `shouldHaveType` "forall a. Maybe(a) -> a"
      "\\($f) -> \\case { $x -> f(x) }" `shouldHaveType` "forall a b. (a -> b) -> a -> b"
