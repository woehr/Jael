module Test.Jael.Seq.TI
( seqInfTests
) where

import           Jael.Seq.HM_Types
import qualified Test.Framework as T
import           Test.Jael.Util

seqInfTests :: [T.Test]
seqInfTests =
  [ testCase "plus" $ checkInferredType exprPlus
  , testCase "int div result type" $ checkInferredType exprIntDiv
  , testCase "type of int div result constructor" $ checkInferredType exprConstrIntDivRes
  , testCase "accessors, first field" $ checkInferredType exprAccessor0
  , testCase "accessors, second field" $ checkInferredType exprAccessor1
  , testCase "tuple expr syntax" $ checkInferredType exprTup
  ]

checkInferredType :: (Text, Ty) -> Assertion
checkInferredType = checkInference testTypes

testTypes :: Text
testTypes = pack [raw|
  struct S { f0 : Bool , f1 : Int }
  enum   E { t0 Bool   , t1 Int   }
|]

exprPlus :: (Text, Ty)
exprPlus = (pack [raw|
  1+~2+3
|], (TySimple TyInt))

exprIntDiv :: (Text, Ty)
exprIntDiv = (pack [raw|
  1/2
|], TyNamed "IntDivRes" [])

exprConstrIntDivRes :: (Text, Ty)
exprConstrIntDivRes = (pack [raw|
  intDivRes
|], TyFun (TySimple TyInt) (TyFun (TySimple TyInt) (TyNamed "IntDivRes" [])))

exprAccessor0 :: (Text, Ty)
exprAccessor0 = (pack [raw|
  s::f0(s(true, 0))
|], (TySimple TyBool))

exprAccessor1 :: (Text, Ty)
exprAccessor1 = (pack [raw|
  s::f1(s(true, 4))
|], (TySimple TyInt))

exprTup :: (Text, Ty)
exprTup = (pack [raw|
  (1,true)
|], TyTup [(TySimple TyInt), (TySimple TyBool)]
  )

