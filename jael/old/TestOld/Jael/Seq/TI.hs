module Test.Jael.Seq.TI
( seqInfTests
) where

import           Jael.Seq.Types
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

checkInferredType :: (Text, HMTy) -> Assertion
checkInferredType = checkInference testTypes

testTypes :: Text
testTypes = pack [raw|
  struct S { f0 : Bool , f1 : Int }
  enum   E { t0 Bool   , t1 Int   }
|]

exprPlus :: (Text, HMTy)
exprPlus = (pack [raw|
  1+~2+3
|], HMTyInt)

exprIntDiv :: (Text, HMTy)
exprIntDiv = (pack [raw|
  1/2
|], HMTyNamed "IntDivRes" [])

exprConstrIntDivRes :: (Text, HMTy)
exprConstrIntDivRes = (pack [raw|
  intDivRes
|], HMTyFun HMTyInt (HMTyFun HMTyInt (HMTyNamed "IntDivRes" [])))

exprAccessor0 :: (Text, HMTy)
exprAccessor0 = (pack [raw|
  s::f0(s(true, 0))
|], HMTyBool)

exprAccessor1 :: (Text, HMTy)
exprAccessor1 = (pack [raw|
  s::f1(s(true, 4))
|], HMTyInt)

exprTup :: (Text, HMTy)
exprTup = (pack [raw|
  (1,true)
|], HMTyTup [HMTyInt, HMTyBool]
  )

