{-# Language NoImplicitPrelude, QuasiQuotes, OverloadedStrings #-}

module Test.Jael.SeqTypeInf
( seqInfTests
) where

import ClassyPrelude
import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.HUnit

import Test.Jael.Util
import Jael.Grammar
import Jael.Parser
import Jael.Seq.AST

seqInfTests :: [T.Test]
seqInfTests = [ testCase "plus" $ checkInferredType exprPlus
              , testCase "abs" $ checkInferredType exprAbs
              , testCase "app" $ checkInferredType exprApp
              , testCase "if" $ checkInferredType exprIf
              ]

checkInferredType :: (Text, Ty) -> Assertion
checkInferredType (tx, ty) = either (assertFailure . unpack) (
    \gEx -> case seqInfer (toSeqEx gEx) of
                  Left es -> assertFailure . unpack . intercalate "\n" $ es
                  Right inferred -> ty @=? inferred
  ) (runParser pGExpr tx)

-- 1
exprPlus :: (Text, Ty)
exprPlus = (pack [raw|
  1+~2+3
|], TInt)

exprAbs :: (Text, Ty)
exprAbs = (pack [raw|
  \a b c -> {
    a*b*c
  }
|], TFun TInt (TFun TInt (TFun TInt TInt)))

exprApp :: (Text, Ty)
exprApp = (pack [raw|
  \a b c -> {
    a+b+c
  }(1)(2)(3)
|], TInt)

exprIf :: (Text, Ty)
exprIf = (pack [raw|
  \b -> {
    f = \a b c -> {
      a+b*c
    };
    if b {
      f(1, 2, 3)
    } else {
      f(4, 5, 6)
    }
  }
|], TFun TBool TInt)

