{-# Language NoImplicitPrelude, QuasiQuotes #-}

module Test.Jael.Seq.TI
( seqInfTests
) where

import ClassyPrelude
import Jael.Grammar
import Jael.Parser
import Jael.Seq.AST
import Jael.Seq.Env
import Jael.Seq.Expr (gToEx)
import Jael.Seq.TI
import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Jael.Util

seqInfTests :: [T.Test]
seqInfTests = [ testCase "plus" $ checkInferredType exprPlus
              , testCase "abs" $ testInferredType exprAbs
              , testCase "app" $ checkInferredType exprApp
              , testCase "if" $ checkInferredType exprIf
              , testCase "let" $ testInferredType exprLet
              , testCase "int div result type" $ checkInferredType exprIntDiv
              , testCase "accessors, first field" $ checkInferredType exprAccessor0
              , testCase "accessors, second field" $ checkInferredType exprAccessor1
              , testCase "struct, polymorphic field" $ checkInferredType exprAccessor2
              ]

checkInferredType :: (Text, Ty) -> Assertion
checkInferredType (tx, expected) =
  case runParser pGExpr tx of
       Left err -> assertFailure (unpack err)
       Right ex -> case seqInfer defaultEnv (gToEx ex) of
                        Left es -> assertFailure . unpack . intercalate "\n" $ es
                        Right ty -> assertEqual "" expected ty

testInferredType :: (Text, Ty -> Maybe Text) -> Assertion
testInferredType (tx, tester) =
  case runParser pGExpr tx of
    Left err -> assertFailure (unpack err)
    Right ex -> case seqInfer defaultEnv (gToEx ex) of
                     Left es -> assertFailure . unpack . intercalate "\n" $ es
                     Right ty -> case tester ty of
                                      Just t  -> assertFailure (unpack t)
                                      Nothing -> return ()

testSingleTv :: Int -> Ty -> Maybe Text
testSingleTv arity ty =
  case join $ liftA toMinLen (funVarsToList ty) :: Maybe (MinLen (Succ Zero) [Text]) of
    Just vs -> if length vs /= (arity + 1)
                  then Just $ "Expected arity of " ++ tshow arity ++ " but got " ++ tshow ty
                  else if all (== head vs) (tailML vs)
                          then Nothing
                          else Just $ "Expected all type variables to be the same. Got: " ++
                            intercalate " " (unMinLen vs)
    Nothing -> Just "Failed to convert to a list of type vars."

-- Second value of tuple is a function that tests the type of exprAbs
funVarsToList :: Ty -> Maybe [Text]
funVarsToList (TFun (TVar x) t) = liftA (x:) (funVarsToList t)
funVarsToList (TVar x) = Just (x:[])
funVarsToList _ = Nothing

testStruct :: Text
testStruct = pack [raw|
  struct X a { Bool :: f0, Int :: f1, a :: f2 }
|]

exprPlus :: (Text, Ty)
exprPlus = (pack [raw|
  1+~2+3
|], TInt)

-- * has type a->a->a so this is expected to be inferred as x->x->x->x
-- where x is some type variable
exprAbs :: (Text, Ty -> Maybe Text)
exprAbs = (pack [raw|
  \a b c -> {
    a*b*c
  }
|], testSingleTv 3)

exprApp :: (Text, Ty)
exprApp = (pack [raw|
  \a b c -> {
    a+b+c
  }(1)(2)(3)
|], TInt)

exprIf :: (Text, Ty)
exprIf = (pack [raw|
  \b -> {
    f = \a b c -> { // a and b are Int so c should be inferred as Int
      a+b*c
    };
    if b {
      f(1, 2)       // thus, f applied twice should be Int -> Int
    } else {
      f(4, 5)
    }
  }
|], TFun TBool (TFun TInt TInt))

exprLet :: (Text, Ty -> Maybe Text)
exprLet = (pack [raw|
  \a b c d e -> {
    f = a+b-c;
    g = b-c*d;
    h = c*d+d;
    i = d-e+a;
    f+g-h*i
  }
|], testSingleTv 5)

exprIntDiv :: (Text, Ty)
exprIntDiv = (pack [raw|
  1/2
|], TNamed "IntDivRes" [])

exprConstrIntDivRes :: (Text, Ty)
exprConstrIntDivRes = (pack [raw|
  intDivResult
|], TFun TInt (TFun TInt (TNamed "IntDivRes" [])))

exprAccessor0 :: (Text, Ty)
exprAccessor0 = (pack [raw|
  x(true, 0, 1)::0 && x(false, 2, 3)::f0
|], TBool)

exprAccessor1 :: (Text, Ty)
exprAccessor1 = (pack [raw|
  x(true, 4, 5)::1 + x(false, 6, 7)::f1
|], TInt)

exprAccessor2 :: (Text, Ty)
exprAccessor2 = (pack [raw|
  x(true, 4, false)::2 || x(false, 6, 7)::f0
|], TBool)
