{-# Language NoImplicitPrelude, QuasiQuotes #-}

module Test.Jael.Seq.TI
( seqInfTests
) where

import ClassyPrelude
import Jael.Grammar
import Jael.Parser
import Jael.Seq.AlgDataTy
import Jael.Seq.AST
import Jael.Seq.Env
import Jael.Seq.Expr (gToEx)
import Jael.Seq.TI
import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Jael.Util

seqInfTests :: [T.Test]
seqInfTests =
  [ testCase "plus" $ checkInferredType exprPlus
  , testCase "abs" $ checkInferredType exprAbs
  , testCase "app" $ checkInferredType exprApp
  , testCase "if" $ checkInferredType exprIf
  , testCase "let" $ checkInferredType exprLet
  , testCase "int div result type" $ checkInferredType exprIntDiv
  , testCase "type of int div result constructor" $ checkInferredType exprConstrIntDivRes 
  , testCase "accessors, first field" $ checkInferredType exprAccessor0
  , testCase "accessors, second field" $ checkInferredType exprAccessor1
  , testCase "struct, polymorphic field" $ checkInferredType exprAccessor2
  , testCase "tuple expr syntax" $ checkInferredType exprTup
  , testCase "built-in tuple constructor" $ checkInferredType exprTupCons
  ]

testStruct :: Text
testStruct = pack [raw|
  S a { f0 :: Bool , f1 :: Int , f2 :: a }
|]

testEnum :: Text
testEnum = pack [raw|
  E a { t0 Bool , t1 Int , t2 a , t3 }
|]

checkInferredType :: (Text, Ty) -> Assertion
checkInferredType (tx, expected) =
  case (runParser pGTStructDef testStruct, runParser pGTEnumDef testEnum) of
       (Left err, _) -> assertFailure (unpack err)
       (_, Left err) -> assertFailure (unpack err)
       (Right sdef, Right edef) ->
         case (validateAdt (gToStruct sdef), validateAdt (gToEnumer edef)) of
              (Left err, _) -> assertFailure (show err)
              (_, Left err) -> assertFailure (show err)
              (Right sfuns, Right efuns) ->
                case join $ liftA (flip addToEnv efuns) (addToEnv defaultEnv sfuns) of
                     Left dups -> assertFailure . unpack . intercalate "\n" $
                       "Duplicates in env:" : dups
                     Right env ->
                       case runParser pGExpr tx of
                            Left err -> assertFailure (unpack err)
                            Right ex ->
                              case seqInfer env (gToEx ex) of
                                   Left es -> assertFailure . unpack . intercalate "\n" $ es
                                   Right ty -> assertBool ("Expected:\n" ++
                                                           show expected ++
                                                           "\n    and:\n" ++
                                                           show ty ++
                                                           "\nto be equivalent."
                                                          ) (expected `tyEquiv` ty)

exprPlus :: (Text, Ty)
exprPlus = (pack [raw|
  1+~2+3
|], TInt)

-- * has type a->a->a so this is expected to be inferred as x->x->x->x
-- where x is some type variable
exprAbs :: (Text, Ty)
exprAbs = (pack [raw|
  \a b c -> {
    a*b*c
  }
|], TFun (TVar "a")
         (TFun (TVar "a")
               (TFun (TVar "a")
                     (TVar "a")
               )
         )
  )

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

exprLet :: (Text, Ty)
exprLet = (pack [raw|
  \a b c d e -> {
    f = a+b-c;
    g = b-c*d;
    h = c*d+d;
    i = d-e+a;
    f+g-h*i
  }
|], TFun (TVar "a")
         (TFun (TVar "a")
               (TFun (TVar "a")
                     (TFun (TVar "a")
                           (TFun (TVar "a")
                                 (TVar "a")
                           )
                     )
               )
         )
  )

exprIntDiv :: (Text, Ty)
exprIntDiv = (pack [raw|
  1/2
|], TNamed "IntDivRes" [])

exprConstrIntDivRes :: (Text, Ty)
exprConstrIntDivRes = (pack [raw|
  intDivRes
|], TFun TInt (TFun TInt (TNamed "IntDivRes" [])))

exprAccessor0 :: (Text, Ty)
exprAccessor0 = (pack [raw|
  S::f0(s(true, 0, 1))
|], TBool)

exprAccessor1 :: (Text, Ty)
exprAccessor1 = (pack [raw|
  S::f1(s(true, 4, 5))
|], TInt)

exprAccessor2 :: (Text, Ty)
exprAccessor2 = (pack [raw|
  S::f2(s(true, 4, false))
|], TBool)

exprTup :: (Text, Ty)
exprTup = (pack [raw|
  {1,true}
|], TNamed "Tup2" [TInt, TBool]
  )

exprTupCons :: (Text, Ty)
exprTupCons = (pack [raw|
  tup2(true)
|], TFun (TVar "a") (TNamed "Tup2" [TBool, TVar "a"])
  )

