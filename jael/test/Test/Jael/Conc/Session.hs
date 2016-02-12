module Test.Jael.Conc.Session
( sessionTests
) where

import qualified Data.Set as S
import           Jael.Conc.Session
import           Jael.Grammar
import           Jael.Parser
import           Jael.Seq.Types
import qualified Test.Framework as T
import           Test.Jael.Util

sessionTests :: [T.Test]
sessionTests =
  [ testCase "session dual" $ checkSessionDual testDual
  , testCase "session dual with vars" $ checkSessionDual testDualVars
  , testCase "duplicate induction variable" $ checkSessionErr dupIndVar
  , testCase "duplicate label" $ checkSessionErr dupLabel
  , testCase "unused recursion variable" $ checkSessionErr unusedRecVar
  , testCase "rec variable is dual error" $ checkSessionErr dualRecVar
  , testCase "no session behaviour" $ checkSessionErr noSessBehaviour
  ]

checkSessionErr :: (Text, SessDefErr) -> Assertion
checkSessionErr (t, expected) =
  case runParser pGSession t of
       Left err -> assertFailure (show err)
       Right gDef ->
         case gToSession gDef of
              Left err -> assertEqual "" expected err
              _ -> assertFailure "Expected session def error"

checkSessionDual :: (Text, Session) -> Assertion
checkSessionDual = checkParsedTree (liftM (dual . (\g -> case gToSession g of
                                                              Left err -> error (show err)
                                                              Right s -> s
                                                  )
                                          ) . pGSession)

-- A session to be parsed and dualed, and the expected dual
testDual :: (Text, Session)
testDual = (pack [raw|
  ![Int<0,1>]
  ?[ ![Bool] ?[Bool] ]
  +[ a =>
   , b => ![Bool]
   , c => ?[ ![Bool] ?[Bool] ]
   , d => &[ a =>
           , b => ![Bool]
           , c => ?[ ![Bool] ?[Bool] ]
           ]
   , e => rec X. ?[Bool] ![Int<~1,0>] <X>
   ]
|], SGetTy (S2TySimple (BTInt 0 1))
  $ SPutSess (SPutTy (S2TySimple BTBool) $ SGetTy (S2TySimple BTBool) $ SEnd)
  $ SChoice [ ("a", SEnd)
            , ("b", SGetTy (S2TySimple BTBool) SEnd)
            , ("c", SPutSess (SPutTy (S2TySimple BTBool) $ SGetTy (S2TySimple BTBool) SEnd) SEnd)
            , ("d", SSelect [ ("a", SEnd)
                            , ("b", SGetTy (S2TySimple BTBool) SEnd)
                            , ("c", SPutSess (SPutTy (S2TySimple BTBool) $ SGetTy (S2TySimple BTBool) SEnd) SEnd)
                            ]
              )
            , ("e", SCoInd "X" $ SPutTy (S2TySimple BTBool) $ SGetTy (S2TySimple (BTInt (-1) 0)) $ SVar "X")
            ]
  )

testDualVars :: (Text, Session)
testDualVars = (pack [raw|
  rec X. +[ a => <X>
          , b => <NamedProto>
          ]
|], SCoInd "X"
  $ SChoice [ ("a", SVar "X")
            , ("b", SDualVar "NamedProto")
            ]
  )

dupIndVar :: (Text, SessDefErr)
dupIndVar = (pack [raw|
  rec X. &[ a=> rec X. ![Void] <X>
          , b=> ?[Bool]
          , c=> +[ a=>
                 , b=>
                 ]
          ]
|], SDEDupInd "X"
  )

dupLabel :: (Text, SessDefErr)
dupLabel = (pack [raw|
  rec X. &[ a=> <X>
          , b=> ?[Bool]
          , c=> +[ a=>
                 , a=>
                 , b=>
                 ]
          , c=>
          , d=>,d=>
          ]
|], SDEDupLabels $ S.fromList ["c", "d"]
  -- Checking errors out after the first error so only c, and d are reported as duplicates
  )

-- Make sure that even though Y is used in one case it isn't in the other.
unusedRecVar :: (Text, SessDefErr)
unusedRecVar = (pack [raw|
  rec X. &[ a=> rec Y. ![Bool] <Y>
          , b=> rec Y. ?[Bool] <Z>
          ]
|], SDEUnused $ S.fromList ["Y"]
  )

dualRecVar :: (Text, SessDefErr)
dualRecVar = (pack [raw|
  rec X. ?[Void] <dual X>
|], SDEDualRec "X"
  )

noSessBehaviour :: (Text, SessDefErr)
noSessBehaviour = (pack [raw|
  rec X. &[ a => rec Y. <Y>
          , b => <X>
          ]
|], SDETrivialRec "Y" "Y"
  )

