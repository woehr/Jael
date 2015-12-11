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
         case validateSession (gToSession gDef) of
              Just err -> assertEqual "" expected err
              Nothing -> assertFailure "Expected session def error"

checkSessionDual :: (Text, Session) -> Assertion
checkSessionDual = checkParsedTree (liftM (dual . gToSession) . pGSession)

-- A session to be parsed and dualed, and the expected dual
testDual :: (Text, Session)
testDual = (pack [raw|
  ![Int]
  ?[ ![Int] ?[Int] ]
  +[ a =>
   , b => ![Int]
   , c => ?[ ![Int] ?[Int] ]
   , d => &[ a =>
           , b => ![Int]
           , c => ?[ ![Int] ?[Int] ]
           ]
   , e => rec X. ?[Bool] ![Int] <X>
   ]
|], SGetTy (TySimple TyInt)
  $ SPutSess (SPutTy (TySimple TyInt) $ SGetTy (TySimple TyInt) $ SEnd)
  $ SChoice [ ("a", SEnd)
            , ("b", SGetTy (TySimple TyInt) SEnd)
            , ("c", SPutSess (SPutTy (TySimple TyInt) $ SGetTy (TySimple TyInt) $ SEnd) SEnd)
            , ("d", SSelect [ ("a", SEnd)
                            , ("b", SGetTy (TySimple TyInt) SEnd)
                            , ("c", SPutSess (SPutTy (TySimple TyInt) $ SGetTy (TySimple TyInt) $ SEnd) SEnd)
                            ]
              )
            , ("e", SCoInd "X" $ SPutTy (TySimple TyBool) $ SGetTy (TySimple TyInt) $ SVar "X")
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
          , b=> ?[Int]
          , c=> +[ a=>
                 , b=>
                 ]
          ]
|], SessDefErr
      { sessErrDupInd = S.fromList ["X"]
      , sessErrDupLab = S.empty
      , sessErrUnused = S.empty
      , sessErrDualRec = S.empty
      , sessErrNoBehaviour = S.empty
      }
  )

dupLabel :: (Text, SessDefErr)
dupLabel = (pack [raw|
  rec X. &[ a=> <X>
          , b=> ?[Int]
          , c=> +[ a=>
                 , a=>
                 , b=>
                 ]
          , c=>
          ]
|], SessDefErr
      { sessErrDupInd = S.empty
      , sessErrDupLab = S.fromList ["a","c"]
      , sessErrUnused = S.empty
      , sessErrDualRec = S.empty
      , sessErrNoBehaviour = S.empty
      }
  )

unusedRecVar :: (Text, SessDefErr)
unusedRecVar = (pack [raw|
  rec X. &[ a=> rec Y. <Z>
          , b=> ?[Int]
          , c=> +[ a=>
                 , b=>
                 ]
          ]
|], SessDefErr
      { sessErrDupInd = S.empty
      , sessErrDupLab = S.empty
      , sessErrUnused = S.fromList ["X", "Y"]
      , sessErrDualRec = S.empty
      , sessErrNoBehaviour = S.empty
      }
  )

dualRecVar :: (Text, SessDefErr)
dualRecVar = (pack [raw|
  rec X. ?[Void] <dual X>
|], SessDefErr
      { sessErrDupInd = S.empty
      , sessErrDupLab = S.empty
      , sessErrUnused = S.empty
      , sessErrDualRec = S.fromList ["X"]
      , sessErrNoBehaviour = S.empty
      }
  )

noSessBehaviour :: (Text, SessDefErr)
noSessBehaviour = (pack [raw|
  rec X. &[ a => rec Y. <Y>
          , b => rec Z. <Z>
          , c => <X>
          ]
|], SessDefErr
      { sessErrDupInd = S.empty
      , sessErrDupLab = S.empty
      , sessErrUnused = S.empty
      , sessErrDualRec = S.empty
      , sessErrNoBehaviour = S.fromList ["Y", "Z"]
      }
  )

