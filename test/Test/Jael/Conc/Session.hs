{-# Language NoImplicitPrelude, QuasiQuotes #-}

module Test.Jael.Conc.Session
( sessionTests
) where

import ClassyPrelude
import qualified Data.Set as S
import Jael.Grammar
import Jael.Parser
import Jael.Conc.Session
import Jael.Seq.Types
import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Jael.Util

sessionTests :: [T.Test]
sessionTests =
  [ testCase "test all session def errors" $ checkSessionErr allErrs
  , testCase "test session dual function" $ checkSessionDual testDual
  ]

checkSessionErr :: (Text, SessDefErr) -> Assertion
checkSessionErr (t, SessDefErr
                     { sessErrDupInd = exDupInd
                     , sessErrDupLab = exDupLab
                     , sessErrUnused = exUnused
                     }) =
  case runParser pGSession t of
       Left err -> assertFailure (show err)
       Right gDef ->
         case validateSession (gToSession gDef) of
              Just (SessDefErr { sessErrDupInd = dupInd
                               , sessErrDupLab = dupLab
                               , sessErrUnused = unused
                               })-> do
                   assertEqual "" exDupInd dupInd
                   assertEqual "" exDupLab dupLab
                   assertEqual "" exUnused unused
              Nothing -> assertFailure "Expected session def error"

checkSessionDual :: (Text, Session) -> Assertion
checkSessionDual = checkParsedTree (liftM (dual . gToSession) . pGSession)

allErrs :: (Text, SessDefErr)
allErrs = (pack [raw|
  rec X. &[ a=>rec X. <Y>
          , b=>?[Int];
          , c=> +[ a=>;
                 , a=>;
                 , b=>;
                 ]
          , c=>;
          ]
|], SessDefErr
      { sessErrDupInd = S.fromList ["X"]
      , sessErrDupLab = S.fromList ["a","c"]
      , sessErrUnused = S.fromList ["X"]
      }
  )

-- A session to be parsed and dualed, and the expected dual
testDual :: (Text, Session)
testDual = (pack [raw|
  ![Int]
  ?[ ![Int] ?[Int] ; ]
  +[ a => ;
   , b => ![Int] ;
   , c => ?[ ![Int] ?[Int] ; ] ;
   , d => &[ a => ;
           , b => ![Int] ;
           , c => ?[ ![Int] ?[Int] ; ] ;
           ]
   , e => rec X. ?[Bool] ![Int] <X>
   ]
|], SGetTy TInt
  $ SPutSess (SPutTy TInt $ SGetTy TInt $ SEnd)
  $ SChoice [ ("a", SEnd)
            , ("b", SGetTy TInt SEnd)
            , ("c", SPutSess (SPutTy TInt $ SGetTy TInt $ SEnd) SEnd)
            , ("d", SSelect [ ("a", SEnd)
                            , ("b", SGetTy TInt SEnd)
                            , ("c", SPutSess (SPutTy TInt $ SGetTy TInt $ SEnd) SEnd)
                            ]
              )
            , ("e", SCoInd "X" $ SPutTy TBool $ SGetTy TInt $ SIndVar "X")
            ]
  )

