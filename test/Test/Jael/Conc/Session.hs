{-# Language NoImplicitPrelude, QuasiQuotes #-}

module Test.Jael.Conc.Session
( sessionTests
) where

import ClassyPrelude
import qualified Data.Set as S
import Jael.Grammar
import Jael.Parser
import Jael.Conc.Session
import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Jael.Util

sessionTests :: [T.Test]
sessionTests =
  [ testCase "test all session def errors" $ checkSession allErrs
  ]

checkSession :: (Text, SessDefErr) -> Assertion
checkSession (t, SessDefErr
                  { sessErrDupInd = exDupInd
                  , sessErrDupLab = exDupLab
                  , sessErrFree   = exFree
                  , sessErrUnused = exUnused
                  }) =
  case runParser pGSession t of
       Left err -> assertFailure (show err)
       Right gDef ->
         case validateSession (gToSession gDef) of
              Just (SessDefErr { sessErrDupInd = dupInd
                               , sessErrDupLab = dupLab
                               , sessErrFree   = free
                               , sessErrUnused = unused
                               })-> do
                   assertEqual "" exDupInd dupInd
                   assertEqual "" exDupLab dupLab
                   assertEqual "" exFree free
                   assertEqual "" exUnused unused
              Nothing -> assertFailure "Expected session def error"

allErrs :: (Text, SessDefErr)
allErrs = (pack [raw|
  rec X. &[ a=>rec X. <Y>
          , b=>?[Int]end
          , c=> +[ a=>end
                 , a=>end
                 , b=>end
                 ] end
          , c=>end
          ] end
|], SessDefErr
      { sessErrDupInd = S.fromList ["X"]
      , sessErrDupLab = S.fromList ["a","c"]
      , sessErrFree   = S.fromList ["Y"]
      , sessErrUnused = S.fromList ["X"]
      }
  )

