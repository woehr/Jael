{-# Language NoImplicitPrelude, QuasiQuotes #-}

module Test.Jael.Hw.Seq
( hwSeqTests
) where

import ClassyPrelude
import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Grammar
import Jael.Hw.Seq
import Jael.Seq.AST
import Jael.Seq.Types
import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Jael.Util

hwSeqTests :: [T.Test]
hwSeqTests =
  [ testCase "re-used variable name" $ checkHwSeqErr reusedVar
  , testCase "valid" $ checkHwSeqInf hwSeqValid
  ]

-- The following variables are used for the purposes of inference
testVars :: M.Map Text Ty
testVars = M.fromList
  [ ("state", TInt)
  ]

checkHwSeqErr :: (Text, HwSeqErr) -> Assertion
checkHwSeqErr (t, e) = undefined

checkHwSeqInf :: (Text, HwEx) -> Assertion
checkHwSeqInf (t, es) = undefined

reusedVar :: (Text, HwSeqErr)
reusedVar = (pack [raw|
|], ReusedVars (S.fromList ["x"])
  )

hwSeqValid :: (Text, HwEx)
hwSeqValid = (pack [raw|
|], undefined
  )

