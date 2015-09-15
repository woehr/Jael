{-# Language NoImplicitPrelude, QuasiQuotes #-}

module Test.Jael.Hw.Area
( hwAreaTests
) where

import ClassyPrelude
import qualified Data.Map as M
import Jael.Grammar
import Jael.Hw.Area
import Jael.Seq.Types
import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Jael.Util

hwAreaTests :: [T.Test]
hwAreaTests =
  [ testCase "polymorphic area" $ checkHwAreaErr polyArea
  , testCase "area location" $ checkHwArea locationTest
  ]

checkHwAreaErr :: (Text, HwAreaErr) -> Assertion
checkHwAreaErr (t, e) = undefined

checkHwArea :: (Text, M.Map Text (Integer, Ty)) -> Assertion
checkHwArea (t, vs) = undefined

polyArea :: (Text, HwAreaErr)
polyArea = (pack [raw|
|], PolyArea
  )

locationTest :: (Text, M.Map Text (Integer, Ty))
locationTest = (pack [raw|
|], M.fromList
  [ ("Area::a", (0x4000, TInt))
  ])

