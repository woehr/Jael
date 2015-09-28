{-# Language NoImplicitPrelude, QuasiQuotes #-}

module Test.Jael.Hw.Area
( hwAreaTests
) where

import ClassyPrelude
import qualified Data.Map as M
import Jael.Grammar
import Jael.Parser
import Jael.Seq.Types
import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Jael.Util

hwAreaTests :: [T.Test]
hwAreaTests =
  [ testCase "area locations" $ checkHwArea locationTest
  ]

checkHwArea :: (Text, M.Map Text (Integer, Ty)) -> Assertion
checkHwArea (t, vs) =
  case runParser pGTypeDef t of
    Left err -> assertFailure (unpack err)
    Right g  -> assertFailure $ "Unimplemented.\nParsed: " ++ show g
                                           ++ "\nExpect: " ++ show vs

locationTest :: (Text, M.Map Text (Integer, Ty))
locationTest = (pack [raw|
  area Area @ 0x4000
  { [pad 8]
    x :: Int
  , y :: Bool
  , [align 4]
    z :: Int
  }
|], M.fromList
  [ ("area",    (0x4000, TNamed "Area" []))
  , ("area::x", (0x4008, TInt))
  , ("area::y", (0x400C, TBool))
  , ("area::z", (0x4010, TInt))
  ])

