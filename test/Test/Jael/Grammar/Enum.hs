{-# Language NoImplicitPrelude, QuasiQuotes #-}

module Test.Jael.Grammar.Enum
( gEnumTests
) where

import ClassyPrelude
import Jael.Grammar
import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.Jael.Util

gEnumTests :: [T.Test]
gEnumTests =
  [ testCase "one element enum, no type" $ checkParsedTree pGTypeDef tagNoType
  , testCase "one element enum, with type" $ checkParsedTree pGTypeDef tagWithType
  ]

tagNoType :: (Text, GTypeDef)
tagNoType = (pack [raw|
  enum X { a }
|], GTDefEnum (UIdent "X") $ GTEnumDef
              []
              [ GTEnumElemNoTy (LIdent "a")
              ]
  )

tagWithType :: (Text, GTypeDef)
tagWithType = (pack [raw|
  enum X { a Int }
|], GTDefEnum (UIdent "X") $ GTEnumDef
              []
              [ GTEnumElemWithTy (LIdent "a") GTInt
              ]
  )

