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
  [ testCase "one element enum, no type" $ checkParsedTree pGTEnumDef tagNoType
  , testCase "one element enum, with type" $ checkParsedTree pGTEnumDef tagWithType
  ]

tagNoType :: (Text, GTEnumDef)
tagNoType = (pack [raw|
  X { a }
|], GTEnumDef (UIdent "X") [] $ map (uncurry GTEnumElement)
                                  [ (LIdent "a", [])
                                  ]
  )

tagWithType :: (Text, GTEnumDef)
tagWithType = (pack [raw|
  X { a Int }
|], GTEnumDef (UIdent "X") [] $ map (uncurry GTEnumElement)
                                  [ (LIdent "a", [GTEnumType GTInt])
                                  ]
  )

