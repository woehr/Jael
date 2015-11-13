module Test.Jael.Grammar.Struct
( gStructTests
) where

import           Jael.Grammar
import qualified Test.Framework as T
import           Test.Jael.Util

gStructTests :: [T.Test]
gStructTests =
  [ testCase "simple monomorphic" $ checkParsedTree pGTypeDef simpleMono
  , testCase "simple polymorphic" $ checkParsedTree pGTypeDef simplePoly
  ]

simpleMono :: (Text, GTypeDef)
simpleMono = (pack [raw|
  struct X { f0 : Int }
|], GTDefStruct (UIdent "X") $ GTStructDef
                []
                [ GTStructElement (GTStructFieldName (LIdent "f0"))
                                  GTInt
                ]
  )

simplePoly :: (Text, GTypeDef)
simplePoly = (pack [raw|
  struct X a { f0 : a }
|], GTDefStruct (UIdent "X") $ GTStructDef
                [ GTVars (LIdent "a")
                ]
                [ GTStructElement (GTStructFieldName (LIdent "f0"))
                                  (GTTVar (LIdent "a"))
                ]
  )

