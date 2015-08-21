{-# Language NoImplicitPrelude, QuasiQuotes #-}

module Test.Jael.Grammar.Struct
( gStructTests
) where

import ClassyPrelude
import Jael.Grammar
import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.Jael.Util

gStructTests :: [T.Test]
gStructTests =
  [ testCase "simple monomorphic" $ checkParsedTree pGTStructDef simpleMono 
  , testCase "simple polymorphic" $ checkParsedTree pGTStructDef simplePoly
  ]

simpleMono :: (Text, GTStructDef)
simpleMono = (pack [raw|
  X { f0 :: Int }
|], GTStructDef (UIdent "X")
                []
                [ GTStructElement (GTStructFieldName (LIdent "f0"))
                                  GTInt
                ]
  )

simplePoly :: (Text, GTStructDef)
simplePoly = (pack [raw|
  X a { f0 :: a }
|], GTStructDef (UIdent "X")
                [ GTVars (LIdent "a")
                ]
                [ GTStructElement (GTStructFieldName (LIdent "f0"))
                                  (GTTVar (LIdent "a"))
                ]
  )

