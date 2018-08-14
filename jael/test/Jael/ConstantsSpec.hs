{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Jael.ConstantsSpec where

import qualified Data.Text as T

import Jael.AST
import Jael.Constants
import Jael.Expr
import Jael.Prelude
import Jael.QType
import Jael.Type

import Test.Hspec

type SpecQType = ParseType

spec :: Spec
spec = describe "Jael constants' in AST form" $ do
  it "should make int constants" $
    intConst 10 `shouldBe`
      QualType
        ( "v"
        , EApp (EPrim OpEq)
            [ EVar @T.Text "v"
            , ELit (LInt (defaultInt 10))
            ]
        )
        (TConF @T.Text "Int" [])
  it "should make bool constants" $
    boolConst True `shouldBe`
      QualType
        ( "v"
        , EApp (EPrim OpIff)
            [ EVar @T.Text "v"
            , EVar @T.Text "true"
            ]
        )
        (TConF @T.Text "Bool" [])
