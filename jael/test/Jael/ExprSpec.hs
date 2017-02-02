{-# Language
  NoImplicitPrelude
, OverloadedStrings #-}

module Jael.ExprSpec (spec) where

import BasePrelude
import Test.Hspec

import Jael.Expr

spec :: Spec
spec = do
  describe "type inference" $ do
    it "infers application" $ do
      pending
    it "infers abstraction" $ do
      pending
    it "infers variables" $ do
      pending
    it "infers tuples" $ do
      pending
    it "infers named types" $ do
      pending
    it "infers lets" $ do
      pending
    it "infers if-then-else" $ do
      pending
    it "leaves qualifications in place" $ do
      pending
    describe "forbids name shadowing" $ do
      it "for let exprs" $ do
        pending
      it "for lambdas" $ do
        pending
    describe "checks annotated types" $ do
      it "fails when they mismatch" $ do
        pending
      it "succeeds when the match" $ do
        pending
