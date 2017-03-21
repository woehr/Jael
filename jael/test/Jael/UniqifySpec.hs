{-# Language NoImplicitPrelude #-}
{-# Language OverloadedStrings #-}

module Jael.UniqifySpec (spec) where

import           Jael.Prelude
import           Test.Hspec
import           Jael.Test.Util

import qualified Data.Map as M
import qualified Text.PrettyPrint.Leijen.Text as P

import           Jael.Uniqify

spec :: Spec
spec = do
  describe "Uniqify makes value variables and program variables unique" $ do
    it "works with unused variables" $ do
      let (m1, x) = uniqifyExpr $ toTE "\\(x,y,z) { x:{v:Int|v==0} }"
      let y = toTE "\\(a,b,c) { a:{v0:Int|v0==0} }"
      traceM $ show m1
      traceM "\n\n"
      traceM . show . P.pretty $ x
      traceM "\n\n"
      traceM . show . P.pretty $ y
      x `shouldBe` y
      m1 `shouldBe` M.fromList [("a", "x"), ("b", "y")]

    it "works with shadowed variables" $ do
      let (m1, x) = uniqifyExpr $ toTE "\\(x){y=x+1;f=\\(y){y+1};f(y)}"
      let y = toTE "\\(a){b=a+1;c=\\(d){d+1};c(b)}"
      traceM . show $ m1
      traceM "\n\n"
      traceM . show . P.pretty $ x
      traceM "\n\n"
      traceM . show . P.pretty $ y
      x `shouldBe` y
      m1 `shouldBe` M.fromList []
