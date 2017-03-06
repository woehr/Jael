{-# Language NoImplicitPrelude #-}
{-# Language OverloadedStrings #-}

module Jael.UniqifySpec (spec) where

import           Jael.Prelude
import           Test.Hspec
import           Jael.Test.Util

import qualified Data.Map as M

import           Jael.Uniqify

spec :: Spec
spec = do
  describe "Uniqify makes value variables and program variables unique" $ do
    it "can allow shadowing of program vairables" $ do
      let (m1, x) = uniqifyVars $ toTE "\\(x,y) { x:{v:Int|v==0} }"
      let y = toTE "\\(a,b) { a:{v0:Int|v0==0} }"
--      traceM . show . P.pretty $ x
--      putStrLn "\n\n"
--      traceM . show . P.pretty $ y
--      putStrLn "\n\n"
--      traceM . concat . intersperse "\n\n" . dispSubExprs $ x
--      putStrLn "\n\n"
--      traceM . concat . intersperse "\n\n" . dispSubExprs $ y
      x `shouldBe` y
      m1 `shouldBe` M.fromList [("a", "x"), ("b", "y")]
