{-# Language NoImplicitPrelude #-}
{-# Language OverloadedStrings #-}

module Jael.ConsGenSpec (spec) where

import           Jael.Prelude
import           Test.Hspec
import           Jael.Test.Util

import qualified Data.Text as T
import qualified Language.Fixpoint.Types as F
--import qualified Text.PrettyPrint.Leijen.Text as P

import           Jael.ConsGen

spec :: Spec
spec = do
  describe "Liquid constraint generation" $ do
    it "generates constrains" $ do
      let p = "{ f=\\(b:{v:Int|v>0}){b}; f(1) }" :: T.Text
      --let p = "{a=5; a:{v:Int|v>=5}+2}" :: T.Text
      let te = toTE p
--      let (tmplt, be, cons) = runConsGen te
--      traceM . show $ tmplt
--      traceM . show . F.toFix $ be
--      traceM . show . F.toFix . wfcs $ cons
--      traceM . show . F.toFix . subcs $ cons
      solve te
      pending

  describe "QType -> F.Sort" $ do
    it "converts QTypes to fixpoint sorts" $ do
      let p ="\\(a, b) { (a, b) }" :: T.Text
      let te = toTE p
      let sortedExpr = fmap toSort te
      --traceM . show . fmap F.toFix $ sortedExpr
      extract sortedExpr `shouldBe`
        F.FAbs 0 (F.FAbs 1
          (F.FFunc (F.FVar 0)
                   (F.FFunc
                    (F.FVar 1)
                    (F.FApp (F.FApp
                      (F.FTC $ F.symbolFTycon (F.dummyLoc "Tup2"))
                      (F.FVar 0))
                      (F.FVar 1))
                   )
          )
        )
