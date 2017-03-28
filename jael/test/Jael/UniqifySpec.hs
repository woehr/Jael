{-# Language NoImplicitPrelude #-}
{-# Language OverloadedStrings #-}
{-# Language TupleSections #-}

module Jael.UniqifySpec (spec) where

import           Jael.Prelude
import           Test.Hspec
import           Jael.Test.Util

import qualified Control.Comonad.Trans.Cofree as C
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Language.Fixpoint.Types as F
--import qualified Text.PrettyPrint.Leijen.Text as P

import           Jael.Types
import           Jael.Uniqify
import           Jael.Util

spec :: Spec
spec = do
  describe "UniqifySpec test utilities" $ do
    it "has a function to rename binds in function types" $ do
      let t  = toQT "xx:Int -> yy:{v:Int|true} -> a"
      let t' = toQT "b0:Int -> b1:{v:Int|true} -> a"
      let tRebound = rebind t
      tRebound `shouldBe` t'

  describe "Uniqify makes value variables and program variables unique" $ do
    it "works with unused variables" $ do
      let (m1, x) = uniqifyExpr $ toTE "\\(x,y,z) { x:{v:Int|v==0} }"
--      let (t1 :< EAbsF n1 (t2 :< EAbsF n2 (t3 :< EAbsF n3 e))) =
      let te = toTE "\\(a,b,c) { a:{v0:Int|v0==0} }"
      let y = fmap (fmap rebind) te
      x `shouldBe` y
      m1 `shouldBe` M.fromList [("a","x"),("b","y"),("b0","x"),("b1","y"),("b2","z"),("c","z")]

    it "works with shadowed variables" $ do
      let (m1, x) = uniqifyExpr $ toTE "\\(x){y=x+1;f=\\(y){y+1};f(y)}"
      let te = toTE "\\(a){b=a+1;c=\\(d){d+1};c(b)}"
      let y = fmap (fmap rebind) te
      x `shouldBe` y
      m1 `shouldBe` M.fromList
        [("a","x"),("b","y"),("b0","x"),("b1","b1"),("c","f"),("d","y")]
    it "works with type annotations" $ do
      let x = toTE "\\(a){a} : v:Int -> Int"
      let (_, y) = uniqifyExpr x
      pending
      x `shouldBe` y

-- Renames the first function bind to b0, and each subsequent bind
-- to b1, b2, b3, ...
rebind :: QType -> QType
rebind = ana alg . (, 0)
  where alg :: (QType, Integer) -> C.CofreeF TypeF F.Reft (QType, Integer)
        alg (r :< (TFunF b t1 t2), n) =
          r C.:< TFunF b{value=T.pack $ "b"++show n} (t1, n+1) (t2, n+1)
        alg (r :< t, n) = r C.:< fmap (,n) t
