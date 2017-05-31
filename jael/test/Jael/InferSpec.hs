{-# Language OverloadedStrings #-}

module Jael.InferSpec (spec) where

import           Jael.Test.Util
import           Test.Hspec

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Language.Fixpoint.Types as F

import           Jael.Types
import           Jael.Util

spec :: Spec
spec = do
  describe "Inference" $ do
    it "shouldn't affect quals" pending
--      let p = "\\(x:{v:Int|v>0}) { x+1 }" :: T.Text
--      let (t1 :< EAbsF i1
--            (t2 :< EAppF
--              (t3 :< EAppF
--                (t4 :< EConF CAdd)
--                (_ :< EVarF i2)
--              )
--              (t6 :< EConF c1)
--            )) = toHM p
--      let x = toTE p
--      let y = (fmap noQual t1) :< EAbsF i1
--                (fmap noQual t2 :< EAppF
--                  (fmap noQual t3 :< EAppF
--                    (fmap noQual t4 :< EConF CAdd)
--                    (Scheme S.empty M.empty
--                      (Just (F.reft (F.symbol ("v"::T.Text))
--                                    (F.PAtom F.Gt
--                                      (F.eVar ("v"::T.Text))
--                                      (F.expr (0::Integer))
--                                    ))
--                       :< TIntF)
--                    :< EVarF i2)
--                  )
--                  (fmap noQual t6 :< EConF c1)
--                )
--      x `shouldBe` y

    it "should insert annotations for liquid type checking" pending
--      let p = "\\(x)\
--              \{ id = \\(y) {y}\
--              \; id(x)+id(1)\
--              \}" :: T.Text
--      let (pt :< (EAbsF x (let_t :< ELetF _ (id_t :< _) (in_t :< _)))) = toTE p
--      -- id_t should be a->a
--      let (Scheme gen _ (TFun _ (TVar id_t1) (TVar id_t2))) = shape id_t
--      let [v] = S.toList gen
--      shape pt `shouldBe` (Scheme S.empty M.empty $ TFun x TInt TInt)
--      id_t1 `shouldBe` id_t2
--      value id_t1 `shouldBe` v
--      shape let_t `shouldBe` shape in_t
