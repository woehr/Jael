{-# Language NoImplicitPrelude #-}
{-# Language OverloadedStrings #-}

module Jael.InferSpec (spec) where

import           Jael.Prelude
import           Jael.Test.Util
import           Test.Hspec

import qualified Data.Text as T
import qualified Language.Fixpoint.Types as F
--import qualified Text.PrettyPrint.Leijen.Text as P

import           Jael.Types
import           Jael.Util

spec :: Spec
spec = do
  describe "Inference" $ do
    it "shouldn't affect quals" $ do
      let p = "\\(x:{v:Int|v>0}) { x+1 }" :: T.Text
      let (t1 :< EAbsF i1
            (t2 :< EAppF
              (t3 :< EAppF
                (t4 :< EConF CAdd)
                (_ :< EVarF i2)
              )
              (t6 :< EConF c1)  -- 
            )) = toHM p
      let x = toTE p
      let y = (noQual t1) :< EAbsF i1
                (noQual t2 :< EAppF
                  (noQual t3 :< EAppF
                    (noQual t4 :< EConF CAdd)
                    ((F.reft (F.symbol ("v"::T.Text))
                                 (F.PAnd
                                   [F.PAtom F.Gt
                                     (F.eVar ("v"::T.Text))
                                     (F.expr (0::Integer))
                                   ])
                      :< TIntF)
                    :< EVarF i2)
                  )
                  (noQual t6 :< EConF c1)
                )
      x `shouldBe` y

    it "should insert annotations for liquid type checking" $ do
      let p = "\\(x)\
              \{ id = \\(y) {y}\
              \; id(x)+id(1)\
              \}" :: T.Text
      let (pt :< (EAbsF x (let_t :< ELetF _ (id_t :< _) (in_t :< _)))) = toTE p
      -- id_t should be a->a
      let (TGen [v] (TFun _ (TVar id_t1) (TVar id_t2))) = shape id_t
--      traceM (show . P.pretty $ pt)
--      traceM (show . P.pretty $ id_t)
--      traceM (show . P.pretty $ in_t)
      shape pt `shouldBe` TFun x TInt TInt
      id_t1 `shouldBe` id_t2
      value id_t1 `shouldBe` v
      shape let_t `shouldBe` shape in_t
