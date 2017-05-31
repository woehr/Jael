{-# Language OverloadedStrings #-}
{-# Language TupleSections #-}

module Jael.ConsGenSpec (spec) where

import           Test.Hspec
import           Jael.Test.Util

--import qualified Data.Set as S
import qualified Data.Text as T
import qualified Language.Fixpoint.Types as F
--import qualified Text.PrettyPrint.Leijen.Text as P

import           Jael.ConsGen

liquidSafe :: T.Text -> Expectation
liquidSafe t = do
  (r, _) <- solve (toHM t, toMTE t)
  r `shouldBe` F.Safe

liquidUnsafe :: T.Text -> Expectation
liquidUnsafe t = do
  (r, _) <- solve (toHM t, toMTE t)
  case r of
    (F.Unsafe _) -> return ()
    _ -> expectationFailure $ "Result not Unsafe:\n\n" ++ T.unpack t

--      let p = "{a=5; (a+2):{v:Int|v>=5}}" :: T.Text
--      let p = "{a=0; a+1}" :: T.Text

spec :: Spec
spec = do
  describe "Liquid type solutions tests" $ do
    it "against examples from the Liquid Types tutorial" $ do
      liquidSafe   "(true : {v: Bool | v})"
      liquidUnsafe "(true : {v: Bool | !v})"

      liquidSafe   "(false : {v: Bool | !v })"
      liquidUnsafe "(false : {v: Bool | v })"

      liquidSafe   "{ f = \\(bat) -> bat || !bat;\
                   \( f : x:Bool -> {v:Bool | true} )\
                   \}"

      liquidSafe   "{ f = \\(bob) -> bob && !bob;\
                   \( f : x:Bool -> {v:Bool | false} )\
                   \}"

      liquidSafe   "((\\(a, b) -> a && b --> a) : x:Bool -> y:Bool -> {v: Bool | v == true})"

--      liquidSafe   "{ f = \\(a, b) -> a && b --> a;\
--                   \( f : x:Bool -> y:Bool -> {v: Bool | v == true} )\
--                   \}"

    it "for abstractions" $ do
      liquidSafe "{ f=\\(b:{v:Int|v>0}) -> b; f(1) }"

  -- describe "QType -> F.Sort" $ do
  --   it "converts QTypes to fixpoint sorts" $ do
  --     let p ="\\(a, b) { (a, b) }" :: T.Text
  --     let te = toTE p
  --     let sortedExpr = fmap toSort te
  --     --traceM . show . fmap F.toFix $ sortedExpr
  --     extract sortedExpr `shouldBe`
  --       F.FAbs 0 (F.FAbs 1
  --         (F.FFunc (F.FVar 0)
  --                  (F.FFunc
  --                   (F.FVar 1)
  --                   (F.FApp (F.FApp
  --                     (F.FTC $ F.symbolFTycon (F.dummyLoc "Tup2"))
  --                     (F.FVar 0))
  --                     (F.FVar 1))
  --                  )
  --         )
  --       )
