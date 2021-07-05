{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Jael.TypeSpec where

import Test.Hspec

import Jael.Classes
import Jael.Prelude   hiding (toSet)
import Jael.Type
import Test.Jael.Util

spec :: Spec
spec =
  describe "Free type variables" $ do
    it "should include type vars" $
      ftv (tVar "x") `shouldBe` toSet ["x"]
    it "should exclude bound type vars" $
      ftv (tAll "x" (tVar "x")) `shouldBe` toSet []
    it "should include row vars" $
      ftv (tRec (rVar "r")) `shouldBe` toSet ["r"]
    it "should exclude bound row vars" $
      ftv (tAll "r" (tRec (rExt ("foo", tInt) (rVar "r")))) `shouldBe` toSet []
    it "should exclude bound row vars but not unbound type vars" $
      ftv (tAll "r" (tRec (rExt ("foo", tVar "t") (rVar "r")))) `shouldBe` toSet ["t"]
    it "should include type vars in rows" $
      ftv (tRec (rExt ("foo", tVar "t") rEmpty)) `shouldBe` toSet ["t"]
    it "should include type vars and row vars in rows" $
      ftv (tRec (rExt ("foo", tVar "t") (rVar "r"))) `shouldBe` toSet ["t", "r"]
    it "should work on rows (empty)" $
      ftv rEmpty `shouldBe` toSet []
    it "should work on rows (variables)" $
      ftv (rVar "r") `shouldBe` toSet ["r"]
    it "should work on rows (extended)" $
      ftv (rExt ("foo", tVar "t") rEmpty) `shouldBe` toSet ["t"]
    it "should work on rows (extended with variable)" $
      ftv (rExt ("foo", tVar "t") (rVar "r")) `shouldBe` toSet ["t", "r"]
