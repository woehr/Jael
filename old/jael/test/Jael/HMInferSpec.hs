{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Jael.HMInferSpec where

import qualified Data.ByteString.Char8         as BS
import           Data.FileEmbed
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import           Data.TreeDiff
import           Test.Hspec

import           Jael.AST
--import Jael.Classes
import           Jael.DataDecl
import           Jael.Expr
import           Jael.HMInfer
import           Jael.Pattern
import           Jael.Prelude                      hiding ( exp )
import           Jael.QType
import           Jael.Type
import           Test.Jael.Util

checkRepeated :: [(T.Text, b)] -> String -> ([(T.Text, b)] -> c) -> c
checkRepeated xs n f =
  let repeated = repeats (fmap fst xs)
  in  if not (null repeated)
        then error $ "Repeated " <> n <> ":\n" <> pShow repeated
        else f xs

getTestData :: (M.Map T.Text Type', [(ParsePattern, E')], M.Map T.Text Type')
getTestData =
  let
    inp
      = $(embedOneFileOf ["test/test-inputs/infer-pass.j", "test-inputs/infer-pass.j"])
    x        = parseToAST (BS.unpack inp)
    dataDefs = fmap (fmap (fmap unrefined)) (_astDataDef x)
    parseTs  = fmap (bimap toText unrefined) (_astTypeConstraint x)
    pe's =
      second
          (overExpr (id :: Type' -> Type')
                    (expandOr :: ParsePattern -> [ParsePattern'])
                    (id :: T.Text -> T.Text)
          )
        <$> _astExpr x

    expectedTypeMap = checkRepeated parseTs "types" M.fromList
    consList        = checkRepeated dataDefs
                                    "data definitions"
                                    (concatMap dataConTypes . fmap snd)
    infEnv = checkRepeated consList "constructors" M.fromList
  in
    (infEnv, pe's, expectedTypeMap)

checkInferred :: M.Map T.Text Type'
              -> ParsePattern
              -> E'
              -> M.Map T.Text Type'
              -> Spec
checkInferred env p e expected = do
  let (t, _) =
        either (error . ("Type inference error:\n" <>) . pShow) id (infer env e)
  let v = case p of
        PVar v' -> v'
        _       -> error "Expected patterns with a single variable."
  let expectedT = unsafeLookup v expected
  it ("Should infer: " <> T.unpack v <> " : " <> show expectedT) $ do
      -- We're checking for alpha equivalence
    let msubs = mkSub expectedT t
    -- If mkSub returned Nothing the structure of the two types don't match
    when (isNothing msubs)
         (expectationFailure . show . prettyEditExpr $ ediff t expectedT)
    let (tsub, rsub) = fromMaybe (error "") msubs
    let sub = M.map (Left . TVar) tsub `M.union` M.map (Right . TRowVar) rsub
    -- Check the abstracted variables separately because they might not be
    -- in the same order
    let (t'vs, t')   = schemeVars t
    let (eT'vs, eT') = schemeVars (apply sub expectedT)
    -- Check the abstracted variables
    sortRecords t' `shouldBe` sortRecords eT'
    -- Check the (non-scheme) types
    S.fromList t'vs `shouldBe` S.fromList eT'vs

testSub :: TypeSub
testSub = M.fromList
  [ ("b", Left tBool)
  , ("v", Left (TVar ("x" :: T.Text)))
  , ("e", Right TRowEmpty)
  ]

spec :: Spec
spec = do
  let (env, pe's, expected) = getTestData

  describe "Apply substitution tests" $ do

    it "should sub tvars" $ apply testSub (tVar "b") `shouldBe` tBool

    it "should sub rows" $ apply testSub (rVar "e") `shouldBe` rEmpty

    it "should apply to type subs"
      $          apply
                   testSub
                   (M.fromList [("x", Left (tVar "b")), ("y", Right (rVar "e"))] :: TypeSub
                   )
      `shouldBe` M.fromList [("x", Left tBool), ("y", Right rEmpty)]

    it "should sub tvars in typed expressions" $ do
      let
        pat, pat' :: TypedPats
        pat  = [pVar ("p1", tVar "b")]
        pat' = [pVar ("p1", tBool)]

        typ, typ' :: Type'
        typ  = TRec (rExt ("foo", tVar "b") (rVar "e"))
        typ' = TRec (rExt ("foo", tBool) rEmpty)

        exp :: TypedPats -> Type' -> TypedExpr
        exp p t =
          ELetT [((p, t), EVar @T.Text "foo")] (ETApp t (EVar @T.Text "bar"))
      apply testSub (exp pat typ) `shouldBe` exp pat' typ'

    it "should apply to quantifiers"
      $
      -- v -> TVar x => forall v. v -> forall x. x
                 apply testSub (TAll @T.Text "v" (tVar "v"))
      `shouldBe` TAll @T.Text "x" (tVar "x")

  describe "Jael HM inference tests" $ forM_ pe's $ \(p, e) ->
    checkInferred env p e expected
