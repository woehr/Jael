{-# Language OverloadedStrings #-}

module ParserSpec where

import           Jael.Grammar.Parser
import           Jael.Grammar.Monad
--import           Jael.Grammar.Token
import           Jael.Grammar.AST               ( JaelExpr )

import           Test.Hspec
import           Data.TreeDiff

import qualified Data.ByteString.Lazy          as BSL

import           Control.Monad                  ( forM_ )
import           System.FilePattern.Directory   ( getDirectoryFiles )
import           Control.Exception              ( try )
import           System.IO.Error                ( isDoesNotExistError )
import           System.FilePattern             ( match )
import           Data.TreeDiff.Golden           ( ediffGolden )


parse :: BSL.ByteString -> JaelExpr
parse s = case runParseMonad s pProg of
  Left  x -> error $ "\"" <> show x <> "\""
  Right x -> x

goldenTest
  :: (  String
     -> IO Expr
     -> IO Expr
     -> (Expr -> Expr -> IO (Maybe String))
     -> (Expr -> IO ())
     -> Spec
     )
goldenTest name getFixtureValue getTestValue cmp updateFixture = do
  mExpected <- runIO $ try getFixtureValue
  actual    <- runIO getTestValue
  it name $ case mExpected of
    Left e | isDoesNotExistError e ->
      updateFixture actual
        >> expectationFailure
             "Test output file created because it did not exist. Please verify its content."
    Left  e        -> expectationFailure $ "Unexpected exception: " <> show e
    Right expected -> do
      result <- cmp expected actual
      maybe (return ()) expectationFailure result

spec :: Spec
spec = do
  describe "parser" $ do
    context "expressions" $ do
      exprFixtures <- runIO $ getDirectoryFiles "golden" ["expr.*.in"]
      forM_ exprFixtures $ \filename -> do
        let
          testName = case match "expr.*.in" filename of
            Just [x] -> x
            _        -> error
              "Expect match to succeed on all results of getDirectoryFiles"

        let goldFile = "golden/expr." <> testName <> ".out"
        ediffGolden goldenTest
                    testName
                    goldFile
                    (parse <$> BSL.readFile ("golden/" <> filename))
