{-# Language OverloadedStrings #-}

module ParserSpec where

import           Jael.Grammar.AST               ( JaelExpr )
import           Jael.Grammar.Monad
import           Jael.Grammar.Parser

import           Test.Hspec

import qualified Data.ByteString               as BS
import qualified Data.TreeDiff                 as TD

import           Control.Exception              ( try )
import           Control.Monad                  ( forM_ )
import           Data.TreeDiff.Golden           ( ediffGolden )
import           System.FilePattern             ( match )
import           System.FilePattern.Directory   ( getDirectoryFiles )
import           System.IO.Error                ( isDoesNotExistError )

exprParser :: BS.ByteString -> JaelExpr
exprParser s = case runParseMonad s pProg of
  Left  x -> error $ "\"" <> show x <> "\""
  Right x -> x

goldenTest
  :: (  String
     -> IO TD.Expr
     -> IO TD.Expr
     -> (TD.Expr -> TD.Expr -> IO (Maybe String))
     -> (TD.Expr -> IO ())
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

goldenSuite
  :: (Eq a, TD.ToExpr a)
  => String
  -> String
  -> (BS.ByteString -> a)
  -> SpecWith ()
goldenSuite ctx prefix parser = context ctx $ do
  exprFixtures <- runIO $ getDirectoryFiles "golden" [prefix <> ".*.in"]
  forM_ exprFixtures $ \filename -> do
    let
      testName = case match (prefix <> ".*.in") filename of
        Just [x] -> x
        _ ->
          error "Expect match to succeed on all results of getDirectoryFiles"

    let goldFile = "golden/" <> prefix <> "." <> testName <> ".out"
    ediffGolden goldenTest
                testName
                goldFile
                (parser <$> BS.readFile ("golden/" <> filename))

spec :: Spec
spec = do
  describe "parser" $ do
    goldenSuite "expressions" "expr" exprParser
