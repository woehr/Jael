{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE TemplateHaskell #-}

-- This module runs tests found in the comments of the grammar file.
--
-- A test has the form "-- expected rule data"
-- where
--  expected is either == or /= for strings that should and shouldn't parse
--  rule is a grammar non-terminal which indicates the parser to use
--  data is a string to parse
--
-- Note: Using hint seems to be the easiest way to programmatically get a
--       function from its string (in this case the parsing function) without
--       having to manually create some kind of lookup table.

module Main (main) where

import Control.Monad
import Data.Either
import Data.List.Split
import Data.Maybe
import Data.Monoid
import Test.Hspec hiding (runIO)

import Paths_jael_grammar_test

import qualified Data.Map                     as M
import qualified Data.Set                     as S
import qualified Language.Haskell.Interpreter as I

import Jael.Grammar

data ParseResult
  = Pass
  | Fail
  | TokPass
  | TokFail
  | Same
  deriving (Eq, Show)

data InlineTest = InlineTest
  { expectedResult :: ParseResult
  , ruleName       :: String
  , testData       :: String
  , lineNo         :: Int
  } deriving (Show)

type ErrMsg = String
type Ast = String
type ParseFun = String -> Either ErrMsg Ast
type LexFun = String -> Bool

main :: IO ()
main = do
  cfFile <- getDataFileName "Grammar.cf" >>= readFile
  let inlineTests = catMaybes . zipWith lineToTest [1..] . lines $ cfFile
  (ps, ls) <- functionsFromTests inlineTests

  let specFromTest InlineTest{..} = it specDesc theTest
        where
          p = unsafeLookup ruleName ps
          l = unsafeLookup ruleName ls
          specDesc = "(ln:" <> show lineNo <> ") should " <> show expectedResult <> " " <> ruleName <> " \"" <> testData <> "\""
          theTest = case expectedResult of
            Pass -> either expectationFailure (const $ return ()) (p testData)
            Fail -> shouldSatisfy (p testData) isLeft
            Same -> do
              let (r1, r2) | [s1,s2] <- splitOn "===" testData = (p s1, p s2)
                           | otherwise = error "Malformed test data."
              r1 `shouldBe` r2
              shouldSatisfy r1 isRight
            TokPass -> testData `shouldSatisfy` l
            TokFail -> testData `shouldNotSatisfy` l

  hspec $
    describe "Inline Grammar.cf tests:" $
      forM_ inlineTests specFromTest

unsafeLookup :: (Ord k, Show k) => k -> M.Map k a -> a
unsafeLookup k m = case M.lookup k m of
  Just x -> x
  _      -> error $ "unsafeLookup failed for the key \"" <> show k <> "\""

-- The returned map is indexed by the grammar rule name, not the parser name.
functionsFromTests :: [InlineTest]
                   -> IO (M.Map String ParseFun, M.Map String LexFun)
functionsFromTests ts =
  let f InlineTest{..} (s1,s2)
        | expectedResult `elem` [Pass, Fail, Same] = (S.insert ruleName s1, s2)
        | expectedResult `elem` [TokPass, TokFail] = (s1, S.insert ruleName s2)
        | otherwise  = error "I do not know what to do for this test"
      (pNames, lNames) = foldr f (S.empty, S.empty) ts
  in I.runInterpreter (loadFunctions pNames lNames) >>= \case
        Left  err      -> error $ show err
        Right (pFuncs, lFuncs) -> return (pFuncs, lFuncs)

loadFunctions :: S.Set String -> S.Set String
              -> I.Interpreter (M.Map String ParseFun, M.Map String LexFun)
loadFunctions ps ls = do
  I.setImports ["Prelude", "Jael.Grammar"]
  (,) <$> mkFunctions ps mkParser <*> mkFunctions ls mkLexer

mkParser :: String -> I.Interpreter ParseFun
mkParser x = I.interpret (parserString x) (I.as :: ParseFun)

mkLexer :: String -> I.Interpreter LexFun
mkLexer x = I.interpret (lexerString x) (I.as :: LexFun)

mkFunctions :: S.Set String -> (String -> I.Interpreter a) -> I.Interpreter (M.Map String a)
mkFunctions xs f =
  let keys = S.toList xs
  in  M.fromList . zip keys <$> mapM f keys

parserString :: String -> String
parserString x = "(\\x -> case p" <> x <> " (tokens x) of\nOk x  -> Right (show x)\nBad e -> Left e)"

lexerString :: String -> String
lexerString x = "(\\x -> case tokens x of \n[PT _ (T_" <> x <> " _)] -> True\n_ -> False)"

lineToTest :: Int -> String -> Maybe InlineTest
lineToTest ln line = do
  ("--":x:y:z) <- Just $ words line
  parseRes <- case x of
    "pass"    -> Just Pass
    "fail"    -> Just Fail
    "same"    -> Just Same
    "tokpass" -> Just TokPass
    "tokfail" -> Just TokFail
    _         -> Nothing
  return InlineTest
    { expectedResult = parseRes
    , ruleName = y
    , testData = unwords z -- might change spacing but shouldn't change results
    , lineNo = ln
    }
