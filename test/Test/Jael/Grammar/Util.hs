{-# Language NoImplicitPrelude #-}

module Test.Jael.Grammar.Util where

import ClassyPrelude
import Jael.Parser
import Test.HUnit

shouldNotParse :: ParseFun a -> Text -> Assertion
shouldNotParse p t = either (\_ -> return ()) (\_ -> assertFailure "Expression parsed successful") (runParser p t)

checkParsedTree :: (Eq a, Show a) => ParseFun a -> (Text, a) -> Assertion
checkParsedTree p (tx, tr) = either (assertFailure . unpack) ((@=?) tr) (runParser p tx)

