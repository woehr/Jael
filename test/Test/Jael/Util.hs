{-# Language NoImplicitPrelude #-}

module Test.Jael.Util
where

import ClassyPrelude
import qualified Data.Map as M
import Jael.Parser
import Jael.Seq.AST
import Jael.Seq.Types
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Test.HUnit

-- https://hackage.haskell.org/package/raw-strings-qq
raw :: QuasiQuoter
raw = QuasiQuoter {
    -- Extracted from dead-simple-json.
    quoteExp  = return . LitE . StringL,

    quotePat  = \_ -> fail "illegal raw string QuasiQuote \
                           \(allowed as expression only, used as a pattern)",
    quoteType = \_ -> fail "illegal raw string QuasiQuote \
                           \(allowed as expression only, used as a type)",
    quoteDec  = \_ -> fail "illegal raw string QuasiQuote \
                           \(allowed as expression only, used as a declaration)"
}

shouldNotParse :: ParseFun a -> Text -> Assertion
shouldNotParse p t = either (\_ -> return ()) (\_ -> assertFailure "Expression parsed successful") (runParser p t)

checkParsedTree :: (Eq a, Show a) => ParseFun a -> (Text, a) -> Assertion
checkParsedTree p (tx, tr) = either (assertFailure . unpack) (tr @=?) (runParser p tx)

-- For testing purposes two PolyTy are equal when their type variables and types
-- are equal. Consider, however, whether PolyTy ["a"] (TVar "a") should be equal
-- to PolyTy ["b"] (TVar "b")
instance Eq PolyTy where
  (PolyTy xs ts) == (PolyTy ys us) = xs == ys && ts == us

checkTDefErr :: ParseFun a -> (a -> Either TDefError b) -> (Text, TDefError) -> Assertion
checkTDefErr p validator (def, TDefError (DuplicateTyVars ets)
                                         (DuplicateFields efs)
                                         (FreeTyVars eftvs)
                                         (UnusedTyVars eutvs)) =
  case runParser p def of
       Left err -> assertFailure (show err)
       Right gDef ->
         case validator gDef of
              Left (TDefError (DuplicateTyVars ts)
                              (DuplicateFields fs)
                              (FreeTyVars ftvs)
                              (UnusedTyVars utvs))-> do
                   assertEqual "Duplicate type variables wrong" (sort ets) (sort ts)
                   assertEqual "Duplicate fields wrong" (sort efs) (sort fs)
                   assertEqual "Free type variables wrong" eftvs ftvs
                   assertEqual "Unused type variables wrong" eutvs utvs
              Right _ -> assertFailure "Expected struct error"

checkParsedTypes :: Show b
                 => ParseFun a
                 -> (a -> Either b [(Text, PolyTy)])
                 -> (Text, [(Text, PolyTy)])
                 -> Assertion
checkParsedTypes p validator (def, expected) =
  case runParser p def of
       Left err -> assertFailure (show err)
       Right gDef ->
         case validator gDef of
              Left sErr -> assertFailure (show sErr)
              Right tys -> assertEqual "" (M.fromList expected) (M.fromList tys)

