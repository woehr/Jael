{-# Language NoImplicitPrelude, QuasiQuotes #-}

module Test.Jael.Seq.Closure
( closureTests
) where

import ClassyPrelude
import Jael.Grammar
import Jael.Parser
import Jael.Seq.Closure
import Jael.Seq.Env
import Jael.Seq.Expr
import Jael.Seq.TI
import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Jael.Util

closureTests :: [T.Test]
closureTests =
  [ testCase "escaping closure" $ checkExClosureConv escaping
  , testCase "non-escaping closure" $ checkExClosureConv nonEscaping
  ]

checkExClosureConv :: (Text, ExCC) -> Assertion
checkExClosureConv (tx, ex) = do
  either (assertFailure . unpack) (assertEqual "" ex . closureConversion) $ do
    g <- runParser pGExpr tx
    typedEx <- either (Left . intercalate "\n") Right (seqInferTypedEx defaultEnv $ gToEx g)
    return typedEx

escaping :: (Text, ExCC)
escaping = (pack [raw|
  \a -> {
    \b -> { a + b + 1 }
  }(1)
|], Asdf
  )

nonEscaping :: (Text, ExCC)
nonEscaping = (pack [raw|
  \a -> {
    \b -> { a + b + 1 }
    b(2)
  }(1)
|], Asdf
  )

