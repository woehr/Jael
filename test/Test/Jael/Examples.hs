{-# Language NoImplicitPrelude #-}
{-# Language QuasiQuotes #-}

module Test.Jael.Examples
( exampleTests
) where

import ClassyPrelude
import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Jael.Util

exampleTests :: [T.Test]
exampleTests =
  [ testCase "am335x" $ runJael bbbExample
  ]

runJael :: Text -> Assertion
runJael program = undefined

bbbExample :: Text
bbbExample = pack [raw|

|]

