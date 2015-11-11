module Test.Jael.Examples
( exampleTests
) where

import qualified Test.Framework as T

exampleTests :: [T.Test]
exampleTests =
  [ testCase "am335x" $ runJael bbbExample
  ]

runJael :: Text -> Assertion
runJael program = undefined

bbbExample :: Text
bbbExample = pack [raw|

|]

