{-# Language NoImplicitPrelude #-}
import ClassyPrelude
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import Test.Jael.Grammar (grammarTests)

main :: IO ()
main = defaultMainWithOpts
        (concat [ grammarTests
                ]
        ) mempty

