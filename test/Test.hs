{-# Language NoImplicitPrelude #-}
import ClassyPrelude
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import Test.Jael.Grammar (grammarTests)
import Test.Jael.SeqTypeInf (seqInfTests)

main :: IO ()
main = defaultMainWithOpts
        (concat [ grammarTests
                , seqInfTests
                ]
        ) mempty

