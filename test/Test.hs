{-# Language NoImplicitPrelude #-}
import ClassyPrelude
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import Test.Jael.Grammar.Expr (gExprTests)
import Test.Jael.Grammar.Type (gTypeTests)
import Test.Jael.Seq.Struct (structTests)
import Test.Jael.Seq.TI (seqInfTests)

main :: IO ()
main = defaultMainWithOpts [ testGroup "Expression grammar" gExprTests
                           , testGroup "Type grammar" gTypeTests
                           , testGroup "Struct creation" structTests
                           , testGroup "Seq type inference" seqInfTests
                           ] mempty

