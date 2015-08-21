{-# Language NoImplicitPrelude #-}

import ClassyPrelude
import Test.Framework

import Test.Jael.Grammar.Enum (gEnumTests)
import Test.Jael.Grammar.Expr (gExprTests)
import Test.Jael.Grammar.Struct (gStructTests)
import Test.Jael.Grammar.Type (gTypeTests)
import Test.Jael.Seq.Enum (enumTests)
import Test.Jael.Seq.Struct (structTests)
import Test.Jael.Seq.TI (seqInfTests)

main :: IO ()
main = defaultMainWithOpts [ testGroup "Expression grammar" gExprTests
                           , testGroup "Type grammar" gTypeTests
                           , testGroup "Struct grammar" gStructTests
                           , testGroup "Enum grammar" gEnumTests
                           , testGroup "Struct creation" structTests
                           , testGroup "Enum creation" enumTests
                           , testGroup "Seq type inference" seqInfTests
                           ] mempty

