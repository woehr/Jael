{-# Language NoImplicitPrelude #-}

import ClassyPrelude
import Test.Framework

import Test.Jael.Compile (compileTests)
import Test.Jael.Grammar.Enum (gEnumTests)
import Test.Jael.Grammar.Expr (gExprTests)
import Test.Jael.Grammar.Struct (gStructTests)
import Test.Jael.Grammar.Type (gTypeTests)
import Test.Jael.Seq.AST (astTests)
import Test.Jael.Seq.Builtin (builtinTests)
import Test.Jael.Seq.Closure (closureTests)
import Test.Jael.Seq.Enum (enumTests)
import Test.Jael.Seq.Struct (structTests)
import Test.Jael.Seq.TI (seqInfTests)

main :: IO ()
main = defaultMainWithOpts [ testGroup "Expr grammar" gExprTests
                           , testGroup "Type grammar" gTypeTests
                           , testGroup "Struct grammar" gStructTests
                           , testGroup "Enum grammar" gEnumTests
                           , testGroup "AST tests" astTests
                           , testGroup "Builtin tests" builtinTests
                           , testGroup "Seq type inference" seqInfTests
                           , testGroup "Closure conversion" closureTests
                           , testGroup "Enum creation" enumTests
                           , testGroup "Struct creation" structTests
                           , testGroup "Compilation" compileTests
                           ] mempty

