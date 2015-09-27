{-# Language NoImplicitPrelude #-}

import ClassyPrelude
import Test.Framework

import Test.Jael.Compile (compileTests)
import Test.Jael.Grammar.Enum (gEnumTests)
import Test.Jael.Grammar.Expr (gExprTests)
import Test.Jael.Grammar.Hwproc (gHwprocTests)
import Test.Jael.Grammar.Struct (gStructTests)
import Test.Jael.Grammar.Type (gTypeTests)
import Test.Jael.Conc.Session (sessionTests)
import Test.Jael.Hw.Area (hwAreaTests)
import Test.Jael.Hw.Seq (hwSeqTests)
import Test.Jael.Seq.AST (astTests)
import Test.Jael.Seq.Builtin (builtinTests)
import Test.Jael.Seq.Closure (closureTests)
import Test.Jael.Seq.Enum (enumTests)
import Test.Jael.Seq.Struct (structTests)
import Test.Jael.Seq.TI (seqInfTests)

main :: IO ()
main = defaultMainWithOpts
  -- seq grammar
  [ testGroup "Expr grammar" gExprTests
  , testGroup "Type grammar" gTypeTests
  , testGroup "Struct grammar" gStructTests
  , testGroup "Enum grammar" gEnumTests
  -- seq
  , testGroup "AST tests" astTests
  , testGroup "Builtin tests" builtinTests
  , testGroup "Seq type inference" seqInfTests
  , testGroup "Closure conversion" closureTests
  -- seq types
  , testGroup "Enum creation" enumTests
  , testGroup "Struct creation" structTests
  -- hw grammar
  , testGroup "Hwproc grammar" gHwprocTests
  -- hw
  , testGroup "Hwproc state/area" hwAreaTests
  , testGroup "Hwproc seq" hwSeqTests
  -- session types
  , testGroup "Session types" sessionTests
  -- everything else
  , testGroup "Compilation" compileTests
  ] mempty

