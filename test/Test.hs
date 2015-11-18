import Test.Jael.Compile (compileTests)
import Test.Jael.Grammar.Enum (gEnumTests)
import Test.Jael.Grammar.Expr (gExprTests)
import Test.Jael.Grammar.Proc (gProcTests)
import Test.Jael.Grammar.Struct (gStructTests)
import Test.Jael.Grammar.Type (gTypeTests)
import Test.Jael.CodeGen.Types (codeGenTypeTests)
import Test.Jael.Conc.Proc (procTests)
import Test.Jael.Conc.Session (sessionTests)
import Test.Jael.Conc.TyCk (concTyCkTests)
import Test.Jael.Hw.Area (hwAreaTests)
import Test.Jael.Seq.AST (astTests)
import Test.Jael.Seq.Builtin (builtinTests)
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
  -- seq types
  , testGroup "Enum creation" enumTests
  , testGroup "Struct creation" structTests
  -- hw
  , testGroup "Hardware areas" hwAreaTests
  -- sessions
  , testGroup "Session types" sessionTests
  -- processes
  , testGroup "Process grammar" gProcTests
  , testGroup "Process definition and type checking" procTests
  , testGroup "Process type checking" concTyCkTests
  -- everything else
  , testGroup "Compilation" compileTests
  , testGroup "Code gen (types)" codeGenTypeTests
  ] mempty

