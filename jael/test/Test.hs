import Test.Jael.Compile (compileTests)
import Test.Jael.CodeGen.Expr (codeGenExprTests)
import Test.Jael.CodeGen.Types (codeGenTypeTests)
import Test.Jael.Conc.Proc (procTests)
import Test.Jael.Conc.Session (sessionTests)
import Test.Jael.Conc.S2TyCk (procS2TyCkTests)
import Test.Jael.Conc.S3TyCk (procS3TyCkTests)
import Test.Jael.Properties (propTests)
import Test.Jael.Seq.AST (astTests)
import Test.Jael.Seq.Builtin (builtinTests)
import Test.Jael.Seq.Enum (enumTests)
import Test.Jael.Seq.Struct (structTests)
import Test.Jael.Seq.TI (seqInfTests)

main :: IO ()
main = defaultMain
  -- seq
  [ testGroup "AST tests" astTests
  , testGroup "Builtin tests" builtinTests
  , testGroup "Seq type inference" seqInfTests
  -- seq types
  , testGroup "Enum creation" enumTests
  , testGroup "Struct creation" structTests
  -- sessions
  , testGroup "Session types" sessionTests
  -- processes
  , testGroup "Process definition and type checking" procTests
  , testGroup "Process seq type checking" procS2TyCkTests
  , testGroup "Process type checking" procS3TyCkTests
  -- everything else
  , testGroup "Compilation" compileTests
  , testGroup "Code gen, types" codeGenTypeTests
  , testGroup "Code gen, expr ast gen" codeGenExprTests
  -- quickcheck property tests
  , testGroup "Property tests" propTests
  ]

