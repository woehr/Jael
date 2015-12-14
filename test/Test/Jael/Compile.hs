module Test.Jael.Compile
( compileTests
) where

import qualified Data.Map as M
import qualified Data.Set as S
import           Jael.Compile
import           Jael.Compile.Common
import           Jael.Seq.TI.S2 (S2TypeErr(..))
import qualified Test.Framework as T

compileTests :: [T.Test]
compileTests =
  [ testCase "duplicate def" $ assertCompErr dupDef
  , testCase "undefined var" $ assertCompErr undefinedVar
  , testCase "undefined proc " $ assertCompErr undefinedProc
  , testCase "call cycle (expr)" $ assertCompErr callCycleExpr
  , testCase "call cycle (proc)" $ assertCompErr callCycleProc
  , testCase "recursive type" $ assertCompErr recType
  , testCase "recursive session" $ assertCompErr recSession
  , testCase "recursive session w/o rec" $ assertCompErr recSessionNoCoRec
  , testCase "undefined type" $ assertCompErr undefType
  , testCase "undefined session" $ assertCompErr undefSession
  , testCase "ambiguous co-rec name" $ assertCompErr ambigName
  , testCase "function arity mismatch" $ assertCompErr fnArityMismatch
  ]

assertCompErr :: (Text, CompileErr) -> Assertion
assertCompErr (t, e) =
  either (assertEqual "" e)
         (const $ assertFailure "Expected compile error but result was success")
         (compile t)

dupDef :: (Text, CompileErr)
dupDef = (pack [raw|
  f=1;
  f=2;
|], DupDef ["f"]
  )

undefinedVar :: (Text, CompileErr)
undefinedVar = (pack [raw|
  f=g;
|], UndefName $ S.fromList ["g"]
  )

undefinedProc :: (Text, CompileErr)
undefinedProc = (pack [raw|
  proc X() { Z() }
|], UndefName $ S.fromList ["Z"]
  )

callCycleExpr :: (Text, CompileErr)
callCycleExpr = (pack [raw|
  f=g;
  g=f;
|], DepCycle ["f", "g"]
  )

callCycleProc :: (Text, CompileErr)
callCycleProc = (pack [raw|
  proc X() { Y() }
  proc Y() { X() }
|], DepCycle ["X", "Y"]
  )

recType :: (Text, CompileErr)
recType = (pack [raw|
  struct S {
    f1 : S
  }
|], DepCycle ["S"]
  )

recSession :: (Text, CompileErr)
recSession = (pack [raw|
  protocol X : ![Void] <Y>;
  protocol Y : ?[Void] <X>;
|], DepCycle ["X", "Y"]
  )

recSessionNoCoRec :: (Text, CompileErr)
recSessionNoCoRec = (pack [raw|
  protocol X : ![Void] <X>;
|], DepCycle ["X"]
  )

undefType :: (Text, CompileErr)
undefType = (pack [raw|
  enum E { f1 T }
|], UndefName $ S.fromList ["T"]
  )

undefSession :: (Text, CompileErr)
undefSession = (pack [raw|
  protocol X : <Y>;
|], UndefName $ S.fromList ["Y"]
  )

ambigName :: (Text, CompileErr)
ambigName = (pack [raw|
  proc X() {}

  proc Y(a:Int) {
    ( rec X(a=a) {}
    | rec Y(a=a) {}
    )
  }
|], AmbigName $ M.fromList [("Y", S.fromList ["X", "Y"])]
  )

fnArityMismatch :: (Text, CompileErr)
fnArityMismatch = (pack [raw|
  func x(a: Int) : Int {
    a
  }
  y = x(42, 43, 44);
|], TypeInfErr $ ArityMismatch $ M.fromList [("x", (1, 3))]
  )

