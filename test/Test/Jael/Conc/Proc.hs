module Test.Jael.Conc.Proc
( procTests
) where

import qualified Data.Map as M
import qualified Data.Set as S
import           Jael.Grammar
import           Jael.Parser
import           Jael.Conc.Proc
import           Jael.Conc.Session
import           Jael.Seq.CG_AST
import           Jael.Seq.Literal
import           Jael.Seq.Prm
import qualified Test.Framework as T

procTests :: [T.Test]
procTests =
  [ testCase "proc valid" $ checkProc valid
  , testCase "free vars" $ checkProcErr testProcFreeVars
  , testCase "dup args" $ checkProcErr dupArgs
  , testCase "co-rec capture" $ checkProcErr coRecCapt
  , testCase "ambiguious co-rec name" $ checkProcErr ambiguousRecName
  ]

checkProc :: (Text, Proc) -> Assertion
checkProc (t, expected) = either (assertFailure . show)
                                 (assertEqual "" expected . gToProc)
                                 (runParser pGProc t)

checkProcErr :: (Text, ProcDefErr) -> Assertion
checkProcErr (t, expected) = either (assertFailure . show)
                                    (assertEqual "" (Just expected)
                                      . validateTopProc
                                      . parseTopProc)
                                    (runParser pGTopDef t)

parseTopProc :: GTopDef -> TopProc
parseTopProc (GTopDefGProcDef (GProcDef _ as p)) = gToTopProc (as, p)
parseTopProc _ = error "Expected only process definitions."

valid :: (Text, Proc)
valid = (pack [raw|
  new (^xp, ^xn) : <SomeProto>;
  y = 5;
  ^xp -> z;
  ^xn <- y;
  ^xp <- true;
  ^xn select label;
  ^xp case
    { p1 =>
    , p2 => (
            | SomeProc(^xp)
            | SomeProc()
            | new (^a, ^b) : <dual Proto2>;
              ( ^z <- ^a;
              | ^z -> b;
              )
            )
    , p3 => rec X(j=x, k=1)
              { ^j <- k;
                ( X(^j, k+1)
                |
                )
              }
    }
|], PNewChan "xp" "xn" (SVar "SomeProto")
  $ PNewVal "y" (CGLit (LInt 5))
  $ PGetVal "xp" "z"
  $ PPutVal "xn" (CGVar "y")
  $ PPutVal "xp" (CGLit $ LBool True)
  $ PSel "xn" "label"
  $ PCase "xp"
      [ ("p1", PNil)
      , ("p2", PPar
                [ PNil
                , PNamed "SomeProc" [Left "xp"]
                , PNamed "SomeProc" []
                , PNewChan "a" "b" (SDualVar "Proto2")
                $ PPar
                    [ PPutChan "z" "a" PNil
                    , PGetVal "z" "b" PNil
                    ]
                ]
        )
      , ("p3", PCoRec "X" [ ("j", Right $ CGVar "x")
                          , ("k", Right $ CGLit (LInt 1))
                          ]
               ( PPutVal "j" (CGVar "k")
               $ PPar [ PNamed "X" [ Left "j"
                                   , Right $ CGCallPrm PAdd [CGVar "k", CGLit (LInt 1)]
                                   ]
                      , PNil
                      ]
               )
        )
      ]
  )

testProcFreeVars :: (Text, ProcDefErr)
testProcFreeVars = (pack [raw|
  proc X(x:Int) {
    ^a <- x;
    ( Y(b)
    | ^y case { p1 => c = d + e;
              , p2 => ^z select p3;
              }
    | ^f -> x;
    )
  }
|], ProcDefErr
      { pErrFreeVars = S.fromList ["a", "b", "d", "e", "f", "y", "z"]
      , pErrDupArgs = S.empty
      , pErrCoRecVarCapture = M.empty
      , pErrAmbiguousRecName = S.empty
      }
  )

dupArgs :: (Text, ProcDefErr)
dupArgs = (pack [raw|
  proc X(x:Int, x:Bool) {
    rec Y(y=void, y=void) {}
  }
|], ProcDefErr
      { pErrFreeVars = S.empty
      , pErrDupArgs = S.fromList ["x", "y"]
      , pErrCoRecVarCapture = M.empty
      , pErrAmbiguousRecName = S.empty
      }
  )

coRecCapt :: (Text, ProcDefErr)
coRecCapt = (pack [raw|
  proc X(x:Int, y:Bool, z:Foo) {
    rec Y(x=x, z=z) {
      ^y <- a + x; // a is undefined so it shouldn't show up in the capture error
      rec Z(x=x) {
        bar = x + y + z;
      }
    }
  }
|], ProcDefErr
      { pErrFreeVars = S.fromList ["a"]
      , pErrDupArgs = S.empty
      , pErrCoRecVarCapture = M.fromList
          [ ("Y", S.fromList ["y"])
          , ("Z", S.fromList ["y", "z"])
          ]
      , pErrAmbiguousRecName = S.empty
      }
  )

-- Y is ambiguous, Z is not. X is as well, but we need to consider all top level
-- processes to determine if a recursive name is hiding another
ambiguousRecName :: (Text, ProcDefErr)
ambiguousRecName = (pack [raw|
  proc X(x:Int, y:Bool, z:Foo) {
    rec X(x=x, z=z) {
      rec Y(x=x) {
        rec Y(x=x) {
          ( rec Z(x=x) {}
          | rec Z(x=x) {}
          )
        }
      }
    }
  }
|], ProcDefErr
      { pErrFreeVars = S.empty
      , pErrDupArgs = S.empty
      , pErrCoRecVarCapture = M.empty
      , pErrAmbiguousRecName = S.fromList ["Y"]
      }
  )

