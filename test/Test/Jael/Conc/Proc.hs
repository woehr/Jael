{-# Language NoImplicitPrelude, QuasiQuotes #-}

module Test.Jael.Conc.Proc
( procTests
) where

import ClassyPrelude
import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Grammar
import Jael.Parser
import Jael.Conc.Proc
import Jael.Seq.AST
import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Jael.Util

procTests :: [T.Test]
procTests =
  [ testCase "proc valid" $ checkProc valid
  , testCase "free vars" $ checkProcErr freeVars
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
  new x : SomeProto;
  y = 5;
  +x -> z;
  -x <- y;
  +x <- true;
  -x select label;
  +x case
  { p1 => done
    , p2 => ( done
            | SomeProc(+x)
            | SomeProc()
            | new a : Proto2;
              ( -z <- a; done
              | +z -> b; done
              )
            )
    , p3 => rec X(j=x, k=1)
              { +j <- k;
                ( X(-j, k+1)
                | done
                )
              }
    }
|], PNew "x" (PNTNamed "SomeProto")
  $ PNew "y" (PNTExpr (ELit (LInt 5)))
  $ PGet ("x", Positive) "z"
  $ PPut ("x", Negative) (Right $ EVar "y")
  $ PPut ("x", Positive) (Right $ ELit (LBool True))
  $ PSel ("x", Negative) "label"
  $ PCase ("x", Positive)
      [ ("p1", PNil)
      , ("p2", PPar
                [ PNil
                , PNamed "SomeProc" [Left ("x", Positive)]
                , PNamed "SomeProc" []
                , PNew "a" (PNTNamed "Proto2")
                $ PPar
                    [ PPut ("z", Negative) (Right $ EVar "a") PNil
                    , PGet ("z", Positive) "b" PNil
                    ]
                ]
        )
      , ("p3", PCoRec "X" [ ("j", Right $ EVar "x")
                          , ("k", Right $ ELit (LInt 1))
                          ]
               ( PPut ("j", Positive) (Right $ EVar "k")
               $ PPar [ PNamed "X" [ Left ("j", Negative)
                                   , Right $ EApp (EApp (EPrm PAdd) (EVar "k")) (ELit (LInt 1))
                                   ]
                      , PNil
                      ]
               )
        )
      ]
  )

freeVars :: (Text, ProcDefErr)
freeVars = (pack [raw|
  proc X(x:Int) {
    +a <- x;
    ( Y(b)
    | -y case { p1 => c = d + e; done
              , p2 => +z select p3; done
              }
    | +f -> x; done
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
    rec Y(y={}, y={}) { done }
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
      +y <- a + x; // a is undefined so it shouldn't show up in the capture error
      rec Z(x=x) {
        bar = x + y + z; done
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
          ( rec Z(x=x) { done }
          | rec Z(x=x) { done }
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

