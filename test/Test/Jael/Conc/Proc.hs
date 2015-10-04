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
parseTopProc (GTopDefGProcDef (GProcDef _ as p)) = gToTopProc as p
parseTopProc _ = error "Expected only process definitions."

valid :: (Text, Proc)
valid = (pack [raw|
  new x : SomeProto;
  y = 5;
  x -> z;
  x <- y;
  x <- true;
  x select label;
  x case
    { p1 => {}
    , p2 => ( {}
            | SomeProc(x)
            | SomeProc()
            | new a : Proto2;
              ( z <- a; {}
              | z -> b; {}
              )
            )
    , p3 => rec X(j=x, k=1)
              { j <- k;
                ( X(j, k+1)
                | {}
                )
              }
    }
|], PNew "x" (PNTNamed "SomeProto")
  $ PNew "y" (PNTExpr (ELit (LInt 5)))
  $ PGet "x" "z"
  $ PPut "x" (EVar "y")
  $ PPut "x" (ELit (LBool True))
  $ PSel "x" "label"
  $ PCase "x"
      [ ("p1", PNil)
      , ("p2", PPar
                [ PNil
                , PNamed "SomeProc" [EVar "x"]
                , PNamed "SomeProc" []
                , PNew "a" (PNTNamed "Proto2")
                $ PPar
                    [ PPut "z" (EVar "a") PNil
                    , PGet "z" "b" PNil
                    ]
                ]
        )
      , ("p3", PCoRec "X" [ ("j", EVar "x")
                          , ("k", ELit (LInt 1))
                          ]
               ( PPut "j" (EVar "k")
               $ PPar [ PNamed "X" [ EVar "j"
                                   , EApp (EApp (EPrm PAdd) (EVar "k")) (ELit (LInt 1))
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
    y <- x;
    {}
  }
|], ProcDefErr
      { pErrFreeVars = S.fromList ["y"]
      , pErrDupArgs = S.empty
      , pErrNonExplicitCoRecVarCapture = M.empty
      }
  )

dupArgs :: (Text, ProcDefErr)
dupArgs = (pack [raw|
  proc X(x:Int, x:Bool) {
    {}
  }
|], ProcDefErr
      { pErrFreeVars = S.empty
      , pErrDupArgs = S.fromList ["x"]
      , pErrNonExplicitCoRecVarCapture = M.empty
      }
  )

coRecCapt :: (Text, ProcDefErr)
coRecCapt = (pack [raw|
  proc X(x:Int, y:Bool) {
    rec X(x=x) {
      y <- x;
      X(x)
    }
  }
|], ProcDefErr
      { pErrFreeVars = S.empty
      , pErrDupArgs = S.empty
      , pErrNonExplicitCoRecVarCapture = M.fromList [("X", S.fromList ["y"])]
      }
  )

