{-# Language NoImplicitPrelude, QuasiQuotes #-}

module Test.Jael.Conc.Proc
( procTests
) where

import ClassyPrelude
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
  [ testCase "proc validation" $ checkProc allErrs
  ]

checkProc :: (Text, Proc) -> Assertion
checkProc (t, expected) = either (assertFailure . show)
                                 (assertEqual "" expected . gToProc)
                                 (runParser pGProc t)

allErrs :: (Text, Proc)
allErrs = (pack [raw|
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
  $ PGet (Chan "x") "z"
  $ PPut (Chan "x") (EVar "y")
  $ PPut (Chan "x") (ELit (LBool True))
  $ PSel (Chan "x") "label"
  $ PCase (Chan "x")
      [ ("p1", PNil)
      , ("p2", PPar
                [ PNil
                , PNamed "SomeProc" [EVar "x"]
                , PNamed "SomeProc" []
                , PNew "a" (PNTNamed "Proto2")
                $ PPar
                    [ PPut (Chan "z") (EVar "a") PNil
                    , PGet (Chan "z") "b" PNil
                    ]
                ]
        )
      , ("p3", PCoRec "X" [ ("j", EVar "x")
                          , ("k", ELit (LInt 1))
                          ]
               ( PPut (Chan "j") (EVar "k")
               $ PPar [ PNamed "X" [ EVar "j"
                                   , EApp (EApp (EPrm PAdd) (EVar "k")) (ELit (LInt 1))
                                   ]
                      , PNil
                      ]
               )
        )
      ]
  )

