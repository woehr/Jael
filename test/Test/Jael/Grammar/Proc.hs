module Test.Jael.Grammar.Proc
( gProcTests
) where

import           Jael.Grammar
import qualified Test.Framework as T
import           Test.Jael.Util

gProcTests :: [T.Test]
gProcTests =
  [ testCase "proc grammar" $ checkParsedTree pGProc comprehensiveCase
  ]

comprehensiveCase:: (Text, GProc)
comprehensiveCase = (pack [raw|
  new (^xp, ^xn) : <SomeProto>;
  y = 5;
  ^xp -> z;
  ^xn <- y;
  ^xp <- true;
  ^xn select label;
  ^xp case
    { p1 =>
    , p2 => (
            | SomeProc(x)
            | SomeProc()
            | new (^a, ^b) : <dual Proto2>;
              ^z <- a;
              ^z -> ^b;
            )
    , p3 => rec X(^j=^x, k=1)
              { ^j <- k;
                ( X(j, k+1)
                |
                )
              }
    }
|], GProcNew (LIdent "xp") (LIdent "xn")
             (GSessVar (UIdent "SomeProto"))
  $ GProcLet (LIdent "y") (GEInt (DecInt "5"))
  $ GProcGetExpr (GChan (LIdent "xp")) (LIdent "z")
  $ GProcPutExpr (GChan (LIdent "xn")) (GEVar (LIdent "y"))
  $ GProcPutExpr (GChan (LIdent "xp")) (GETrue)
  $ GProcSel (GChan (LIdent "xn")) (GChoiceLabel (LIdent "label"))
  $ GProcCho (GChan (LIdent "xp"))
      [ GConcChoice (GChoiceLabel (LIdent "p1"))
          $ GProcInact
      , GConcChoice (GChoiceLabel (LIdent "p2"))
          $ GProcPar (GParElem GProcInact)
                     [ GParElem (GProcNamed (GProcName (UIdent "SomeProc"))
                                  [GProcParamExpr (GEVar (LIdent "x"))]
                                )
                     , GParElem (GProcNamed (GProcName (UIdent "SomeProc"))
                                  []
                                )
                     , GParElem
                       $ GProcNew (LIdent "a") (LIdent "b") (GSessVarDual (UIdent "Proto2"))
                       $ GProcPutExpr (GChan (LIdent "z")) (GEVar (LIdent "a"))
                       $ GProcGetChan (GChan (LIdent "z")) (LIdent "b")
                       $ GProcInact
                     ]
      , GConcChoice (GChoiceLabel (LIdent "p3"))
          $ GProcRec (GProcName (UIdent "X"))
              [ GRecInitializerChan (LIdent "j") (GChan (LIdent "x"))
              , GRecInitializerExpr (LIdent "k") (GEInt (DecInt "1"))
              ]
          $ GProcPutExpr (GChan (LIdent "j")) (GEVar (LIdent "k"))
          $ GProcPar (GParElem (GProcNamed (GProcName (UIdent "X"))
                         [ GProcParamExpr (GEVar (LIdent "j"))
                         , GProcParamExpr (GEPlus (GEVar (LIdent "k")) (GEInt (DecInt "1")))
                         ]
                     ))
                     [GParElem GProcInact]
      ]
  )

