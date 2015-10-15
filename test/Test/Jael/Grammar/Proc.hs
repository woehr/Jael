{-# Language NoImplicitPrelude #-}
{-# Language QuasiQuotes #-}

module Test.Jael.Grammar.Proc
( gProcTests
) where

import ClassyPrelude
import Jael.Grammar
import Test.Framework as T
import Test.Framework.Providers.HUnit
--import Test.HUnit
import Test.Jael.Util

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
    { p1 => done
    , p2 => ( done
            | SomeProc(x)
            | SomeProc()
            | new (^a, ^b) : <dual Proto2>;
              ^z <- a;
              ^z -> b;
              done
            )
    , p3 => rec X(j=^x, k=1)
              { ^j <- k;
                ( X(j, k+1)
                | done
                )
              }
    }
|], GProcNew (LIdent "xp") (LIdent "xn")
             (GSessVar (UIdent "SomeProto"))
  $ GProcLet (LIdent "y") (GEInt (DecInt "5"))
  $ GProcGet (GChan (GScopedIdent [GScopeElem (LIdent "xp")])) (LIdent "z")
  $ GProcPut (GChan (GScopedIdent [GScopeElem (LIdent "xn")])) (GChanOrExprE $ GEVar (LIdent "y"))
  $ GProcPut (GChan (GScopedIdent [GScopeElem (LIdent "xp")])) (GChanOrExprE $ GETrue)
  $ GProcSel (GChan (GScopedIdent [GScopeElem (LIdent "xn")])) (GChoiceLabel (LIdent "label"))
  $ GProcCho (GChan (GScopedIdent [GScopeElem (LIdent "xp")]))
      [ GConcChoice (GChoiceLabel (LIdent "p1"))
          $ GProcInact
      , GConcChoice (GChoiceLabel (LIdent "p2"))
          $ GProcPar (GParElem GProcInact)
                     [ GParElem (GProcNamed (GProcName (UIdent "SomeProc"))
                                  [GProcParam (GChanOrExprE $ GEVar (LIdent "x"))]
                                )
                     , GParElem (GProcNamed (GProcName (UIdent "SomeProc"))
                                  []
                                )
                     , GParElem
                       $ GProcNew (LIdent "a") (LIdent "b") (GSessVarDual (UIdent "Proto2"))
                       $ GProcPut (GChan (GScopedIdent [GScopeElem (LIdent "z")])) (GChanOrExprE $ GEVar (LIdent "a"))
                       $ GProcGet (GChan (GScopedIdent [GScopeElem (LIdent "z")])) (LIdent "b")
                       $ GProcInact
                     ]
      , GConcChoice (GChoiceLabel (LIdent "p3"))
          $ GProcRec (GProcName (UIdent "X"))
              [ GRecInitializer (LIdent "j") (GChanOrExprC $ GChan (GScopedIdent [GScopeElem (LIdent "x")]))
              , GRecInitializer (LIdent "k") (GChanOrExprE $ GEInt (DecInt "1"))
              ]
          $ GProcPut (GChan (GScopedIdent [GScopeElem (LIdent "j")])) (GChanOrExprE $ GEVar (LIdent "k"))
          $ GProcPar (GParElem (GProcNamed (GProcName (UIdent "X"))
                         [ GProcParam (GChanOrExprE $ GEVar (LIdent "j"))
                         , GProcParam (GChanOrExprE $ GEPlus (GEVar (LIdent "k")) (GEInt (DecInt "1")))
                         ]
                     ))
                     [GParElem GProcInact]
      ]
  )

