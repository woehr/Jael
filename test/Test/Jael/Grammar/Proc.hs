{-# Language NoImplicitPrelude, QuasiQuotes #-}

module Test.Jael.Grammar.Proc
( gProcTests
) where

import ClassyPrelude
import Jael.Grammar
import Jael.Parser
import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Jael.Util

gProcTests :: [T.Test]
gProcTests =
  [ testCase "proc grammar" $ checkParsedTree pGProc comprehensiveCase
  ]

comprehensiveCase:: (Text, GProc)
comprehensiveCase = (pack [raw|
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
              z <- a;
              z -> b;
              {}
            )
    , p3 => rec X(j=x, k=1)
              { j <- k;
                ( X(j, k+1)
                | X()
                )
              }
    }
|], GProcNew (GUserChan (LIdent "x"))
             (UIdent "SomeProto")
  $ GProcLet (LIdent "y") (GEInt (IntTok "5"))
  $ GProcGet (GChan (GScopedIdent [GScopeElem (LIdent "x")])) (LIdent "z")
  $ GProcPut (GChan (GScopedIdent [GScopeElem (LIdent "x")])) (GEVar (LIdent "y"))
  $ GProcPut (GChan (GScopedIdent [GScopeElem (LIdent "x")])) GETrue
  $ GProcSel (GChan (GScopedIdent [GScopeElem (LIdent "x")])) (GChoiceLabel (LIdent "label"))
  $ GProcCho (GChan (GScopedIdent [GScopeElem (LIdent "x")]))
      [ GConcChoice (GChoiceLabel (LIdent "p1"))
          $ GProcInact GUnit
      , GConcChoice (GChoiceLabel (LIdent "p2"))
          $ GProcPar (GParElem (GProcInact GUnit))
                     [ GParElem (GProcNamed (GProcName (UIdent "SomeProc"))
                                  [GProcParam (GEVar (LIdent "x"))]
                                )
                     , GParElem (GProcNamed (GProcName (UIdent "SomeProc"))
                                  []
                                )
                     , GParElem
                       $ GProcNew (GUserChan (LIdent "a")) (UIdent "Proto2")
                       $ GProcPut (GChan (GScopedIdent [GScopeElem (LIdent "z")])) (GEVar (LIdent "a"))
                       $ GProcGet (GChan (GScopedIdent [GScopeElem (LIdent "z")])) (LIdent "b")
                       $ GProcInact GUnit
                     ]
      , GConcChoice (GChoiceLabel (LIdent "p3"))
          $ GProcRec (GProcName (UIdent "X"))
              [ GRecInitializer (LIdent "j") (GEVar (LIdent "x"))
              , GRecInitializer (LIdent "k") (GEInt (IntTok "1"))
              ]
          $ GProcPut (GChan (GScopedIdent [GScopeElem (LIdent "j")])) (GEVar (LIdent "k"))
          $ GProcPar (GParElem (GProcNamed (GProcName (UIdent "X"))
                         [ GProcParam (GEVar (LIdent "j"))
                         , GProcParam (GEPlus (GEVar (LIdent "k")) (GEInt (IntTok "1")))
                         ]
                     ))
                     [GParElem $ GProcNamed (GProcName (UIdent "X")) []
                     ]
      ]
  )

