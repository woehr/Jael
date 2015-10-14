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
  new x : <SomeProto>;
  y = 5;
  +x -> z;
  -x <- y;
  +x <- true;
  -x select label;
  +x case
  { p1 => done
    , p2 => ( done
            | SomeProc(x)
            | SomeProc()
            | new a : dual <Proto2>;
              +z <- a;
              -z -> b;
              done
            )
    , p3 => rec X(j=+x, k=1)
              { -j <- k;
                ( X(j, k+1)
                | X()
                )
              }
    }
|], GProcNew (LIdent "x")
             (GSessDef GPolPos $ GSessVar (UIdent "SomeProto"))
  $ GProcLet (LIdent "y") (GEInt (DecInt "5"))
  $ GProcGet (GChanPos (GScopedIdent [GScopeElem (LIdent "x")])) (LIdent "z")
  $ GProcPut (GChanNeg (GScopedIdent [GScopeElem (LIdent "x")])) (GChanOrExprE $ GEVar (LIdent "y"))
  $ GProcPut (GChanPos (GScopedIdent [GScopeElem (LIdent "x")])) (GChanOrExprE $ GETrue)
  $ GProcSel (GChanNeg (GScopedIdent [GScopeElem (LIdent "x")])) (GChoiceLabel (LIdent "label"))
  $ GProcCho (GChanPos (GScopedIdent [GScopeElem (LIdent "x")]))
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
                       $ GProcNew (LIdent "a") (GSessDef GPolNeg $ GSessVar (UIdent "Proto2"))
                       $ GProcPut (GChanPos (GScopedIdent [GScopeElem (LIdent "z")])) (GChanOrExprE $ GEVar (LIdent "a"))
                       $ GProcGet (GChanNeg (GScopedIdent [GScopeElem (LIdent "z")])) (LIdent "b")
                       $ GProcInact
                     ]
      , GConcChoice (GChoiceLabel (LIdent "p3"))
          $ GProcRec (GProcName (UIdent "X"))
              [ GRecInitializer (LIdent "j") (GChanOrExprC $ GChanPos (GScopedIdent [GScopeElem (LIdent "x")]))
              , GRecInitializer (LIdent "k") (GChanOrExprE $ GEInt (DecInt "1"))
              ]
          $ GProcPut (GChanNeg (GScopedIdent [GScopeElem (LIdent "j")])) (GChanOrExprE $ GEVar (LIdent "k"))
          $ GProcPar (GParElem (GProcNamed (GProcName (UIdent "X"))
                         [ GProcParam (GChanOrExprE $ GEVar (LIdent "j"))
                         , GProcParam (GChanOrExprE $ GEPlus (GEVar (LIdent "k")) (GEInt (DecInt "1")))
                         ]
                     ))
                     [GParElem $ GProcNamed (GProcName (UIdent "X")) []
                     ]
      ]
  )

