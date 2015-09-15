{-# Language NoImplicitPrelude, QuasiQuotes #-}

module Test.Jael.Grammar.Hwproc
( gHwprocTests
) where

import ClassyPrelude
import Jael.Grammar
import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Jael.Util

gHwprocTests :: [T.Test]
gHwprocTests =
  [ testCase "Parse valid hwproc" $ checkParsedTree pGHwproc hwprocTop
  ]

hwprocTop :: (Text, GHwproc)
hwprocTop = (pack [raw|
  hwproc SomeDev {
    interrupt 0x00100000;
    area SomeDevRegs @ 0x4000 {
      f1 :: Int
    }
    state X(Int, Bool);
    channel;
    channel;
    init = {
      letExpr = 2+4;
      // write 2 to x field of state
      // Note that 2 is an expression
      state <- 2;
      // read from field f1 of the memory area SomeDevRegs into a new variable y
      SomeDevRegs::f1::f2 -> y;
      // write the variable y into z field of state
      state::x::y::z <- y;
    }
    handler x = {
      state <- {};
    }
  }
|], GHwproc (UIdent "SomeDev")
            (HexInt "0x00100000")
            [ GHwArea
                (UIdent "SomeDevRegs")
                (HexInt "0x4000")
                (GTStructDef
                  []
                  [ GTStructElement
                      (GTStructFieldName (LIdent "f1"))
                      GTInt
                  ]
                )
            ]
            (GHwState
              (GHwStateDefTy
                (GTNamed
                  (UIdent "X")
                  (GTNamedParams
                    [ GTNamedParam GTInt
                    , GTNamedParam GTBool
                    ]
            ))))
            [ GHwChan
            , GHwChan
            ]
            (GHwInit
              [ GHwSeqLet (LIdent "letExpr")
                          (GEPlus (GEInt (IntTok "2")) (GEInt (IntTok "4")))
              , GHwSeqWrite
                  (GHwSeqLocState [])
                  (GHwValGExpr (GEInt (IntTok "2")))
              , GHwSeqRead
                  (GHwSeqLocArea
                    (UIdent "SomeDevRegs")
                    (map (GEScopeArgNE . GEScopeIdent . LIdent) ["f1", "f2"])
                  )
                  (LIdent "y")
              , GHwSeqWrite
                  (GHwSeqLocState (map (GEScopeIdent. LIdent) ["x", "y", "z"]))
                  (GHwValLIdent (LIdent "y"))
              ]
            )
            (GHwHandler
              (LIdent "x")
              [GHwSeqWrite (GHwSeqLocState []) (GHwValGExpr (GEUnit GUnit))]
            )
  )

