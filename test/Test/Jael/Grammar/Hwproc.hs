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
    init = {
      letExpr = 2+4;
      c::x::y::z <- y;
    }
  }
|], GHwproc (GProcName (UIdent "SomeDev"))
            (HexInt "0x00100000")
            (GProcSeq
              [ GProcLet (LIdent "letExpr")
                         (GEPlus (GEInt (IntTok "2")) (GEInt (IntTok "4")))
              , GProcPut (GChan (LIdentWithScope "c::x::y::z"))
                         (GEVar (LIdent "y"))
              ]
            )
  )

